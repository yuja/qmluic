use super::expr::{SimpleValue, StringKind};
use crate::opcode::BuiltinFunctionKind;
use crate::tir::{
    BasicBlockRef, BinaryBitwiseOp, BinaryOp, CodeBody, ConstantValue, Operand, Rvalue, Statement,
    Terminator,
};

#[derive(Clone, Debug, PartialEq)]
pub(super) enum EvaluatedValue {
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String, StringKind),
    StringList(Vec<(String, StringKind)>),
    EnumSet(Vec<String>),
    ObjectRef(String),
    ObjectRefList(Vec<String>),
    EmptyList,
}

impl EvaluatedValue {
    pub fn unwrap_into_simple_value(self) -> SimpleValue {
        match self {
            EvaluatedValue::Bool(v) => SimpleValue::Bool(v),
            EvaluatedValue::Integer(v) => SimpleValue::Number(v as f64),
            EvaluatedValue::Float(v) => SimpleValue::Number(v),
            EvaluatedValue::String(s, k) => SimpleValue::String(s, k),
            // enum can't be mapped to SimpleValue without type information
            _ => panic!("evaluated type must be simple value"),
        }
    }

    pub fn unwrap_string(self) -> (String, StringKind) {
        match self {
            EvaluatedValue::String(s, k) => (s, k),
            _ => panic!("evaluated value must be string"),
        }
    }

    pub fn unwrap_string_list(self) -> Vec<(String, StringKind)> {
        match self {
            EvaluatedValue::StringList(xs) => xs,
            EvaluatedValue::EmptyList => vec![],
            _ => panic!("evaluated value must be string list"),
        }
    }

    pub fn unwrap_enum_set(self) -> Vec<String> {
        match self {
            EvaluatedValue::EnumSet(es) => es,
            _ => panic!("evaluated value must be enum set"),
        }
    }

    pub fn unwrap_object_ref(self) -> String {
        match self {
            EvaluatedValue::ObjectRef(s) => s,
            _ => panic!("evaluated value must be object ref"),
        }
    }

    pub fn into_object_ref_list(self) -> Option<Vec<String>> {
        match self {
            EvaluatedValue::ObjectRefList(ss) => Some(ss),
            EvaluatedValue::EmptyList => Some(vec![]),
            _ => None,
        }
    }
}

/// Evaluates TIR code to constant value.
pub(super) fn evaluate_code(code: &CodeBody) -> Option<EvaluatedValue> {
    // fast path for simple constant expression
    if let Terminator::Return(a @ Operand::Constant(_)) = code.basic_blocks[0].terminator() {
        return to_evaluated_value(&[], a, StringKind::NoTr);
    }

    let mut visited_blocks = vec![false; code.basic_blocks.len()];
    let mut visit_block = |r: BasicBlockRef| {
        if visited_blocks[r.0] {
            None // prevent infinite loop
        } else {
            visited_blocks[r.0] = true;
            Some(&code.basic_blocks[r.0])
        }
    };

    let mut block = visit_block(BasicBlockRef(0))?;
    let mut locals: Vec<Option<EvaluatedValue>> = vec![None; code.locals.len()];
    loop {
        for stmt in &block.statements {
            match stmt {
                Statement::Assign(l, r) => {
                    locals[l.0] = match r {
                        Rvalue::Copy(a) => to_evaluated_value(&locals, a, StringKind::NoTr),
                        Rvalue::BinaryOp(BinaryOp::Bitwise(BinaryBitwiseOp::Or), l, r) => {
                            to_evaluated_enum_set(&locals, l, r)
                        }
                        Rvalue::CallBuiltinFunction(BuiltinFunctionKind::Tr, args) => {
                            to_evaluated_value(&locals, &args[0], StringKind::Tr)
                        }
                        Rvalue::MakeList(args) => to_evaluated_list(&locals, args),
                        // No need to support other operations since constants are evaluated
                        // by TIR builder.
                        _ => return None,
                    }
                }
                Statement::Exec(_) => {} // uninteresting as a constant expression
            }
        }

        match block.terminator() {
            Terminator::Br(r) => block = visit_block(*r)?,
            Terminator::BrCond(..) => return None, // unsupported
            Terminator::Return(a) => return to_evaluated_value(&locals, a, StringKind::NoTr),
            Terminator::Unreachable => unreachable!(),
        }
    }
}

fn to_evaluated_value(
    locals: &[Option<EvaluatedValue>],
    a: &Operand,
    k: StringKind,
) -> Option<EvaluatedValue> {
    match a {
        Operand::Constant(x) => match &x.value {
            ConstantValue::Bool(v) => Some(EvaluatedValue::Bool(*v)),
            ConstantValue::Integer(v) => Some(EvaluatedValue::Integer(*v)),
            ConstantValue::Float(v) => Some(EvaluatedValue::Float(*v)),
            ConstantValue::CString(v) => Some(EvaluatedValue::String(v.clone(), k)),
            ConstantValue::QString(v) => Some(EvaluatedValue::String(v.clone(), k)),
            ConstantValue::EmptyList => Some(EvaluatedValue::EmptyList),
        },
        Operand::EnumVariant(x) => Some(EvaluatedValue::EnumSet(vec![x.cxx_expression()])),
        Operand::Local(x) => locals[x.name.0].clone(),
        Operand::NamedObject(x) => Some(EvaluatedValue::ObjectRef(x.name.0.clone())),
        Operand::Void(_) => None,
    }
}

fn to_evaluated_list(
    locals: &[Option<EvaluatedValue>],
    args: &[Operand],
) -> Option<EvaluatedValue> {
    let mut item_iter = args
        .iter()
        .map(|a| to_evaluated_value(locals, a, StringKind::NoTr))
        .peekable();
    match item_iter.peek() {
        Some(Some(EvaluatedValue::String(..))) => {
            let ss = item_iter
                .map(|v| {
                    if let Some(EvaluatedValue::String(s, k)) = v {
                        Some((s, k))
                    } else {
                        None
                    }
                })
                .collect::<Option<Vec<_>>>()?;
            Some(EvaluatedValue::StringList(ss))
        }
        Some(Some(EvaluatedValue::ObjectRef(..))) => {
            let ss = item_iter
                .map(|v| {
                    if let Some(EvaluatedValue::ObjectRef(s)) = v {
                        Some(s)
                    } else {
                        None
                    }
                })
                .collect::<Option<Vec<_>>>()?;
            Some(EvaluatedValue::ObjectRefList(ss))
        }
        _ => None,
    }
}

fn to_evaluated_enum_set(
    locals: &[Option<EvaluatedValue>],
    left: &Operand,
    right: &Operand,
) -> Option<EvaluatedValue> {
    match (
        to_evaluated_value(locals, left, StringKind::NoTr)?,
        to_evaluated_value(locals, right, StringKind::NoTr)?,
    ) {
        (EvaluatedValue::EnumSet(mut ls), EvaluatedValue::EnumSet(rs)) => {
            ls.extend(rs);
            Some(EvaluatedValue::EnumSet(ls))
        }
        _ => None,
    }
}
