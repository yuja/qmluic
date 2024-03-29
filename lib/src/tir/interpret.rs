//! Minimal TIR interpreter designed for .ui generation pass.

use super::{BasicBlockRef, CodeBody, ConstantValue, Operand, Rvalue, Statement, Terminator};
use crate::opcode::{BinaryBitwiseOp, BinaryOp, BuiltinFunctionKind};
use crate::typemap::TypeSpace;

#[derive(Clone, Debug, PartialEq)]
pub enum EvaluatedValue {
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

/// Marker of bare or translatable string.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StringKind {
    /// Bare string.
    NoTr,
    /// String wrapped with `qsTr()`.
    Tr,
}

impl EvaluatedValue {
    pub fn unwrap_integer(self) -> i64 {
        match self {
            EvaluatedValue::Integer(d) => d,
            _ => panic!("evaluated value must be integer"),
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
pub fn evaluate_code(code: &CodeBody) -> Option<EvaluatedValue> {
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
                        Rvalue::CallMethod(Operand::NamedObject(x), meth, _)
                            if meth.object_class().name() == "QMenu"
                                && meth.name() == "menuAction"
                                && meth.arguments_len() == 0
                                && meth.return_type_name() == "QAction*" =>
                        {
                            // TODO: better check for applicability of static evaluation
                            // translate menu.menuAction() to <addaction name="menu"/>
                            Some(EvaluatedValue::ObjectRef(x.name.0.clone()))
                        }
                        Rvalue::MakeList(_, args) => to_evaluated_list(&locals, args),
                        // No need to support other operations since constants are evaluated
                        // by TIR builder.
                        _ => return None,
                    }
                }
                Statement::Exec(_) => {} // uninteresting as a constant expression
                Statement::ObserveProperty(..) => {}
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
            ConstantValue::NullPointer => None,
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
