use super::ceval;
use super::core::{
    BasicBlock, BasicBlockRef, CodeBody, Constant, ConstantValue, EnumVariant, Local, LocalRef,
    NamedObject, Operand, Rvalue, Statement, Terminator, Void,
};
use crate::diagnostic::Diagnostics;
use crate::opcode::{BinaryArithOp, BinaryOp, BuiltinFunctionKind, BuiltinMethodKind, UnaryOp};
use crate::qmlast::Node;
use crate::typedexpr::{
    self, DescribeType, ExpressionVisitor, RefSpace, TypeAnnotationSpace, TypeDesc,
};
use crate::typemap::{
    Class, Enum, MethodMatches, NamedType, PrimitiveType, Property, TypeKind, TypeMapError,
};
use crate::typeutil::{self, TypeError};
use itertools::Itertools as _;
use std::num::TryFromIntError;
use std::ops::Range;
use thiserror::Error;

/// Translates AST to type-checked IR.
#[derive(Clone, Debug)]
struct CodeBuilder<'a> {
    code: CodeBody<'a>,
}

impl<'a> CodeBuilder<'a> {
    fn new() -> Self {
        CodeBuilder {
            code: CodeBody::empty(),
        }
    }

    fn alloca(&mut self, ty: TypeKind<'a>, byte_range: Range<usize>) -> Result<Local<'a>, Void> {
        if ty != TypeKind::VOID {
            let a = Local::new(self.code.locals.len(), ty, byte_range);
            self.code.locals.push(a.clone());
            Ok(a)
        } else {
            Err(Void::new(byte_range))
        }
    }

    fn current_basic_block_ref(&self) -> BasicBlockRef {
        assert!(!self.code.basic_blocks.is_empty());
        BasicBlockRef(self.code.basic_blocks.len() - 1)
    }

    fn current_basic_block_mut(&mut self) -> &mut BasicBlock<'a> {
        self.code
            .basic_blocks
            .last_mut()
            .expect("at least one basic block must exist")
    }

    fn get_basic_block_mut(&mut self, r: BasicBlockRef) -> &mut BasicBlock<'a> {
        &mut self.code.basic_blocks[r.0]
    }

    fn set_completion_value(&mut self, value: Operand<'a>) {
        self.current_basic_block_mut().set_completion_value(value)
    }

    fn push_statement(&mut self, stmt: Statement<'a>) {
        self.current_basic_block_mut().push_statement(stmt);
    }

    fn emit_result(
        &mut self,
        ty: TypeKind<'a>,
        rv: Rvalue<'a>,
        byte_range: Range<usize>,
    ) -> Operand<'a> {
        match self.alloca(ty, byte_range) {
            Ok(a) => {
                self.push_statement(Statement::Assign(a.name, rv));
                Operand::Local(a)
            }
            Err(v) => {
                self.push_statement(Statement::Exec(rv));
                Operand::Void(v)
            }
        }
    }
}

impl<'a> ExpressionVisitor<'a> for CodeBuilder<'a> {
    type Item = Operand<'a>;
    type Local = LocalRef;
    type Label = BasicBlockRef;
    type Error = ExpressionError;

    fn make_void(&self, byte_range: Range<usize>) -> Self::Item {
        Operand::Void(Void::new(byte_range))
    }

    fn visit_integer(
        &mut self,
        value: u64,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let v = ConstantValue::Integer(value.try_into()?);
        Ok(Operand::Constant(Constant::new(v, byte_range)))
    }

    fn visit_float(
        &mut self,
        value: f64,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let v = ConstantValue::Float(value);
        Ok(Operand::Constant(Constant::new(v, byte_range)))
    }

    fn visit_string(
        &mut self,
        value: String,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let v = ConstantValue::CString(value);
        Ok(Operand::Constant(Constant::new(v, byte_range)))
    }

    fn visit_bool(
        &mut self,
        value: bool,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let v = ConstantValue::Bool(value);
        Ok(Operand::Constant(Constant::new(v, byte_range)))
    }

    fn visit_enum(
        &mut self,
        enum_ty: Enum<'a>,
        variant: &str,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        Ok(Operand::EnumVariant(EnumVariant::new(
            enum_ty, variant, byte_range,
        )))
    }

    fn visit_array(
        &mut self,
        elements: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let operands: Vec<_> = elements.into_iter().map(ensure_concrete_string).collect();
        if let Some(mut elem_t) = operands.first().map(|a| a.type_desc()) {
            for a in operands.iter().skip(1) {
                elem_t = deduce_type("array", elem_t, a.type_desc())?;
            }
            Ok(self.emit_result(
                to_concrete_list_type("array", elem_t)?,
                Rvalue::MakeList(operands),
                byte_range,
            ))
        } else {
            Ok(Operand::Constant(Constant::new(
                ConstantValue::EmptyList,
                byte_range,
            )))
        }
    }

    fn visit_local_ref(&mut self, name: Self::Local) -> Result<Self::Item, Self::Error> {
        let a = self.code.locals[name.0].clone(); // name must be valid
        Ok(Operand::Local(a))
    }

    fn visit_local_declaration(
        &mut self,
        value: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Local, Self::Error> {
        let value = ensure_concrete_string(value);
        let ty = to_concrete_type("=", value.type_desc())?;
        let a = self.alloca(ty, byte_range).map_err(|_| {
            ExpressionError::OperationOnUnsupportedType(
                "=".to_owned(),
                value.type_desc().qualified_name().into(),
            )
        })?;
        self.push_statement(Statement::Assign(a.name, Rvalue::Copy(value)));
        Ok(a.name)
    }

    fn visit_local_assignment(
        &mut self,
        name: Self::Local,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let ty = &self.code.locals[name.0].ty; // name must be valid
        let right = ensure_concrete_string(right);
        if !typeutil::is_assignable(ty, &right.type_desc())? {
            return Err(ExpressionError::OperationOnIncompatibleTypes(
                "=".to_owned(),
                ty.qualified_cxx_name().into(),
                right.type_desc().qualified_name().into(),
            ));
        }
        self.push_statement(Statement::Assign(name, Rvalue::Copy(right)));
        // TODO: or return rvalue?, but property assignment doesn't because it would have
        // to re-read property
        Ok(Operand::Void(Void::new(byte_range)))
    }

    fn visit_object_ref(
        &mut self,
        cls: Class<'a>,
        name: &str,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        Ok(Operand::NamedObject(NamedObject::new(
            name, cls, byte_range,
        )))
    }

    fn visit_object_property(
        &mut self,
        object: Self::Item,
        property: Property<'a>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        if !property.is_readable() {
            return Err(ExpressionError::UnreadableProperty);
        }
        Ok(self.emit_result(
            property.value_type()?,
            Rvalue::ReadProperty(object, property),
            byte_range,
        ))
    }

    fn visit_object_property_assignment(
        &mut self,
        object: Self::Item,
        property: Property<'a>,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        if !property.is_writable() {
            return Err(ExpressionError::UnwritableProperty);
        }
        let ty = property.value_type()?;
        let right = ensure_concrete_string(right);
        if !typeutil::is_assignable(&ty, &right.type_desc())? {
            return Err(ExpressionError::OperationOnIncompatibleTypes(
                "=".to_owned(),
                ty.qualified_cxx_name().into(),
                right.type_desc().qualified_name().into(),
            ));
        }
        // TODO: should we allow chained assignment?
        // TODO: do we want to break binding? maybe no, but document the behavior difference
        Ok(self.emit_result(
            TypeKind::VOID,
            Rvalue::WriteProperty(object, property, right),
            byte_range,
        ))
    }

    fn visit_object_method_call(
        &mut self,
        object: Self::Item,
        methods: MethodMatches<'a>,
        arguments: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let arguments: Vec<_> = arguments.into_iter().map(ensure_concrete_string).collect();
        let mut matched_index: Option<usize> = None;
        for (i, m) in methods.iter().enumerate() {
            if m.arguments_len() == arguments.len() {
                let compatible: Result<bool, TypeMapError> = m
                    .argument_types()
                    .zip(&arguments)
                    .try_fold(true, |acc, (ty, v)| {
                        Ok(acc && typeutil::is_assignable(&ty?, &v.type_desc()).unwrap_or(false))
                    });
                if compatible? {
                    matched_index = Some(i);
                    break;
                }
            }
        }

        if let Some(i) = matched_index {
            let m = methods.into_vec().swap_remove(i);
            Ok(self.emit_result(
                m.return_type()?,
                Rvalue::CallMethod(object, m, arguments),
                byte_range,
            ))
        } else {
            let expects = methods
                .iter()
                .map(|m| {
                    m.argument_types()
                        .map(|r| {
                            r.map(|t| t.qualified_cxx_name().into_owned())
                                .unwrap_or_else(|_| "?".to_owned())
                        })
                        .join(", ")
                })
                .join(") | (");
            let actual = arguments
                .iter()
                .map(|v| v.type_desc().qualified_name().into_owned())
                .join(", ");
            Err(ExpressionError::InvalidArgument(format!(
                "expects ({expects}), but got ({actual})"
            )))
        }
    }

    fn visit_object_builtin_method_call(
        &mut self,
        object: Self::Item,
        function: BuiltinMethodKind,
        arguments: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let object = ensure_concrete_string(object);
        let ty = match function {
            BuiltinMethodKind::Arg => {
                assert!(object.type_desc() == TypeDesc::STRING);
                if arguments.len() == 1 {
                    match arguments[0].type_desc() {
                        TypeDesc::ConstInteger
                        | TypeDesc::ConstString
                        | TypeDesc::Concrete(TypeKind::Just(NamedType::Primitive(
                            PrimitiveType::Bool
                            | PrimitiveType::Double
                            | PrimitiveType::Int
                            | PrimitiveType::QString
                            | PrimitiveType::Uint,
                        ))) => Ok(TypeKind::STRING),
                        t @ (TypeDesc::EmptyList
                        | TypeDesc::Concrete(TypeKind::Just(
                            NamedType::Class(_)
                            | NamedType::Enum(_)
                            | NamedType::Namespace(_)
                            | NamedType::Primitive(PrimitiveType::QStringList | PrimitiveType::Void)
                            | NamedType::QmlComponent(_),
                        ))
                        | TypeDesc::Concrete(
                            TypeKind::Pointer(_) | TypeKind::PointerList(_),
                        )) => Err(ExpressionError::InvalidArgument(t.qualified_name().into())),
                    }
                } else {
                    Err(ExpressionError::InvalidArgument(format!(
                        "expects 1 argument, but got {}",
                        arguments.len()
                    )))
                }
            }
        }?;
        Ok(self.emit_result(
            ty,
            Rvalue::CallBuiltinMethod(object, function, arguments),
            byte_range,
        ))
    }

    fn visit_builtin_call(
        &mut self,
        function: BuiltinFunctionKind,
        arguments: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let ty = match function {
            BuiltinFunctionKind::Tr => {
                if arguments.len() == 1 {
                    match arguments[0].type_desc() {
                        TypeDesc::ConstString => Ok(TypeKind::STRING),
                        t => Err(ExpressionError::InvalidArgument(t.qualified_name().into())),
                    }
                } else {
                    Err(ExpressionError::InvalidArgument(format!(
                        "expects 1 argument, but got {}",
                        arguments.len()
                    )))
                }
            }
        }?;
        Ok(self.emit_result(
            ty,
            Rvalue::CallBuiltinFunction(function, arguments),
            byte_range,
        ))
    }

    fn visit_unary_expression(
        &mut self,
        unary: UnaryOp,
        argument: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        match argument {
            Operand::Constant(a) => match unary {
                UnaryOp::Arith(op) => ceval::eval_unary_arith_expression(op, a.value),
                UnaryOp::Bitwise(op) => ceval::eval_unary_bitwise_expression(op, a.value),
                UnaryOp::Logical(op) => ceval::eval_unary_logical_expression(op, a.value),
            }
            .map(|v| Operand::Constant(Constant::new(v, byte_range))),
            argument => self.emit_unary_expression(unary, argument, byte_range),
        }
    }

    fn visit_binary_expression(
        &mut self,
        binary: BinaryOp,
        left: Self::Item,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        match (left, right) {
            (Operand::Constant(l), Operand::Constant(r)) => match binary {
                BinaryOp::Arith(op) => ceval::eval_binary_arith_expression(op, l.value, r.value),
                BinaryOp::Bitwise(op) => {
                    ceval::eval_binary_bitwise_expression(op, l.value, r.value)
                }
                BinaryOp::Shift(op) => ceval::eval_shift_expression(op, l.value, r.value),
                BinaryOp::Logical(op) => {
                    ceval::eval_binary_logical_expression(op, l.value, r.value)
                }
                BinaryOp::Comparison(op) => ceval::eval_comparison_expression(op, l.value, r.value),
            }
            .map(|v| Operand::Constant(Constant::new(v, byte_range))),
            (left, right) => self.emit_binary_expression(binary, left, right, byte_range),
        }
    }

    fn visit_ternary_expression(
        &mut self,
        (condition, condition_ref): (Self::Item, Self::Label),
        (consequence, consequence_ref): (Self::Item, Self::Label),
        (alternative, alternative_ref): (Self::Item, Self::Label),
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        if condition.type_desc() != TypeDesc::BOOL {
            return Err(ExpressionError::IncompatibleConditionType(
                condition.type_desc().qualified_name().into(),
            ));
        }
        let consequence = ensure_concrete_string(consequence);
        let alternative = ensure_concrete_string(alternative);
        let ty = deduce_concrete_type("ternary", consequence.type_desc(), alternative.type_desc())?;
        let sink = self.alloca(ty, byte_range);
        self.get_basic_block_mut(condition_ref)
            .finalize(Terminator::BrCond(
                condition,
                condition_ref.next(),   // consequence start
                consequence_ref.next(), // alternative start
            ));
        for (src, src_ref) in [
            (consequence, consequence_ref),
            (alternative, alternative_ref),
        ] {
            let block = self.get_basic_block_mut(src_ref);
            if let Ok(a) = &sink {
                block.push_statement(Statement::Assign(a.name, Rvalue::Copy(src)));
            }
            block.finalize(Terminator::Br(alternative_ref.next())); // end
        }
        Ok(sink.map(Operand::Local).unwrap_or_else(Operand::Void))
    }

    fn visit_expression_statement(&mut self, value: Self::Item) -> Result<(), Self::Error> {
        self.set_completion_value(ensure_concrete_string(value));
        Ok(())
    }

    fn visit_if_statement(
        &mut self,
        (condition, condition_ref): (Self::Item, Self::Label),
        consequence_ref: Self::Label,
        alternative_ref: Option<Self::Label>,
    ) -> Result<(), Self::Error> {
        if condition.type_desc() != TypeDesc::BOOL {
            return Err(ExpressionError::IncompatibleConditionType(
                condition.type_desc().qualified_name().into(),
            ));
        }
        self.get_basic_block_mut(condition_ref)
            .finalize(Terminator::BrCond(
                condition,
                condition_ref.next(),   // consequence start
                consequence_ref.next(), // alternative start or end
            ));
        let end_ref = alternative_ref.unwrap_or(consequence_ref).next();
        self.get_basic_block_mut(consequence_ref)
            .finalize(Terminator::Br(end_ref));
        if let Some(l) = alternative_ref {
            self.get_basic_block_mut(l)
                .finalize(Terminator::Br(end_ref));
        }
        Ok(())
    }

    fn visit_switch_statement(
        &mut self,
        cases: Vec<(Self::Item, Self::Label, Self::Label)>,
        default: Option<(usize, Self::Label)>,
        head_ref: Self::Label,
        exit_ref: Self::Label,
    ) -> Result<(), Self::Error> {
        // order of blocks: ...|exit|case0|body0|case1|body1|...|default body|
        let last_case_body_ref = cases.last().map(|&(_, _, b)| b).unwrap_or(exit_ref);
        let last_body_ref = default.map(|(_, b)| b).unwrap_or(last_case_body_ref);

        // connect fall-through paths: |body0|case1|body1|     |default body|
        //                                   +-----^         --^            +-->
        let mut fall_through_list: Vec<_> = cases.iter().map(|&(_, c, b)| (c, b)).collect();
        if let Some((pos, body_ref)) = default {
            fall_through_list.insert(pos, (last_case_body_ref, body_ref));
        }
        fall_through_list.push((last_body_ref, BasicBlockRef(usize::MAX)));
        for (&(_, body_ref), &(next_case_ref, _)) in fall_through_list.iter().tuple_windows() {
            self.get_basic_block_mut(body_ref)
                .finalize(Terminator::Br(next_case_ref.next())); // next body start
        }

        // connect case branches
        for (condition, condition_ref, body_ref) in cases {
            self.get_basic_block_mut(condition_ref)
                .finalize(Terminator::BrCond(
                    condition,
                    condition_ref.next(), // body start
                    body_ref.next(),      // next condition start, default body start, or end
                ));
        }

        // connect enter/exit paths
        self.get_basic_block_mut(head_ref)
            .finalize(Terminator::Br(exit_ref.next())); // first case/default start
        self.get_basic_block_mut(exit_ref)
            .finalize(Terminator::Br(last_body_ref.next())); // end
        Ok(())
    }

    fn visit_break_statement(&mut self, exit_ref: Self::Label) -> Result<(), Self::Error> {
        self.current_basic_block_mut()
            .finalize(Terminator::Br(exit_ref));
        self.code.basic_blocks.push(BasicBlock::empty()); // unreachable code may be inserted here
        Ok(())
    }

    fn visit_return_statement(&mut self, value: Self::Item) -> Result<(), Self::Error> {
        let value = ensure_concrete_string(value);
        self.current_basic_block_mut()
            .finalize(Terminator::Return(value));
        self.code.basic_blocks.push(BasicBlock::empty()); // unreachable code may be inserted here
        Ok(())
    }

    /// Inserts new basic block for the statements after the branch, returns the reference
    /// to the old (pre-branch) basic block.
    ///
    /// The returned basic block should be finalized by the subsequent `visit_*()` call.
    fn mark_branch_point(&mut self) -> Self::Label {
        let old_ref = self.current_basic_block_ref();
        self.code.basic_blocks.push(BasicBlock::empty());
        old_ref
    }
}

impl<'a> CodeBuilder<'a> {
    fn emit_unary_expression(
        &mut self,
        unary: UnaryOp,
        argument: Operand<'a>,
        byte_range: Range<usize>,
    ) -> Result<Operand<'a>, ExpressionError> {
        let unsupported = || ExpressionError::UnsupportedOperation(unary.to_string());
        let argument = ensure_concrete_string(argument);
        match unary {
            UnaryOp::Arith(_) => {
                let ty = to_concrete_type(unary, argument.type_desc())?;
                match &ty {
                    &TypeKind::INT | &TypeKind::UINT | &TypeKind::DOUBLE => Ok(ty),
                    _ => Err(unsupported()),
                }
            }
            UnaryOp::Bitwise(_) => {
                let ty = to_concrete_type(unary, argument.type_desc())?;
                match &ty {
                    &TypeKind::INT | &TypeKind::UINT | TypeKind::Just(NamedType::Enum(_)) => Ok(ty),
                    _ => Err(unsupported()),
                }
            }
            UnaryOp::Logical(_) => match argument.type_desc() {
                TypeDesc::BOOL => Ok(TypeKind::BOOL),
                _ => Err(unsupported()),
            },
        }
        .map(|ty| self.emit_result(ty, Rvalue::UnaryOp(unary, argument), byte_range))
    }

    fn emit_binary_expression(
        &mut self,
        binary: BinaryOp,
        left: Operand<'a>,
        right: Operand<'a>,
        byte_range: Range<usize>,
    ) -> Result<Operand<'a>, ExpressionError> {
        let unsupported = || ExpressionError::UnsupportedOperation(binary.to_string());
        let left = ensure_concrete_string(left);
        let right = ensure_concrete_string(right);
        match binary {
            BinaryOp::Arith(op) => {
                use BinaryArithOp::*;
                let ty = deduce_concrete_type(op, left.type_desc(), right.type_desc())?;
                match &ty {
                    &TypeKind::INT | &TypeKind::UINT | &TypeKind::DOUBLE => Ok(ty),
                    &TypeKind::STRING => match op {
                        Add => Ok(ty),
                        Sub | Mul | Div | Rem => Err(unsupported()),
                    },
                    _ => Err(unsupported()),
                }
            }
            BinaryOp::Bitwise(op) => {
                let ty = deduce_concrete_type(op, left.type_desc(), right.type_desc())?;
                match &ty {
                    &TypeKind::BOOL
                    | &TypeKind::INT
                    | &TypeKind::UINT
                    | TypeKind::Just(NamedType::Enum(_)) => Ok(ty),
                    _ => Err(unsupported()),
                }
            }
            BinaryOp::Shift(op) => {
                let lty = to_concrete_type(op, left.type_desc())?;
                match (&lty, right.type_desc()) {
                    (
                        &TypeKind::INT | &TypeKind::UINT,
                        TypeDesc::ConstInteger | TypeDesc::INT | TypeDesc::UINT,
                    ) => Ok(lty),
                    _ => Err(unsupported()),
                }
            }
            BinaryOp::Logical(_) => match (left.type_desc(), right.type_desc()) {
                (TypeDesc::BOOL, TypeDesc::BOOL) => Ok(TypeKind::BOOL),
                _ => Err(unsupported()),
            },
            BinaryOp::Comparison(op) => {
                match deduce_concrete_type(op, left.type_desc(), right.type_desc())? {
                    TypeKind::BOOL
                    | TypeKind::INT
                    | TypeKind::UINT
                    | TypeKind::DOUBLE
                    | TypeKind::STRING
                    | TypeKind::Just(NamedType::Enum(_)) => Ok(TypeKind::BOOL),
                    _ => Err(unsupported()),
                }
            }
        }
        .map(|ty| self.emit_result(ty, Rvalue::BinaryOp(binary, left, right), byte_range))
    }
}

#[derive(Clone, Debug, Error)]
pub(super) enum ExpressionError {
    #[error("integer conversion failed: {0}")]
    IntegerConversion(#[from] TryFromIntError),
    #[error("integer overflow")]
    IntegerOverflow,
    #[error("type resolution failed: {0}")]
    TypeResolution(#[from] TypeMapError),
    #[error("condition must be of bool type, but got: {0}")]
    IncompatibleConditionType(String),
    #[error("invalid argument: {0}")]
    InvalidArgument(String),
    #[error("operation '{0}' on incompatible types: {1} and {2}")]
    OperationOnIncompatibleTypes(String, String, String),
    #[error("operation '{0}' on undetermined type: {1}")]
    OperationOnUndeterminedType(String, String),
    #[error("operation '{0}' on unsupported type: {1}")]
    OperationOnUnsupportedType(String, String),
    #[error("unsupported operation '{0}'")]
    UnsupportedOperation(String),
    #[error("not a readable property")]
    UnreadableProperty,
    #[error("not a writable property")]
    UnwritableProperty,
}

fn to_operation_type_error(op_desc: impl ToString, err: TypeError) -> ExpressionError {
    match err {
        TypeError::TypeResolution(e) => ExpressionError::TypeResolution(e),
        TypeError::IncompatibleTypes(l, r) => {
            ExpressionError::OperationOnIncompatibleTypes(op_desc.to_string(), l, r)
        }
        TypeError::UndeterminedType(t) => {
            ExpressionError::OperationOnUndeterminedType(op_desc.to_string(), t)
        }
        TypeError::UnsupportedType(t) => {
            ExpressionError::OperationOnUnsupportedType(op_desc.to_string(), t)
        }
    }
}

fn deduce_concrete_type<'a>(
    op_desc: impl ToString,
    left: TypeDesc<'a>,
    right: TypeDesc<'a>,
) -> Result<TypeKind<'a>, ExpressionError> {
    typeutil::deduce_concrete_type(left, right).map_err(|e| to_operation_type_error(op_desc, e))
}

fn to_concrete_type(op_desc: impl ToString, t: TypeDesc) -> Result<TypeKind, ExpressionError> {
    typeutil::to_concrete_type(t).map_err(|e| to_operation_type_error(op_desc, e))
}

fn to_concrete_list_type(
    op_desc: impl ToString,
    elem_t: TypeDesc,
) -> Result<TypeKind, ExpressionError> {
    typeutil::to_concrete_list_type(elem_t).map_err(|e| to_operation_type_error(op_desc, e))
}

fn deduce_type<'a>(
    op_desc: impl ToString,
    left: TypeDesc<'a>,
    right: TypeDesc<'a>,
) -> Result<TypeDesc<'a>, ExpressionError> {
    typeutil::deduce_type(left, right).map_err(|e| to_operation_type_error(op_desc, e))
}

fn ensure_concrete_string(x: Operand) -> Operand {
    match x {
        Operand::Constant(Constant {
            value: ConstantValue::CString(s),
            byte_range,
        }) => Operand::Constant(Constant {
            value: ConstantValue::QString(s),
            byte_range,
        }),
        x => x,
    }
}

/// Translates expression AST nodes into type-checked IR.
pub fn build<'a, C>(
    ctx: &C,
    node: Node,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<CodeBody<'a>>
where
    C: RefSpace<'a> + TypeAnnotationSpace<'a>,
{
    let mut builder = CodeBuilder::new();
    typedexpr::walk(ctx, node, source, &mut builder, diagnostics)?;
    let current_ref = builder.current_basic_block_ref();
    builder
        .code
        .finalize_completion_values(current_ref, node.byte_range());
    Some(builder.code)
}

/// Translates callback AST nodes into type-checked IR.
///
/// If the top-level node is a function, a callback body having the specified function
/// parameters will be built. Otherwise this function is identical to `build()`.
pub fn build_callback<'a, C>(
    ctx: &C,
    node: Node,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<CodeBody<'a>>
where
    C: RefSpace<'a> + TypeAnnotationSpace<'a>,
{
    let mut builder = CodeBuilder::new();
    typedexpr::walk_callback(ctx, node, source, &mut builder, diagnostics)?;
    let current_ref = builder.current_basic_block_ref();
    builder
        .code
        .finalize_completion_values(current_ref, node.byte_range());
    Some(builder.code)
}

#[cfg(test)]
mod tests {
    use super::super::testenv::*;

    #[test]
    fn string_literal() {
        insta::assert_snapshot!(dump("'foo'"), @r###"
        .0:
            return "foo": QString
        "###);
    }

    #[test]
    fn enum_ref() {
        insta::assert_snapshot!(dump("Foo.Bar0"), @r###"
        .0:
            return 'Foo::Bar0': Foo::Bar
        "###);
    }

    #[test]
    fn empty_array_literal() {
        insta::assert_snapshot!(dump("[]"), @r###"
        .0:
            return {}: list
        "###);
    }

    #[test]
    fn string_array_literal() {
        insta::assert_snapshot!(dump("['foo', 'bar']"), @r###"
            %0: QStringList
        .0:
            %0 = make_list {"foo": QString, "bar": QString}
            return %0: QStringList
        "###);
    }

    #[test]
    fn object_array_literal() {
        insta::assert_snapshot!(dump("[foo, foo2]"), @r###"
            %0: QList<Foo*>
        .0:
            %0 = make_list {[foo]: Foo*, [foo2]: Foo*}
            return %0: QList<Foo*>
        "###);
    }

    #[test]
    fn dynamic_array_literal() {
        insta::assert_snapshot!(dump("[foo.text, foo2.text]"), @r###"
            %0: QString
            %1: QString
            %2: QStringList
        .0:
            %0 = read_property [foo]: Foo*, "text"
            %1 = read_property [foo2]: Foo*, "text"
            %2 = make_list {%0: QString, %1: QString}
            return %2: QStringList
        "###);
    }

    #[test]
    fn incompatible_array_literal() {
        let env = Env::new();
        assert!(env.try_build("[foo, 'bar']").is_err());
    }

    #[test]
    fn local_declaration_with_literal() {
        insta::assert_snapshot!(dump("{ let s = 'hello'; s }"), @r###"
            %0: QString
        .0:
            %0 = copy "hello": QString
            return %0: QString
        "###);
    }

    #[test]
    fn local_declaration_with_property() {
        insta::assert_snapshot!(dump("{ let s = foo.checked; s }"), @r###"
            %0: bool
            %1: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            %1 = copy %0: bool
            return %1: bool
        "###);
    }

    #[test]
    fn local_declaration_with_ternary() {
        insta::assert_snapshot!(dump("{ let s = foo.checked ? 1 : 2; s }"), @r###"
            %0: bool
            %1: int
            %2: int
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = copy 1: integer
            br .3
        .2:
            %1 = copy 2: integer
            br .3
        .3:
            %2 = copy %1: int
            return %2: int
        "###);
    }

    #[test]
    fn local_declaration_in_nested_block() {
        insta::assert_snapshot!(dump(r###"{
            let a = 'outer';
            {
                let a = a + 'inner';
                foo.text = a;
            }
            foo2.text = a;
        }"###), @r###"
            %0: QString
            %1: QString
            %2: QString
        .0:
            %0 = copy "outer": QString
            %1 = binary_op '+', %0: QString, "inner": QString
            %2 = copy %1: QString
            write_property [foo]: Foo*, "text", %2: QString
            write_property [foo2]: Foo*, "text", %0: QString
            return _: void
        "###);
    }

    #[test]
    fn local_declaration_with_void() {
        let env = Env::new();
        assert!(env.try_build("{ let a = foo.done() }").is_err());
    }

    #[test]
    fn local_assignment_branched() {
        insta::assert_snapshot!(dump(r###"{
            let a = '';
            if (foo.checked) {
                a = foo.text;
            } else if (foo2.checked) {
                a = foo2.text;
            }
            foo3.text = a;
        }"###), @r###"
            %0: QString
            %1: bool
            %2: QString
            %3: bool
            %4: QString
        .0:
            %0 = copy "": QString
            %1 = read_property [foo]: Foo*, "checked"
            br_cond %1: bool, .1, .2
        .1:
            %2 = read_property [foo]: Foo*, "text"
            %0 = copy %2: QString
            br .5
        .2:
            %3 = read_property [foo2]: Foo*, "checked"
            br_cond %3: bool, .3, .4
        .3:
            %4 = read_property [foo2]: Foo*, "text"
            %0 = copy %4: QString
            br .4
        .4:
            br .5
        .5:
            write_property [foo3]: Foo*, "text", %0: QString
            return _: void
        "###);
    }

    #[test]
    fn local_assignment_type_mismatch() {
        let env = Env::new();
        assert!(env.try_build("{ let a = true; a = foo.text }").is_err());
    }

    #[test]
    fn const_declaration() {
        insta::assert_snapshot!(dump("{ const s = 'hello'; s }"), @r###"
            %0: QString
        .0:
            %0 = copy "hello": QString
            return %0: QString
        "###);
    }

    #[test]
    fn const_reassignment() {
        let env = Env::new();
        assert!(env.try_build("{ const a = 0; a = 1 }").is_err());
    }

    #[test]
    fn local_declaration_with_object_then_read_property() {
        insta::assert_snapshot!(dump("{ let o = foo; o.checked }"), @r###"
            %0: Foo*
            %1: bool
        .0:
            %0 = copy [foo]: Foo*
            %1 = read_property %0: Foo*, "checked"
            return %1: bool
        "###);
    }

    #[test]
    fn local_declaration_with_object_then_write_property() {
        insta::assert_snapshot!(dump("{ let o = foo; o.checked = true }"), @r###"
            %0: Foo*
        .0:
            %0 = copy [foo]: Foo*
            write_property %0: Foo*, "checked", true: bool
            return _: void
        "###);
    }

    #[test]
    fn named_object_ref() {
        insta::assert_snapshot!(dump("foo"), @r###"
        .0:
            return [foo]: Foo*
        "###);
    }

    #[test]
    fn read_object_property() {
        insta::assert_snapshot!(dump("foo.checked"), @r###"
            %0: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            return %0: bool
        "###);
    }

    #[test]
    fn write_object_property() {
        insta::assert_snapshot!(dump("foo.checked = true"), @r###"
        .0:
            write_property [foo]: Foo*, "checked", true: bool
            return _: void
        "###);
    }

    #[test]
    fn call_object_method() {
        insta::assert_snapshot!(dump("foo.done(1)"), @r###"
        .0:
            call_method [foo]: Foo*, "done", {1: integer}
            return _: void
        "###);
    }

    #[test]
    fn call_string_arg_method() {
        insta::assert_snapshot!(dump("'Hello %1'.arg('world')"), @r###"
            %0: QString
        .0:
            %0 = call_builtin_method "Hello %1": QString, Arg, {"world": string}
            return %0: QString
        "###);
    }

    #[test]
    fn call_tr_function() {
        insta::assert_snapshot!(dump("qsTr('Hello')"), @r###"
            %0: QString
        .0:
            %0 = call_builtin_function Tr, {"Hello": string}
            return %0: QString
        "###);
    }

    #[test]
    fn call_tr_function_on_dynamic_string() {
        let env = Env::new();
        assert!(env.try_build("qsTr(foo.text)").is_err());
    }

    #[test]
    fn constant_number_arithmetic() {
        insta::assert_snapshot!(dump("(-1 + 2 * 3) / +4"), @r###"
        .0:
            return 1: integer
        "###);
        insta::assert_snapshot!(dump("(-1. + 2. * 3.) / +4."), @r###"
        .0:
            return 1.25: double
        "###);
    }

    #[test]
    fn constant_string_concatenation() {
        insta::assert_snapshot!(dump("'foo' + 'bar'"), @r###"
        .0:
            return "foobar": QString
        "###);
    }

    #[test]
    fn constant_bool_bitwise() {
        insta::assert_snapshot!(dump("false ^ true | false"), @r###"
        .0:
            return true: bool
        "###);
    }

    #[test]
    fn dynamic_bool_bitwise() {
        insta::assert_snapshot!(dump("foo.checked ^ foo2.checked | foo3.checked"), @r###"
            %0: bool
            %1: bool
            %2: bool
            %3: bool
            %4: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            %1 = read_property [foo2]: Foo*, "checked"
            %2 = binary_op '^', %0: bool, %1: bool
            %3 = read_property [foo3]: Foo*, "checked"
            %4 = binary_op '|', %2: bool, %3: bool
            return %4: bool
        "###);
    }

    #[test]
    fn dynamic_integer_arithmetic() {
        insta::assert_snapshot!(dump("-foo.currentIndex + 1"), @r###"
            %0: int
            %1: int
            %2: int
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            %1 = unary_op '-', %0: int
            %2 = binary_op '+', %1: int, 1: integer
            return %2: int
        "###);
    }

    #[test]
    fn dynamic_string_concatenation() {
        insta::assert_snapshot!(dump("'Hello ' + foo.text"), @r###"
            %0: QString
            %1: QString
        .0:
            %0 = read_property [foo]: Foo*, "text"
            %1 = binary_op '+', "Hello ": QString, %0: QString
            return %1: QString
        "###);
    }

    #[test]
    fn constant_integer_bitwise() {
        insta::assert_snapshot!(dump("((1 ^ 3) | 4) & ~0"), @r###"
        .0:
            return 6: integer
        "###);
    }

    #[test]
    fn constant_enum_bitwise() {
        insta::assert_snapshot!(dump("(Foo.Bar0 ^ Foo.Bar1 | Foo.Bar2) & ~Foo.Bar3"), @r###"
            %0: Foo::Bar
            %1: Foo::Bar
            %2: Foo::Bar
            %3: Foo::Bar
        .0:
            %0 = binary_op '^', 'Foo::Bar0': Foo::Bar, 'Foo::Bar1': Foo::Bar
            %1 = binary_op '|', %0: Foo::Bar, 'Foo::Bar2': Foo::Bar
            %2 = unary_op '~', 'Foo::Bar3': Foo::Bar
            %3 = binary_op '&', %1: Foo::Bar, %2: Foo::Bar
            return %3: Foo::Bar
        "###);
    }

    #[test]
    fn dynamic_integer_bitwise() {
        insta::assert_snapshot!(dump("~foo.currentIndex & foo2.currentIndex"), @r###"
            %0: int
            %1: int
            %2: int
            %3: int
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            %1 = unary_op '~', %0: int
            %2 = read_property [foo2]: Foo*, "currentIndex"
            %3 = binary_op '&', %1: int, %2: int
            return %3: int
        "###);
    }

    #[test]
    fn constant_bool_logical() {
        insta::assert_snapshot!(dump("!true && true || false"), @r###"
        .0:
            return false: bool
        "###);
    }

    #[test]
    fn dynamic_bool_logical() {
        insta::assert_snapshot!(dump("!foo.checked && foo2.checked || foo3.checked"), @r###"
            %0: bool
            %1: bool
            %2: bool
            %3: bool
            %4: bool
            %5: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            %1 = unary_op '!', %0: bool
            %2 = read_property [foo2]: Foo*, "checked"
            %3 = binary_op '&&', %1: bool, %2: bool
            %4 = read_property [foo3]: Foo*, "checked"
            %5 = binary_op '||', %3: bool, %4: bool
            return %5: bool
        "###);
    }

    #[test]
    fn const_literal_comparison() {
        insta::assert_snapshot!(dump("1 == 1"), @r###"
        .0:
            return true: bool
        "###);
        insta::assert_snapshot!(dump("1 > 2"), @r###"
        .0:
            return false: bool
        "###);
        insta::assert_snapshot!(dump("'bar' <= 'baz'"), @r###"
        .0:
            return true: bool
        "###);
    }

    #[test]
    fn const_enum_comparison() {
        insta::assert_snapshot!(dump("Foo.Bar1 == Foo.Bar2"), @r###"
            %0: bool
        .0:
            %0 = binary_op '==', 'Foo::Bar1': Foo::Bar, 'Foo::Bar2': Foo::Bar
            return %0: bool
        "###);
    }

    #[test]
    fn dynamic_integer_comparison() {
        insta::assert_snapshot!(dump("foo.currentIndex > 0"), @r###"
            %0: int
            %1: bool
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            %1 = binary_op '>', %0: int, 0: integer
            return %1: bool
        "###);
    }

    #[test]
    fn dynamic_string_comparison() {
        insta::assert_snapshot!(dump("'yoda' != foo.text"), @r###"
            %0: QString
            %1: bool
        .0:
            %0 = read_property [foo]: Foo*, "text"
            %1 = binary_op '!=', "yoda": QString, %0: QString
            return %1: bool
        "###);
    }

    #[test]
    fn ternary_simple() {
        insta::assert_snapshot!(dump("foo.checked ? 1 : 2"), @r###"
            %0: bool
            %1: int
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = copy 1: integer
            br .3
        .2:
            %1 = copy 2: integer
            br .3
        .3:
            return %1: int
        "###);
    }

    #[test]
    fn ternary_string_literal() {
        insta::assert_snapshot!(dump("foo.checked ? 'yes' : 'no'"), @r###"
            %0: bool
            %1: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = copy "yes": QString
            br .3
        .2:
            %1 = copy "no": QString
            br .3
        .3:
            return %1: QString
        "###);
    }

    #[test]
    fn ternary_dynamic_result() {
        insta::assert_snapshot!(dump("foo.checked ? foo.text : foo2.text"), @r###"
            %0: bool
            %1: QString
            %2: QString
            %3: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = read_property [foo]: Foo*, "text"
            %3 = copy %1: QString
            br .3
        .2:
            %2 = read_property [foo2]: Foo*, "text"
            %3 = copy %2: QString
            br .3
        .3:
            return %3: QString
        "###);
    }

    #[test]
    fn ternary_nested_condition() {
        insta::assert_snapshot!(
            dump("(foo.checked ? foo2.checked : foo3.checked) ? foo2.text : foo3.text"), @r###"
            %0: bool
            %1: bool
            %2: bool
            %3: bool
            %4: QString
            %5: QString
            %6: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = read_property [foo2]: Foo*, "checked"
            %3 = copy %1: bool
            br .3
        .2:
            %2 = read_property [foo3]: Foo*, "checked"
            %3 = copy %2: bool
            br .3
        .3:
            br_cond %3: bool, .4, .5
        .4:
            %4 = read_property [foo2]: Foo*, "text"
            %6 = copy %4: QString
            br .6
        .5:
            %5 = read_property [foo3]: Foo*, "text"
            %6 = copy %5: QString
            br .6
        .6:
            return %6: QString
        "###);
    }

    #[test]
    fn ternary_nested_consequence() {
        insta::assert_snapshot!(
            dump("foo.checked ? (foo2.checked ? foo.text : foo2.text) : foo3.text"), @r###"
            %0: bool
            %1: bool
            %2: QString
            %3: QString
            %4: QString
            %5: QString
            %6: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .5
        .1:
            %1 = read_property [foo2]: Foo*, "checked"
            br_cond %1: bool, .2, .3
        .2:
            %2 = read_property [foo]: Foo*, "text"
            %4 = copy %2: QString
            br .4
        .3:
            %3 = read_property [foo2]: Foo*, "text"
            %4 = copy %3: QString
            br .4
        .4:
            %6 = copy %4: QString
            br .6
        .5:
            %5 = read_property [foo3]: Foo*, "text"
            %6 = copy %5: QString
            br .6
        .6:
            return %6: QString
        "###);
    }

    #[test]
    fn ternary_nested_alternative() {
        insta::assert_snapshot!(
            dump("foo.checked ? foo.text : (foo2.checked ? foo2.text : foo3.text)"), @r###"
            %0: bool
            %1: QString
            %2: bool
            %3: QString
            %4: QString
            %5: QString
            %6: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = read_property [foo]: Foo*, "text"
            %6 = copy %1: QString
            br .6
        .2:
            %2 = read_property [foo2]: Foo*, "checked"
            br_cond %2: bool, .3, .4
        .3:
            %3 = read_property [foo2]: Foo*, "text"
            %5 = copy %3: QString
            br .5
        .4:
            %4 = read_property [foo3]: Foo*, "text"
            %5 = copy %4: QString
            br .5
        .5:
            %6 = copy %5: QString
            br .6
        .6:
            return %6: QString
        "###);
    }

    #[test]
    fn ternary_concatenation() {
        insta::assert_snapshot!(
            dump("(foo.checked ? foo.text : foo2.text) + (foo3.checked ? foo3.text : foo4.text)"), @r###"
            %0: bool
            %1: QString
            %2: QString
            %3: QString
            %4: bool
            %5: QString
            %6: QString
            %7: QString
            %8: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = read_property [foo]: Foo*, "text"
            %3 = copy %1: QString
            br .3
        .2:
            %2 = read_property [foo2]: Foo*, "text"
            %3 = copy %2: QString
            br .3
        .3:
            %4 = read_property [foo3]: Foo*, "checked"
            br_cond %4: bool, .4, .5
        .4:
            %5 = read_property [foo3]: Foo*, "text"
            %7 = copy %5: QString
            br .6
        .5:
            %6 = read_property [foo4]: Foo*, "text"
            %7 = copy %6: QString
            br .6
        .6:
            %8 = binary_op '+', %3: QString, %7: QString
            return %8: QString
        "###);
    }

    #[test]
    fn multiple_statements() {
        insta::assert_snapshot!(
            dump("{ qsTr('hello'); foo.done(0); 'discarded'; foo.done(1); 'world' }"), @r###"
            %0: QString
        .0:
            %0 = call_builtin_function Tr, {"hello": string}
            call_method [foo]: Foo*, "done", {0: integer}
            call_method [foo]: Foo*, "done", {1: integer}
            return "world": QString
        "###);
    }

    #[test]
    fn empty_statement() {
        insta::assert_snapshot!(
            dump(";"), @r###"
        .0:
            return _: void
        "###);
    }

    #[test]
    fn if_statement_literal() {
        insta::assert_snapshot!(dump("if (foo.checked) { 'yes' }"), @r###"
            %0: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            return "yes": QString
        .2:
            return _: void
        "###);
    }

    #[test]
    fn if_else_statement_literal() {
        insta::assert_snapshot!(dump("if (foo.checked) { 'yes' } else { 'no' }"), @r###"
            %0: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            return "yes": QString
        .2:
            return "no": QString
        .3:
            unreachable
        "###);
    }

    #[test]
    fn if_else_statement_literal_incompatible_completion() {
        insta::assert_snapshot!(dump("if (foo.checked) { 'yes' } else { false }"), @r###"
            %0: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            return "yes": QString
        .2:
            return false: bool
        .3:
            unreachable
        "###);
    }

    #[test]
    fn if_statement_call() {
        insta::assert_snapshot!(dump("if (foo.checked) { foo.done(0) }"), @r###"
            %0: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            call_method [foo]: Foo*, "done", {0: integer}
            return _: void
        .2:
            return _: void
        "###);
    }

    #[test]
    fn if_else_statement_call() {
        insta::assert_snapshot!(
            dump("if (foo.checked) { foo.done(0); 'yes' } else { foo.done(1); 'no' }"), @r###"
            %0: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            call_method [foo]: Foo*, "done", {0: integer}
            return "yes": QString
        .2:
            call_method [foo]: Foo*, "done", {1: integer}
            return "no": QString
        .3:
            unreachable
        "###);
    }

    #[test]
    fn if_else_statement_call_incompatible_completion() {
        insta::assert_snapshot!(
            dump("if (foo.checked) { foo.done(0); 'yes' } else { foo.done(1); false }"), @r###"
            %0: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            call_method [foo]: Foo*, "done", {0: integer}
            return "yes": QString
        .2:
            call_method [foo]: Foo*, "done", {1: integer}
            return false: bool
        .3:
            unreachable
        "###);
    }

    #[test]
    fn if_else_statement_chained() {
        insta::assert_snapshot!(
            dump(r###"
            if (foo.checked) {
                foo.text
            } else if (foo2.checked) {
                foo2.text
            } else {
                foo3.text
            }
            "###), @r###"
            %0: bool
            %1: QString
            %2: bool
            %3: QString
            %4: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = read_property [foo]: Foo*, "text"
            return %1: QString
        .2:
            %2 = read_property [foo2]: Foo*, "checked"
            br_cond %2: bool, .3, .4
        .3:
            %3 = read_property [foo2]: Foo*, "text"
            return %3: QString
        .4:
            %4 = read_property [foo3]: Foo*, "text"
            return %4: QString
        .5:
            unreachable
        .6:
            unreachable
        "###);
    }

    #[test]
    fn if_statement_ternary_in_condition() {
        insta::assert_snapshot!(
            dump("if (foo.checked ? foo2.checked : foo3.checked) { foo.done(0) }"), @r###"
            %0: bool
            %1: bool
            %2: bool
            %3: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = read_property [foo2]: Foo*, "checked"
            %3 = copy %1: bool
            br .3
        .2:
            %2 = read_property [foo3]: Foo*, "checked"
            %3 = copy %2: bool
            br .3
        .3:
            br_cond %3: bool, .4, .5
        .4:
            call_method [foo]: Foo*, "done", {0: integer}
            return _: void
        .5:
            return _: void
        "###);
    }

    #[test]
    fn if_else_statement_ternary_in_condition() {
        insta::assert_snapshot!(
            dump("if (foo.checked ? foo2.checked : foo3.checked) { foo.done(0) } else { foo.done(1) }"),
            @r###"
            %0: bool
            %1: bool
            %2: bool
            %3: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = read_property [foo2]: Foo*, "checked"
            %3 = copy %1: bool
            br .3
        .2:
            %2 = read_property [foo3]: Foo*, "checked"
            %3 = copy %2: bool
            br .3
        .3:
            br_cond %3: bool, .4, .5
        .4:
            call_method [foo]: Foo*, "done", {0: integer}
            return _: void
        .5:
            call_method [foo]: Foo*, "done", {1: integer}
            return _: void
        .6:
            unreachable
        "###);
    }

    #[test]
    fn if_else_statement_empty_body() {
        insta::assert_snapshot!(
            dump("if (foo.checked) {} else {}"),
            @r###"
            %0: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            return _: void
        .2:
            return _: void
        .3:
            unreachable
        "###);
    }

    #[test]
    fn return_nothing() {
        insta::assert_snapshot!(dump("{ return; }"), @r###"
        .0:
            return _: void
        .1:
            unreachable
        "###);
    }

    #[test]
    fn return_literal() {
        insta::assert_snapshot!(dump("{ return 'hello'; }"), @r###"
        .0:
            return "hello": QString
        .1:
            unreachable
        "###);
    }

    #[test]
    fn return_dynamic_expression() {
        insta::assert_snapshot!(dump("{ return foo.checked; }"), @r###"
            %0: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            return %0: bool
        .1:
            unreachable
        "###);
    }

    #[test]
    fn return_if() {
        insta::assert_snapshot!(
            dump(r###"{
            if (foo.checked)
                return foo.text;
            if (foo2.checked)
                return foo2.text;
            foo3.text
        }"###), @r###"
            %0: bool
            %1: QString
            %2: bool
            %3: QString
            %4: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .3
        .1:
            %1 = read_property [foo]: Foo*, "text"
            return %1: QString
        .2:
            br .3
        .3:
            %2 = read_property [foo2]: Foo*, "checked"
            br_cond %2: bool, .4, .6
        .4:
            %3 = read_property [foo2]: Foo*, "text"
            return %3: QString
        .5:
            br .6
        .6:
            %4 = read_property [foo3]: Foo*, "text"
            return %4: QString
        "###);
    }

    #[test]
    fn return_if_else_commplete() {
        insta::assert_snapshot!(
            dump(r###"{
            if (foo.checked) {
                return foo.text;
            } else if (foo2.checked) {
                return foo2.text;
            } else {
                return foo3.text;
            }
        }"###), @r###"
            %0: bool
            %1: QString
            %2: bool
            %3: QString
            %4: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .3
        .1:
            %1 = read_property [foo]: Foo*, "text"
            return %1: QString
        .2:
            unreachable
        .3:
            %2 = read_property [foo2]: Foo*, "checked"
            br_cond %2: bool, .4, .6
        .4:
            %3 = read_property [foo2]: Foo*, "text"
            return %3: QString
        .5:
            unreachable
        .6:
            %4 = read_property [foo3]: Foo*, "text"
            return %4: QString
        .7:
            unreachable
        .8:
            unreachable
        .9:
            unreachable
        "###);
    }

    #[test]
    fn return_if_else_partial() {
        insta::assert_snapshot!(
            dump(r###"{
            if (foo.checked) {
                return foo.text;
            } else if (foo2.checked) {
                foo2.text;
            } else {
                return foo3.text;
            }
        }"###), @r###"
            %0: bool
            %1: QString
            %2: bool
            %3: QString
            %4: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .3
        .1:
            %1 = read_property [foo]: Foo*, "text"
            return %1: QString
        .2:
            unreachable
        .3:
            %2 = read_property [foo2]: Foo*, "checked"
            br_cond %2: bool, .4, .5
        .4:
            %3 = read_property [foo2]: Foo*, "text"
            return %3: QString
        .5:
            %4 = read_property [foo3]: Foo*, "text"
            return %4: QString
        .6:
            unreachable
        .7:
            unreachable
        .8:
            unreachable
        "###);
    }

    #[test]
    fn return_if_else_partial_and_trailing_code() {
        insta::assert_snapshot!(
            dump(r###"{
            if (foo.checked) {
                return foo.text;
            } else if (foo2.checked) {
                foo2.text;
            } else {
                return foo3.text;
            }
            foo4.text;
        }"###), @r###"
            %0: bool
            %1: QString
            %2: bool
            %3: QString
            %4: QString
            %5: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .3
        .1:
            %1 = read_property [foo]: Foo*, "text"
            return %1: QString
        .2:
            br .8
        .3:
            %2 = read_property [foo2]: Foo*, "checked"
            br_cond %2: bool, .4, .5
        .4:
            %3 = read_property [foo2]: Foo*, "text"
            br .7
        .5:
            %4 = read_property [foo3]: Foo*, "text"
            return %4: QString
        .6:
            br .7
        .7:
            br .8
        .8:
            %5 = read_property [foo4]: Foo*, "text"
            return %5: QString
        "###);
    }

    #[test]
    fn return_with_trailing_garbage() {
        insta::assert_snapshot!(
            dump(r###"{
            if (foo.checked) {
                return foo.text;
                return;
            } else {
                { return foo2.text; }
                foo3.text;
            }
            foo4.text;
        }"###), @r###"
            %0: bool
            %1: QString
            %2: QString
            %3: QString
            %4: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .4
        .1:
            %1 = read_property [foo]: Foo*, "text"
            return %1: QString
        .2:
            return _: void
        .3:
            br .6
        .4:
            %2 = read_property [foo2]: Foo*, "text"
            return %2: QString
        .5:
            %3 = read_property [foo3]: Foo*, "text"
            br .6
        .6:
            %4 = read_property [foo4]: Foo*, "text"
            return %4: QString
        "###);
    }

    #[test]
    fn switch_basic() {
        insta::assert_snapshot!(
            dump(r###"{
            let s = foo.text;
            switch (s) {
            case "foo2":
                foo2.text;
                break;
            case "foo3":
                foo3.text;
                break;
            default:
                foo.text;
            }
        }"###), @r###"
            %0: QString
            %1: QString
            %2: bool
            %3: QString
            %4: bool
            %5: QString
            %6: QString
        .0:
            %0 = read_property [foo]: Foo*, "text"
            %1 = copy %0: QString
            br .2
        .1:
            unreachable
        .2:
            %2 = binary_op '==', %1: QString, "foo2": QString
            br_cond %2: bool, .3, .5
        .3:
            %3 = read_property [foo2]: Foo*, "text"
            return %3: QString
        .4:
            br .6
        .5:
            %4 = binary_op '==', %1: QString, "foo3": QString
            br_cond %4: bool, .6, .8
        .6:
            %5 = read_property [foo3]: Foo*, "text"
            return %5: QString
        .7:
            br .8
        .8:
            %6 = read_property [foo]: Foo*, "text"
            return %6: QString
        .9:
            unreachable
        "###);
    }

    #[test]
    fn switch_default_only() {
        insta::assert_snapshot!(
            dump(r###"{
            switch (foo.currentIndex) {
            default:
                foo.done(0);
            }
            foo.done(-1);
        }"###), @r###"
            %0: int
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            br .2
        .1:
            br .3
        .2:
            call_method [foo]: Foo*, "done", {0: integer}
            br .3
        .3:
            call_method [foo]: Foo*, "done", {-1: integer}
            return _: void
        "###);
    }

    #[test]
    fn switch_case_only() {
        insta::assert_snapshot!(
            dump(r###"{
            switch (foo.currentIndex) {
            case 1:
                foo.done(1);
                break;
            }
            foo.done(-1);
        }"###), @r###"
            %0: int
            %1: bool
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            br .2
        .1:
            br .5
        .2:
            %1 = binary_op '==', %0: int, 1: integer
            br_cond %1: bool, .3, .5
        .3:
            call_method [foo]: Foo*, "done", {1: integer}
            br .1
        .4:
            br .5
        .5:
            call_method [foo]: Foo*, "done", {-1: integer}
            return _: void
        "###);
    }

    #[test]
    fn switch_default_first_fall_through() {
        insta::assert_snapshot!(
            dump(r###"{
            switch (foo.currentIndex) {
            default:
                foo.done(0);
            case 1:
                foo.done(1);
            case 2:
                foo.done(2);
            }
            foo.done(-1);
        }"###), @r###"
            %0: int
            %1: bool
            %2: bool
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            br .2
        .1:
            br .7
        .2:
            %1 = binary_op '==', %0: int, 1: integer
            br_cond %1: bool, .3, .4
        .3:
            call_method [foo]: Foo*, "done", {1: integer}
            br .5
        .4:
            %2 = binary_op '==', %0: int, 2: integer
            br_cond %2: bool, .5, .6
        .5:
            call_method [foo]: Foo*, "done", {2: integer}
            br .7
        .6:
            call_method [foo]: Foo*, "done", {0: integer}
            br .3
        .7:
            call_method [foo]: Foo*, "done", {-1: integer}
            return _: void
        "###);
    }

    #[test]
    fn switch_default_mid_fall_through() {
        insta::assert_snapshot!(
            dump(r###"{
            switch (foo.currentIndex) {
            case 1:
                foo.done(1);
            default:
                foo.done(0);
            case 2:
                foo.done(2);
            }
            foo.done(-1);
        }"###), @r###"
            %0: int
            %1: bool
            %2: bool
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            br .2
        .1:
            br .7
        .2:
            %1 = binary_op '==', %0: int, 1: integer
            br_cond %1: bool, .3, .4
        .3:
            call_method [foo]: Foo*, "done", {1: integer}
            br .6
        .4:
            %2 = binary_op '==', %0: int, 2: integer
            br_cond %2: bool, .5, .6
        .5:
            call_method [foo]: Foo*, "done", {2: integer}
            br .7
        .6:
            call_method [foo]: Foo*, "done", {0: integer}
            br .5
        .7:
            call_method [foo]: Foo*, "done", {-1: integer}
            return _: void
        "###);
    }

    #[test]
    fn switch_default_end_fall_through() {
        insta::assert_snapshot!(
            dump(r###"{
            switch (foo.currentIndex) {
            case 1:
                foo.done(1);
            case 2:
                foo.done(2);
            default:
                foo.done(0);
            }
            foo.done(-1);
        }"###), @r###"
            %0: int
            %1: bool
            %2: bool
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            br .2
        .1:
            br .7
        .2:
            %1 = binary_op '==', %0: int, 1: integer
            br_cond %1: bool, .3, .4
        .3:
            call_method [foo]: Foo*, "done", {1: integer}
            br .5
        .4:
            %2 = binary_op '==', %0: int, 2: integer
            br_cond %2: bool, .5, .6
        .5:
            call_method [foo]: Foo*, "done", {2: integer}
            br .6
        .6:
            call_method [foo]: Foo*, "done", {0: integer}
            br .7
        .7:
            call_method [foo]: Foo*, "done", {-1: integer}
            return _: void
        "###);
    }

    #[test]
    fn switch_empty_fall_through() {
        insta::assert_snapshot!(
            dump(r###"{
            switch (foo.currentIndex) {
            case 1:
            case 2:
                foo.done(1);
                break;
            case 3:
                foo.done(3);
                break;
            }
            foo.done(-1);
        }"###), @r###"
            %0: int
            %1: bool
            %2: bool
            %3: bool
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            br .2
        .1:
            br .10
        .2:
            %1 = binary_op '==', %0: int, 1: integer
            br_cond %1: bool, .3, .4
        .3:
            br .5
        .4:
            %2 = binary_op '==', %0: int, 2: integer
            br_cond %2: bool, .5, .7
        .5:
            call_method [foo]: Foo*, "done", {1: integer}
            br .1
        .6:
            br .8
        .7:
            %3 = binary_op '==', %0: int, 3: integer
            br_cond %3: bool, .8, .10
        .8:
            call_method [foo]: Foo*, "done", {3: integer}
            br .1
        .9:
            br .10
        .10:
            call_method [foo]: Foo*, "done", {-1: integer}
            return _: void
        "###);
    }

    #[test]
    fn switch_return() {
        insta::assert_snapshot!(
            dump(r###"{
            switch (foo.currentIndex) {
            case 0:
                return "0";
            case 1:
                return "1";
            default:
                return "default";
            }
            "unreachable"
        }"###), @r###"
            %0: int
            %1: bool
            %2: bool
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            br .2
        .1:
            br .10
        .2:
            %1 = binary_op '==', %0: int, 0: integer
            br_cond %1: bool, .3, .5
        .3:
            return "0": QString
        .4:
            br .6
        .5:
            %2 = binary_op '==', %0: int, 1: integer
            br_cond %2: bool, .6, .8
        .6:
            return "1": QString
        .7:
            br .8
        .8:
            return "default": QString
        .9:
            br .10
        .10:
            return "unreachable": QString
        "###);
    }

    #[test]
    fn switch_conditional_break() {
        insta::assert_snapshot!(
            dump(r###"{
            switch (foo.currentIndex) {
            case 2:
                if (foo2.checked)
                    break;
                return "2";
            case 3:
                if (foo3.checked)
                    break;
                return "3";
            default:
                if (foo.checked)
                    break;
                return "default";
            }
            "break"
        }"###), @r###"
            %0: int
            %1: bool
            %2: bool
            %3: bool
            %4: bool
            %5: bool
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            br .2
        .1:
            br .19
        .2:
            %1 = binary_op '==', %0: int, 2: integer
            br_cond %1: bool, .3, .8
        .3:
            %2 = read_property [foo2]: Foo*, "checked"
            br_cond %2: bool, .4, .6
        .4:
            br .1
        .5:
            br .6
        .6:
            return "2": QString
        .7:
            br .9
        .8:
            %3 = binary_op '==', %0: int, 3: integer
            br_cond %3: bool, .9, .14
        .9:
            %4 = read_property [foo3]: Foo*, "checked"
            br_cond %4: bool, .10, .12
        .10:
            br .1
        .11:
            br .12
        .12:
            return "3": QString
        .13:
            br .14
        .14:
            %5 = read_property [foo]: Foo*, "checked"
            br_cond %5: bool, .15, .17
        .15:
            br .1
        .16:
            br .17
        .17:
            return "default": QString
        .18:
            br .19
        .19:
            return "break": QString
        "###);
    }

    #[test]
    fn switch_ternary_in_left_value() {
        insta::assert_snapshot!(
            dump(r###"{
            switch (foo.checked ? foo.currentIndex : foo2.currentIndex) {
            case 1:
                foo.done(1);
                break;
            case 2:
                foo.done(2);
                break;
            }
            foo.done(-1);
        }"###), @r###"
            %0: bool
            %1: int
            %2: int
            %3: int
            %4: bool
            %5: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = read_property [foo]: Foo*, "currentIndex"
            %3 = copy %1: int
            br .3
        .2:
            %2 = read_property [foo2]: Foo*, "currentIndex"
            %3 = copy %2: int
            br .3
        .3:
            br .5
        .4:
            br .11
        .5:
            %4 = binary_op '==', %3: int, 1: integer
            br_cond %4: bool, .6, .8
        .6:
            call_method [foo]: Foo*, "done", {1: integer}
            br .4
        .7:
            br .9
        .8:
            %5 = binary_op '==', %3: int, 2: integer
            br_cond %5: bool, .9, .11
        .9:
            call_method [foo]: Foo*, "done", {2: integer}
            br .4
        .10:
            br .11
        .11:
            call_method [foo]: Foo*, "done", {-1: integer}
            return _: void
        "###);
    }

    #[test]
    fn callback_block() {
        let env = Env::new();
        let code = env.build_callback(
            r###"{
                foo.done(0)
            }"###,
        );
        insta::assert_snapshot!(dump_code(&code), @r###"
        .0:
            call_method [foo]: Foo*, "done", {0: integer}
            return _: void
        "###);
    }

    #[test]
    fn callback_expr() {
        let env = Env::new();
        let code = env.build_callback("foo.done(0)");
        insta::assert_snapshot!(dump_code(&code), @r###"
        .0:
            call_method [foo]: Foo*, "done", {0: integer}
            return _: void
        "###);
    }

    #[test]
    fn callback_function_no_arg() {
        let env = Env::new();
        let code = env.build_callback(
            r###"function() {
                foo.done(0)
            }"###,
        );
        insta::assert_snapshot!(dump_code(&code), @r###"
        .0:
            call_method [foo]: Foo*, "done", {0: integer}
            return _: void
        "###);
    }

    #[test]
    fn callback_arrow_function_no_arg_no_block() {
        let env = Env::new();
        let code = env.build_callback("() => foo.done(0)");
        insta::assert_snapshot!(dump_code(&code), @r###"
        .0:
            call_method [foo]: Foo*, "done", {0: integer}
            return _: void
        "###);
    }

    #[test]
    fn callback_function_parenthesized() {
        let env = Env::new();
        assert!(env.try_build_callback("(function() {})").is_err());
    }
}
