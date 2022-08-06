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
        ty: TypeKind<'a>,
        byte_range: Range<usize>,
    ) -> Result<Self::Local, Self::Error> {
        let a = self.alloca(ty, byte_range).map_err(|_| {
            ExpressionError::OperationOnUnsupportedType(
                "local declaration".to_owned(),
                "void".to_owned(),
            )
        })?;
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

    fn visit_function_parameter(
        &mut self,
        ty: TypeKind<'a>,
        byte_range: Range<usize>,
    ) -> Result<Self::Local, Self::Error> {
        assert_eq!(
            self.code.locals.len(),
            self.code.parameter_count,
            "function parameters must be declared prior to any local declarations"
        );
        let a = self.alloca(ty, byte_range).map_err(|_| {
            ExpressionError::OperationOnUnsupportedType(
                "function parameter".to_owned(),
                "void".to_owned(),
            )
        })?;
        self.code.parameter_count = self.code.locals.len();
        Ok(a.name)
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

    fn visit_as_expression(
        &mut self,
        value: Self::Item,
        ty: TypeKind<'a>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        todo!();
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
