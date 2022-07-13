use super::ceval;
use super::core::{
    BasicBlock, BasicBlockRef, BinaryArithOp, BinaryBitwiseOp, BinaryLogicalOp, BinaryOp, CodeBody,
    ComparisonOp, Constant, ConstantValue, EnumVariant, Local, NamedObject, Operand, Rvalue,
    ShiftOp, Statement, Terminator, UnaryArithOp, UnaryBitwiseOp, UnaryLogicalOp, UnaryOp,
};
use super::typeutil::{self, TypeError};
use crate::diagnostic::Diagnostics;
use crate::qmlast::{BinaryOperator, Node, UnaryOperator};
use crate::typedexpr::{
    self, BuiltinFunctionKind, BuiltinMethodKind, DescribeType, ExpressionVisitor, RefSpace,
    TypeDesc,
};
use crate::typemap::{Class, Enum, NamedType, PrimitiveType, Property, TypeKind, TypeMapError};
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

    fn alloca(&mut self, ty: TypeKind<'a>) -> Local<'a> {
        let a = Local::new(self.code.locals.len(), ty);
        self.code.locals.push(a.clone());
        a
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

    fn push_statement(&mut self, stmt: Statement<'a>) {
        self.current_basic_block_mut().push_statement(stmt);
    }

    fn emit_result(&mut self, ty: TypeKind<'a>, rv: Rvalue<'a>) -> Operand<'a> {
        let a = self.alloca(ty);
        self.push_statement(Statement::Assign(a.name, rv));
        Operand::Local(a)
    }
}

impl<'a> ExpressionVisitor<'a> for CodeBuilder<'a> {
    type Item = Operand<'a>;
    type Label = BasicBlockRef;
    type Error = ExpressionError;

    fn visit_integer(
        &mut self,
        value: u64,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let v = ConstantValue::Integer(value.try_into()?);
        Ok(Operand::Constant(Constant::new(v)))
    }

    fn visit_float(
        &mut self,
        value: f64,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let v = ConstantValue::Float(value);
        Ok(Operand::Constant(Constant::new(v)))
    }

    fn visit_string(
        &mut self,
        value: String,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let v = ConstantValue::CString(value);
        Ok(Operand::Constant(Constant::new(v)))
    }

    fn visit_bool(
        &mut self,
        value: bool,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let v = ConstantValue::Bool(value);
        Ok(Operand::Constant(Constant::new(v)))
    }

    fn visit_enum(
        &mut self,
        enum_ty: Enum<'a>,
        variant: &str,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        Ok(Operand::EnumVariant(EnumVariant::new(enum_ty, variant)))
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
            ))
        } else {
            Ok(Operand::Constant(Constant::new(ConstantValue::EmptyList)))
        }
    }

    fn visit_object_ref(
        &mut self,
        cls: Class<'a>,
        name: &str,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        Ok(Operand::NamedObject(NamedObject::new(name, cls)))
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
        ))
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
        Ok(self.emit_result(ty, Rvalue::CallBuiltinMethod(object, function, arguments)))
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
        Ok(self.emit_result(ty, Rvalue::CallBuiltinFunction(function, arguments)))
    }

    fn visit_unary_expression(
        &mut self,
        operator: UnaryOperator,
        argument: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let unary = UnaryOp::try_from(operator)?;
        match argument {
            Operand::Constant(a) => match unary {
                UnaryOp::Arith(op) => ceval::eval_unary_arith_expression(op, a.value),
                UnaryOp::Bitwise(op) => ceval::eval_unary_bitwise_expression(op, a.value),
                UnaryOp::Logical(op) => ceval::eval_unary_logical_expression(op, a.value),
            }
            .map(|v| Operand::Constant(Constant::new(v))),
            argument => self.emit_unary_expression(unary, argument, byte_range),
        }
    }

    fn visit_binary_expression(
        &mut self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error> {
        let binary = BinaryOp::try_from(operator)?;
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
            .map(|v| Operand::Constant(Constant::new(v))),
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
        let sink = self.alloca(ty);
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
            block.push_statement(Statement::Assign(sink.name, Rvalue::Copy(src)));
            block.finalize(Terminator::Br(alternative_ref.next())); // end
        }
        Ok(Operand::Local(sink))
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
        .map(|ty| self.emit_result(ty, Rvalue::UnaryOp(unary, argument)))
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
        .map(|ty| self.emit_result(ty, Rvalue::BinaryOp(binary, left, right)))
    }
}

impl UnaryOp {
    fn try_from(operator: UnaryOperator) -> Result<Self, ExpressionError> {
        use UnaryOperator::*;
        match operator {
            LogicalNot => Ok(UnaryOp::Logical(UnaryLogicalOp::Not)),
            BitwiseNot => Ok(UnaryOp::Bitwise(UnaryBitwiseOp::Not)),
            Minus => Ok(UnaryOp::Arith(UnaryArithOp::Minus)),
            Plus => Ok(UnaryOp::Arith(UnaryArithOp::Plus)),
            Typeof | Void | Delete => {
                Err(ExpressionError::UnsupportedOperation(operator.to_string()))
            }
        }
    }
}

impl BinaryOp {
    fn try_from(operator: BinaryOperator) -> Result<Self, ExpressionError> {
        use BinaryOperator::*;
        match operator {
            LogicalAnd => Ok(BinaryOp::Logical(BinaryLogicalOp::And)),
            LogicalOr => Ok(BinaryOp::Logical(BinaryLogicalOp::Or)),
            RightShift => Ok(BinaryOp::Shift(ShiftOp::RightShift)),
            UnsignedRightShift => Err(ExpressionError::UnsupportedOperation(operator.to_string())),
            LeftShift => Ok(BinaryOp::Shift(ShiftOp::LeftShift)),
            BitwiseAnd => Ok(BinaryOp::Bitwise(BinaryBitwiseOp::And)),
            BitwiseXor => Ok(BinaryOp::Bitwise(BinaryBitwiseOp::Xor)),
            BitwiseOr => Ok(BinaryOp::Bitwise(BinaryBitwiseOp::Or)),
            Add => Ok(BinaryOp::Arith(BinaryArithOp::Add)),
            Sub => Ok(BinaryOp::Arith(BinaryArithOp::Sub)),
            Mul => Ok(BinaryOp::Arith(BinaryArithOp::Mul)),
            Div => Ok(BinaryOp::Arith(BinaryArithOp::Div)),
            Rem => Ok(BinaryOp::Arith(BinaryArithOp::Rem)),
            Exp => Err(ExpressionError::UnsupportedOperation(operator.to_string())),
            Equal | StrictEqual => Ok(BinaryOp::Comparison(ComparisonOp::Equal)),
            NotEqual | StrictNotEqual => Ok(BinaryOp::Comparison(ComparisonOp::NotEqual)),
            LessThan => Ok(BinaryOp::Comparison(ComparisonOp::LessThan)),
            LessThanEqual => Ok(BinaryOp::Comparison(ComparisonOp::LessThanEqual)),
            GreaterThan => Ok(BinaryOp::Comparison(ComparisonOp::GreaterThan)),
            GreaterThanEqual => Ok(BinaryOp::Comparison(ComparisonOp::GreaterThanEqual)),
            NullishCoalesce | Instanceof | In => {
                Err(ExpressionError::UnsupportedOperation(operator.to_string()))
            }
        }
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
        }) => Operand::Constant(Constant {
            value: ConstantValue::QString(s),
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
    C: RefSpace<'a>,
{
    let mut builder = CodeBuilder::new();
    let a = typedexpr::walk(ctx, node, source, &mut builder, diagnostics)?;
    builder
        .current_basic_block_mut()
        .finalize(Terminator::Return(ensure_concrete_string(a)));
    Some(builder.code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metatype;
    use crate::qmlast::{UiObjectDefinition, UiProgram};
    use crate::qmldoc::UiDocument;
    use crate::typedexpr::RefKind;
    use crate::typemap::{ModuleData, ModuleId, Namespace, TypeMap, TypeMapError, TypeSpace as _};

    struct Env {
        type_map: TypeMap,
        module_id: ModuleId<'static>,
    }

    impl Env {
        fn new() -> Self {
            let mut type_map = TypeMap::with_primitive_types();
            let module_id = ModuleId::Named("foo".into());
            let mut module_data = ModuleData::with_builtins();
            let foo_meta = metatype::Class {
                class_name: "Foo".to_owned(),
                qualified_class_name: "Foo".to_owned(),
                object: true,
                enums: vec![metatype::Enum::with_values(
                    "Bar",
                    ["Bar0", "Bar1", "Bar2", "Bar3"],
                )],
                properties: vec![
                    metatype::Property {
                        name: "checked".to_owned(),
                        r#type: "bool".to_owned(),
                        read: Some("isChecked".to_owned()),
                        write: Some("setChecked".to_owned()),
                        notify: Some("toggled".to_owned()),
                        ..Default::default()
                    },
                    metatype::Property {
                        name: "currentIndex".to_owned(),
                        r#type: "int".to_owned(),
                        read: Some("currentIndex".to_owned()),
                        write: Some("setCurrentIndex".to_owned()),
                        notify: Some("currentIndexChanged".to_owned()),
                        ..Default::default()
                    },
                    metatype::Property {
                        name: "text".to_owned(),
                        r#type: "QString".to_owned(),
                        read: Some("text".to_owned()),
                        write: Some("setText".to_owned()),
                        notify: Some("textChanged".to_owned()),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            };
            module_data.extend([foo_meta, metatype::Class::new("A")]);
            type_map.insert_module(module_id.clone(), module_data);
            Env {
                type_map,
                module_id,
            }
        }

        fn build(&self, expr_source: &str) -> CodeBody {
            self.try_build(expr_source).unwrap()
        }

        fn try_build(&self, expr_source: &str) -> Result<CodeBody, Diagnostics> {
            let ctx = Context {
                type_space: self.type_map.get_module(&self.module_id).unwrap(),
            };

            let doc = UiDocument::parse(format!("A {{ a: {expr_source}}}"), "MyType", None);
            let program = UiProgram::from_node(doc.root_node(), doc.source()).unwrap();
            let obj =
                UiObjectDefinition::from_node(program.root_object_node(), doc.source()).unwrap();
            let map = obj.build_binding_map(doc.source()).unwrap();
            let node = map.get("a").unwrap().get_node().unwrap();

            let mut diagnostics = Diagnostics::new();
            super::build(&ctx, node, doc.source(), &mut diagnostics).ok_or(diagnostics)
        }
    }

    struct Context<'a> {
        type_space: Namespace<'a>,
    }

    impl<'a> RefSpace<'a> for Context<'a> {
        fn get_ref(&self, name: &str) -> Option<Result<RefKind<'a>, TypeMapError>> {
            match name {
                "foo" | "foo2" | "foo3" | "foo4" => {
                    match self.type_space.get_type("Foo").unwrap().unwrap() {
                        NamedType::Class(cls) => Some(Ok(RefKind::Object(cls))),
                        _ => panic!("Foo must be of class type"),
                    }
                }
                "qsTr" => Some(Ok(RefKind::BuiltinFunction(BuiltinFunctionKind::Tr))),
                _ => self.type_space.get_ref(name),
            }
        }
    }

    fn dump(expr_source: &str) -> String {
        let env = Env::new();
        let code = env.build(expr_source);
        let mut buf = Vec::new();
        super::super::dump_code_body(&mut buf, &code).unwrap();
        String::from_utf8(buf).unwrap()
    }

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
}
