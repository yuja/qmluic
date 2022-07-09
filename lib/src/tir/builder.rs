use super::{
    BasicBlock, BinaryArithOp, CodeBody, ConstantValue, Local, NamedObject, Operand, Rvalue,
    Statement, Terminator,
};
use crate::diagnostic::Diagnostics;
use crate::qmlast::{BinaryOperator, Node, UnaryOperator};
use crate::typedexpr::{
    self, BuiltinFunctionKind, BuiltinMethodKind, DescribeType, ExpressionVisitor, RefSpace,
    TypeDesc,
};
use crate::typemap::{Class, Enum, NamedType, Property, TypeKind, TypeMapError, TypeSpace as _};
use std::num::TryFromIntError;
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

    fn current_basic_block_mut(&mut self) -> &mut BasicBlock<'a> {
        self.code
            .basic_blocks
            .last_mut()
            .expect("at least one basic block must exist")
    }

    fn push_statement(&mut self, stmt: Statement<'a>) {
        self.current_basic_block_mut().push_statement(stmt);
    }
}

impl<'a> ExpressionVisitor<'a> for CodeBuilder<'a> {
    type Item = Operand<'a>;
    type Label = ();
    type Error = ExpressionError;

    fn visit_integer(&mut self, value: u64) -> Result<Self::Item, Self::Error> {
        Ok(Operand::Constant(ConstantValue::Integer(value.try_into()?)))
    }

    fn visit_float(&mut self, value: f64) -> Result<Self::Item, Self::Error> {
        Ok(Operand::Constant(ConstantValue::Float(value)))
    }

    fn visit_string(&mut self, value: String) -> Result<Self::Item, Self::Error> {
        Ok(Operand::Constant(ConstantValue::CString(value)))
    }

    fn visit_bool(&mut self, value: bool) -> Result<Self::Item, Self::Error> {
        Ok(Operand::Constant(ConstantValue::Bool(value)))
    }

    fn visit_enum(
        &mut self,
        _enum_ty: Enum<'a>,
        _variant: &str,
    ) -> Result<Self::Item, Self::Error> {
        todo!()
    }

    fn visit_array(&mut self, _elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error> {
        todo!()
    }

    fn visit_object_ref(&mut self, cls: Class<'a>, name: &str) -> Result<Self::Item, Self::Error> {
        Ok(Operand::NamedObject(NamedObject::new(name, cls)))
    }

    fn visit_object_property(
        &mut self,
        object: Self::Item,
        property: Property<'a>,
    ) -> Result<Self::Item, Self::Error> {
        if !property.is_readable() {
            return Err(ExpressionError::UnreadableProperty);
        }
        let a = self.alloca(property.value_type()?);
        self.push_statement(Statement::Assign(
            a.name,
            Rvalue::ReadProperty(object, property),
        ));
        Ok(Operand::Local(a))
    }

    fn visit_object_builtin_method_call(
        &mut self,
        _object: Self::Item,
        _function: BuiltinMethodKind,
        _arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        todo!()
    }

    fn visit_builtin_call(
        &mut self,
        _function: BuiltinFunctionKind,
        _arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        todo!()
    }

    fn visit_unary_expression(
        &mut self,
        _operator: UnaryOperator,
        _argument: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        todo!()
    }

    fn visit_binary_expression(
        &mut self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        use BinaryOperator::*;
        match operator {
            LogicalAnd => todo!(),
            LogicalOr => todo!(),
            RightShift => todo!(),
            UnsignedRightShift => Err(ExpressionError::UnsupportedOperation(operator.to_string())),
            LeftShift => todo!(),
            BitwiseAnd => todo!(),
            BitwiseXor => todo!(),
            BitwiseOr => todo!(),
            Add => self.visit_binary_arith_expression(BinaryArithOp::Add, left, right),
            Sub => self.visit_binary_arith_expression(BinaryArithOp::Sub, left, right),
            Mul => self.visit_binary_arith_expression(BinaryArithOp::Mul, left, right),
            Div => self.visit_binary_arith_expression(BinaryArithOp::Div, left, right),
            Rem => self.visit_binary_arith_expression(BinaryArithOp::Rem, left, right),
            Exp => Err(ExpressionError::UnsupportedOperation(operator.to_string())),
            Equal | StrictEqual => todo!(),
            NotEqual | StrictNotEqual => todo!(),
            LessThan => todo!(),
            LessThanEqual => todo!(),
            GreaterThan => todo!(),
            GreaterThanEqual => todo!(),
            NullishCoalesce | Instanceof | In => {
                Err(ExpressionError::UnsupportedOperation(operator.to_string()))
            }
        }
    }

    fn visit_ternary_expression(
        &mut self,
        _condition: Self::Item,
        _consequence: Self::Item,
        _alternative: Self::Item,
        _condition_label: Self::Label,
        _consequence_label: Self::Label,
        _alternative_label: Self::Label,
    ) -> Result<Self::Item, Self::Error> {
        todo!()
    }

    fn mark_branch_point(&mut self) -> Self::Label {
        todo!()
    }
}

impl<'a> CodeBuilder<'a> {
    fn visit_binary_arith_expression(
        &mut self,
        op: BinaryArithOp,
        left: Operand<'a>,
        right: Operand<'a>,
    ) -> Result<Operand<'a>, ExpressionError> {
        match (left, right) {
            (Operand::Constant(l), Operand::Constant(r)) => self
                .eval_binary_arith_expression(op, l, r)
                .map(Operand::Constant),
            (left, right) => self.emit_binary_arith_instruction(
                op,
                ensure_concrete_string(left),
                ensure_concrete_string(right),
            ),
        }
    }

    fn emit_binary_arith_instruction(
        &mut self,
        op: BinaryArithOp,
        left: Operand<'a>,
        right: Operand<'a>,
    ) -> Result<Operand<'a>, ExpressionError> {
        use BinaryArithOp::*;
        let ty = deduce_concrete_type(op, left.type_desc(), right.type_desc())?;
        match &ty {
            &TypeKind::INT | &TypeKind::UINT | &TypeKind::DOUBLE => Ok(()),
            &TypeKind::STRING => match op {
                Add => Ok(()),
                Sub | Mul | Div | Rem => Err(ExpressionError::UnsupportedOperation(op.to_string())),
            },
            _ => Err(ExpressionError::UnsupportedOperation(op.to_string())),
        }?;
        let a = self.alloca(ty);
        self.push_statement(Statement::Assign(
            a.name,
            Rvalue::BinaryArithOp(op, left, right),
        ));
        Ok(Operand::Local(a))
    }

    /// Evaluates constant binary arithmetic expression.
    ///
    /// This is not for optimization. We don't want to allocate local variable without
    /// concrete integer/string type. And we do want to concatenate string literals to
    /// support translation.
    fn eval_binary_arith_expression(
        &self,
        op: BinaryArithOp,
        left: ConstantValue,
        right: ConstantValue,
    ) -> Result<ConstantValue, ExpressionError> {
        use BinaryArithOp::*;
        match (left, right) {
            (ConstantValue::Bool(_), ConstantValue::Bool(_)) => {
                Err(ExpressionError::UnsupportedOperation(op.to_string()))
            }
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => {
                let a = match op {
                    Add => i64::checked_add(l, r),
                    Sub => i64::checked_sub(l, r),
                    Mul => i64::checked_mul(l, r),
                    Div => i64::checked_div(l, r),
                    Rem => i64::checked_rem(l, r),
                };
                a.map(ConstantValue::Integer)
                    .ok_or(ExpressionError::IntegerOverflow)
            }
            (ConstantValue::Float(l), ConstantValue::Float(r)) => {
                let a = match op {
                    Add => l + r,
                    Sub => l - r,
                    Mul => l * r,
                    Div => l / r,
                    Rem => l % r,
                };
                Ok(ConstantValue::Float(a))
            }
            (ConstantValue::CString(l), ConstantValue::CString(r)) => match op {
                Add => Ok(ConstantValue::CString(l + &r)),
                Sub | Mul | Div | Rem => Err(ExpressionError::UnsupportedOperation(op.to_string())),
            },
            (ConstantValue::QString(_), ConstantValue::QString(_)) => {
                Err(ExpressionError::UnsupportedOperation(op.to_string()))
            }
            (left, right) => Err(ExpressionError::OperationOnIncompatibleTypes(
                op.to_string(),
                left.type_desc().qualified_name().into(),
                right.type_desc().qualified_name().into(),
            )),
        }
    }
}

#[derive(Clone, Debug, Error)]
enum ExpressionError {
    #[error("integer conversion failed: {0}")]
    IntegerConversion(#[from] TryFromIntError),
    #[error("integer overflow")]
    IntegerOverflow,
    #[error("type resolution failed: {0}")]
    TypeResolution(#[from] TypeMapError),
    #[error("operation '{0}' on incompatible types: {1} and {2}")]
    OperationOnIncompatibleTypes(String, String, String),
    #[error("operation '{0}' on undetermined type: {1}")]
    OperationOnUndeterminedType(String, String),
    #[error("unsupported operation '{0}'")]
    UnsupportedOperation(String),
    #[error("not a readable property")]
    UnreadableProperty,
}

fn is_compatible_enum(left: &Enum, right: &Enum) -> Result<bool, TypeMapError> {
    Ok(left == right
        || left
            .alias_enum()
            .transpose()?
            .map_or(false, |en| &en == right)
        || right
            .alias_enum()
            .transpose()?
            .map_or(false, |en| &en == left))
}

fn deduce_concrete_type<'a>(
    op_desc: impl ToString + Copy,
    left: TypeDesc<'a>,
    right: TypeDesc<'a>,
) -> Result<TypeKind<'a>, ExpressionError> {
    match deduce_type(op_desc, left, right) {
        Ok(TypeDesc::Concrete(ty)) => Ok(ty),
        Ok(TypeDesc::ConstInteger) => Ok(TypeKind::INT), // fallback to default
        Ok(TypeDesc::ConstString) => panic!("should have been converted to concrete string"),
        Ok(t @ TypeDesc::EmptyList) => Err(ExpressionError::OperationOnUndeterminedType(
            op_desc.to_string(),
            t.qualified_name().into(),
        )),
        Err(e) => Err(e),
    }
}

fn deduce_type<'a>(
    op_desc: impl ToString,
    left: TypeDesc<'a>,
    right: TypeDesc<'a>,
) -> Result<TypeDesc<'a>, ExpressionError> {
    match (left, right) {
        (left, right) if left == right => Ok(left),
        (l @ (TypeDesc::INT | TypeDesc::UINT), TypeDesc::ConstInteger) => Ok(l),
        (TypeDesc::ConstInteger, r @ (TypeDesc::INT | TypeDesc::UINT)) => Ok(r),
        (
            TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(l))),
            TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(r))),
        ) if is_compatible_enum(&l, &r)? => {
            Ok(TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(l))))
        }
        (
            TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(l))),
            TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(r))),
        ) => l
            .common_base_class(&r)
            .transpose()?
            .map(|c| TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(c))))
            .ok_or_else(|| {
                ExpressionError::OperationOnIncompatibleTypes(
                    op_desc.to_string(),
                    l.qualified_cxx_name().into(),
                    r.qualified_cxx_name().into(),
                )
            }),
        (left, right) => Err(ExpressionError::OperationOnIncompatibleTypes(
            op_desc.to_string(),
            left.qualified_name().into(),
            right.qualified_name().into(),
        )),
    }
}

fn ensure_concrete_string(x: Operand) -> Operand {
    match x {
        Operand::Constant(ConstantValue::CString(s)) => {
            Operand::Constant(ConstantValue::QString(s))
        }
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
    // TODO: pass return value via local? ensure concrete type?
    let a = typedexpr::walk(ctx, node, source, &mut builder, diagnostics)?;
    builder
        .current_basic_block_mut()
        .finalize(Terminator::Return(a));
    Some(builder.code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metatype;
    use crate::qmlast::{UiObjectDefinition, UiProgram};
    use crate::qmldoc::UiDocument;
    use crate::typedexpr::RefKind;
    use crate::typemap::{ModuleData, ModuleId, Namespace, TypeMap, TypeMapError};

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
                enums: vec![metatype::Enum::with_values("Bar", ["Bar0", "Bar1", "Bar2"])],
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
                "foo" => match self.type_space.get_type("Foo").unwrap().unwrap() {
                    NamedType::Class(cls) => Some(Ok(RefKind::Object(cls))),
                    _ => panic!("Foo must be of class type"),
                },
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
            return "foo": string
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
    fn constant_integer_arithmetic() {
        insta::assert_snapshot!(dump("(1 + 2 * 3) / 4"), @r###"
        .0:
            return 1: integer
        "###);
    }

    #[test]
    fn constant_string_concatenation() {
        insta::assert_snapshot!(dump("'foo' + 'bar'"), @r###"
        .0:
            return "foobar": string
        "###);
    }

    #[test]
    fn dynamic_integer_arithmetic() {
        insta::assert_snapshot!(dump("foo.currentIndex + 1"), @r###"
            %0: int
            %1: int
        .0:
            %0 = read_property [foo]: Foo*, "currentIndex"
            %1 = binary_arith_op '+', %0: int, 1: integer
            return %1: int
        "###);
    }

    #[test]
    fn dynamic_string_concatenation() {
        insta::assert_snapshot!(dump("'Hello ' + foo.text"), @r###"
            %0: QString
            %1: QString
        .0:
            %0 = read_property [foo]: Foo*, "text"
            %1 = binary_arith_op '+', "Hello ": QString, %0: QString
            return %1: QString
        "###);
    }
}
