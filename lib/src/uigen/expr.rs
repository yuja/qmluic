use super::gadget::{Gadget, SizePolicy};
use super::xmlutil;
use super::{BuildContext, XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{BinaryOperator, Node, UiBindingMap, UiBindingValue, UnaryOperator};
use crate::typedexpr::{self, DescribeType, ExpressionVisitor, TypeDesc};
use crate::typemap::{Class, Enum, PrimitiveType, Type, TypeSpace};
use quick_xml::events::{BytesStart, BytesText, Event};
use std::fmt;
use std::io;
use thiserror::Error;

/// Variant for the constant expressions which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub enum Value {
    Simple(SimpleValue),
    Gadget(Gadget),
    SizePolicy(SizePolicy),
}

impl Value {
    /// Generates constant expression of `ty` type from the given `binding_value`.
    pub(super) fn from_binding_value(
        ctx: &BuildContext,
        ty: &Type,
        binding_value: &UiBindingValue,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match binding_value {
            UiBindingValue::Node(n) => {
                SimpleValue::from_expression(ctx, ty, *n, diagnostics).map(Value::Simple)
            }
            UiBindingValue::Map(n, m) => match ty {
                Type::Class(cls) => Self::from_binding_map(ctx, cls, *n, m, diagnostics),
                _ => {
                    diagnostics.push(Diagnostic::error(
                        n.byte_range(),
                        format!(
                            "binding map cannot be parsed as non-class type '{}'",
                            ty.qualified_name()
                        ),
                    ));
                    None
                }
            },
        }
    }

    /// Generates constant expression of `cls` type from the given `binding_map`.
    fn from_binding_map(
        ctx: &BuildContext,
        cls: &Class,
        node: Node,
        binding_map: &UiBindingMap,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match cls.name() {
            "QSizePolicy" => {
                let policy = SizePolicy::from_binding_map(ctx, cls, binding_map, diagnostics);
                Some(Value::SizePolicy(policy))
            }
            _ => Gadget::from_binding_map(ctx, cls, node, binding_map, diagnostics)
                .map(Value::Gadget),
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use Value::*;
        match self {
            Simple(x) => x.serialize_to_xml(writer),
            Gadget(x) => x.serialize_to_xml(writer),
            SizePolicy(x) => x.serialize_to_xml(writer),
        }
    }

    pub(super) fn serialize_to_xml_as<W, T>(
        &self,
        writer: &mut XmlWriter<W>,
        tag_name: T,
    ) -> XmlResult<()>
    where
        W: io::Write,
        T: AsRef<[u8]>,
    {
        use Value::*;
        match self {
            Simple(x) => x.serialize_to_xml_as(writer, tag_name),
            Gadget(x) => x.serialize_to_xml_as(writer, tag_name),
            SizePolicy(x) => x.serialize_to_xml_as(writer, tag_name),
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Simple(x) => x.as_number(),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&str> {
        match self {
            Value::Simple(x) => x.as_enum(),
            _ => None,
        }
    }
}

/// Constant expression which can be serialized to UI XML as a simple tagged value.
#[derive(Clone, Debug)]
pub enum SimpleValue {
    Bool(bool),
    Number(f64),
    String { s: String, tr: bool },
    Enum(String),
    Set(String),
}

impl SimpleValue {
    /// Generates value of `ty` type from the given expression `node`.
    fn from_expression(
        ctx: &BuildContext,
        ty: &Type,
        node: Node,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match ty {
            Type::Class(_) => {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!(
                        "unsupported constant value expression: class '{}'",
                        ty.qualified_name(),
                    ),
                ));
                None
            }
            Type::Enum(en) => {
                let (res_t, res_expr, _) = typedexpr::walk(
                    &ctx.type_map.root(), // TODO: should be QML space, not C++ metatype space
                    node,
                    ctx.source,
                    &ExpressionFormatter,
                    diagnostics,
                )?;
                match &res_t {
                    TypeDesc::Enum(res_en) if is_compatible_enum(res_en, en) => {
                        if en.is_flag() {
                            Some(SimpleValue::Set(res_expr))
                        } else {
                            Some(SimpleValue::Enum(res_expr))
                        }
                    }
                    _ => {
                        diagnostics.push(Diagnostic::error(
                            node.byte_range(),
                            format!(
                                "expression type mismatch (expected: {}, actual: {})",
                                ty.qualified_name(),
                                res_t.qualified_name()
                            ),
                        ));
                        None
                    }
                }
            }
            Type::Namespace(_) => {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!(
                        "unsupported constant value expression: namespace '{}'",
                        ty.qualified_name(),
                    ),
                ));
                None
            }
            Type::Primitive(p) => {
                let res = typedexpr::walk(
                    &ctx.type_map.root(), // TODO: should be QML space, not C++ metatype space
                    node,
                    ctx.source,
                    &ExpressionEvaluator,
                    diagnostics,
                )?;
                if !describe_primitive_type(*p).map_or(false, |t| res.type_desc() == t) {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!(
                            "evaluated type mismatch (expected: {}, actual: {})",
                            ty.qualified_name(),
                            res.type_desc().qualified_name()
                        ),
                    ));
                    return None;
                }
                match res {
                    EvaluatedValue::Bool(v) => Some(SimpleValue::Bool(v)),
                    EvaluatedValue::Number(v) => Some(SimpleValue::Number(v)),
                    EvaluatedValue::String(s) => Some(SimpleValue::String { s, tr: false }),
                    EvaluatedValue::TrString(s) => Some(SimpleValue::String { s, tr: true }),
                }
            }
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use SimpleValue::*;
        let tag_name = match self {
            Bool(_) => "bool",
            Number(_) => "number",
            String { .. } => "string",
            Enum(_) => "enum",
            Set(_) => "set",
        };
        self.serialize_to_xml_as(writer, tag_name)
    }

    pub(super) fn serialize_to_xml_as<W, T>(
        &self,
        writer: &mut XmlWriter<W>,
        tag_name: T,
    ) -> XmlResult<()>
    where
        W: io::Write,
        T: AsRef<[u8]>,
    {
        use SimpleValue::*;
        match self {
            String { s, tr } => {
                let mut tag = BytesStart::borrowed_name(tag_name.as_ref());
                if !tr {
                    tag.push_attribute(("notr", "true"));
                }
                writer.write_event(Event::Start(tag.to_borrowed()))?;
                writer.write_event(Event::Text(BytesText::from_plain_str(s)))?;
                writer.write_event(Event::End(tag.to_end()))
            }
            _ => xmlutil::write_tagged_str(writer, tag_name, self.to_string()),
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            SimpleValue::Number(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&str> {
        match self {
            SimpleValue::Enum(x) | SimpleValue::Set(x) => Some(x),
            _ => None,
        }
    }
}

impl fmt::Display for SimpleValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SimpleValue::*;
        match self {
            Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Number(d) => write!(f, "{}", d),
            String { s, .. } | Enum(s) | Set(s) => write!(f, "{}", s),
        }
    }
}

fn describe_primitive_type(t: PrimitiveType) -> Option<TypeDesc<'static>> {
    match t {
        PrimitiveType::Bool => Some(TypeDesc::Bool),
        PrimitiveType::Int => Some(TypeDesc::Number),
        PrimitiveType::QReal => Some(TypeDesc::Number),
        PrimitiveType::QString => Some(TypeDesc::String),
        PrimitiveType::UInt => Some(TypeDesc::Number),
        PrimitiveType::Void => None,
    }
}

#[derive(Clone, Debug, Error)]
enum ExpressionError {
    #[error("unsupported literal: {0}")]
    UnsupportedLiteral(&'static str),
    #[error("unsupported function call")]
    UnsupportedFunctionCall,
    #[error("unsupported unary operation on type: {0}")]
    UnsupportedUnaryOperationOnType(String),
    #[error("unsupported binary operation on types: {0} and {1}")]
    UnsupportedBinaryOperationOnType(String, String),
}

/// Evaluates expression tree as arbitrary constant value expression.
///
/// Here we don't follow the JavaScript language model, but try to be stricter.
#[derive(Debug)]
struct ExpressionEvaluator;

#[derive(Clone, Debug)]
enum EvaluatedValue {
    Bool(bool),
    Number(f64),
    String(String),
    TrString(String),
}

impl DescribeType<'_> for EvaluatedValue {
    fn type_desc(&self) -> TypeDesc<'static> {
        match self {
            EvaluatedValue::Bool(_) => TypeDesc::Bool,
            EvaluatedValue::Number(_) => TypeDesc::Number,
            EvaluatedValue::String(_) => TypeDesc::String,
            EvaluatedValue::TrString(_) => TypeDesc::String,
        }
    }
}

impl<'a> ExpressionVisitor<'a> for ExpressionEvaluator {
    type Item = EvaluatedValue;
    type Error = ExpressionError;

    fn visit_number(&self, value: f64) -> Result<Self::Item, Self::Error> {
        Ok(EvaluatedValue::Number(value))
    }

    fn visit_string(&self, value: String) -> Result<Self::Item, Self::Error> {
        Ok(EvaluatedValue::String(value))
    }

    fn visit_bool(&self, value: bool) -> Result<Self::Item, Self::Error> {
        Ok(EvaluatedValue::Bool(value))
    }

    fn visit_enum(&self, _enum_ty: Enum<'a>, _variant: &str) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("enum")) // enum value is unknown
    }

    fn visit_call_expression(
        &self,
        function: &str,
        mut arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        match function {
            "qsTr" if arguments.len() == 1 => match arguments.pop() {
                Some(EvaluatedValue::String(a)) => Ok(EvaluatedValue::TrString(a)),
                _ => Err(ExpressionError::UnsupportedFunctionCall),
            },
            _ => Err(ExpressionError::UnsupportedFunctionCall),
        }
    }

    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        argument: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        use UnaryOperator::*;
        let type_error = {
            let arg_t = argument.type_desc();
            move || ExpressionError::UnsupportedUnaryOperationOnType(arg_t.qualified_name().into())
        };
        match argument {
            EvaluatedValue::Bool(a) => match operator {
                LogicalNot => Ok(EvaluatedValue::Bool(!a)),
                BitwiseNot => Err(type_error()),
                Minus | Plus => Err(type_error()),
                Typeof | Void | Delete => Err(type_error()),
            },
            EvaluatedValue::Number(a) => match operator {
                // TODO: handle overflow, etc.
                LogicalNot => Err(type_error()), // TODO: !
                BitwiseNot => Err(type_error()), // TODO: ~
                Minus => Ok(EvaluatedValue::Number(-a)),
                Plus => Ok(EvaluatedValue::Number(a)),
                Typeof | Void | Delete => Err(type_error()),
            },
            EvaluatedValue::String(_) => Err(type_error()),
            EvaluatedValue::TrString(_) => Err(type_error()), // can't evaluate as constant
        }
    }

    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        use BinaryOperator::*;
        let type_error = {
            let left_t = left.type_desc();
            let right_t = right.type_desc();
            move || {
                ExpressionError::UnsupportedBinaryOperationOnType(
                    left_t.qualified_name().into(),
                    right_t.qualified_name().into(),
                )
            }
        };
        match (left, right) {
            #[allow(clippy::bool_comparison)]
            (EvaluatedValue::Bool(l), EvaluatedValue::Bool(r)) => match operator {
                LogicalAnd => Ok(EvaluatedValue::Bool(l && r)),
                LogicalOr => Ok(EvaluatedValue::Bool(l || r)),
                RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                BitwiseAnd => Ok(EvaluatedValue::Bool(l & r)),
                BitwiseXor => Ok(EvaluatedValue::Bool(l ^ r)),
                BitwiseOr => Ok(EvaluatedValue::Bool(l | r)),
                Add | Sub | Mul | Div | Rem | Exp => Err(type_error()),
                Equal => Ok(EvaluatedValue::Bool(l == r)),
                StrictEqual => Ok(EvaluatedValue::Bool(l == r)),
                NotEqual => Ok(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Ok(EvaluatedValue::Bool(l != r)),
                LessThan => Ok(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Ok(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Ok(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Ok(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce | Instanceof | In => Err(type_error()),
            },
            (EvaluatedValue::Number(l), EvaluatedValue::Number(r)) => match operator {
                // TODO: handle overflow, etc.
                LogicalAnd | LogicalOr => Err(type_error()),
                // TODO: >>, (unsigned)>>, <<
                RightShift => Err(type_error()),
                UnsignedRightShift => Err(type_error()),
                LeftShift => Err(type_error()),
                // TODO: &, ^, |
                BitwiseAnd => Err(type_error()),
                BitwiseXor => Err(type_error()),
                BitwiseOr => Err(type_error()),
                Add => Ok(EvaluatedValue::Number(l + r)),
                Sub => Ok(EvaluatedValue::Number(l - r)),
                Mul => Ok(EvaluatedValue::Number(l * r)),
                Div => Ok(EvaluatedValue::Number(l / r)),
                Rem => Ok(EvaluatedValue::Number(l % r)),
                Exp => Ok(EvaluatedValue::Number(l.powf(r))),
                Equal => Ok(EvaluatedValue::Bool(l == r)),
                StrictEqual => Ok(EvaluatedValue::Bool(l == r)),
                NotEqual => Ok(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Ok(EvaluatedValue::Bool(l != r)),
                LessThan => Ok(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Ok(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Ok(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Ok(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce => Err(type_error()),
                Instanceof => Err(type_error()),
                In => Err(type_error()),
            },
            (EvaluatedValue::String(l), EvaluatedValue::String(r)) => match operator {
                LogicalAnd | LogicalOr => Err(type_error()),
                RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                BitwiseAnd | BitwiseXor | BitwiseOr => Err(type_error()),
                Add => Ok(EvaluatedValue::String(l + &r)),
                Sub | Mul | Div | Rem | Exp => Err(type_error()),
                Equal => Ok(EvaluatedValue::Bool(l == r)),
                StrictEqual => Ok(EvaluatedValue::Bool(l == r)),
                NotEqual => Ok(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Ok(EvaluatedValue::Bool(l != r)),
                LessThan => Ok(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Ok(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Ok(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Ok(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce => Err(type_error()),
                Instanceof => Err(type_error()),
                In => Err(type_error()),
            },
            (EvaluatedValue::TrString(_), _) => Err(type_error()), // can't evaluate as constant
            (_, EvaluatedValue::TrString(_)) => Err(type_error()), // can't evaluate as constant
            _ => Err(type_error()),
        }
    }
}

/// Formats expression tree as arbitrary constant value expression.
///
/// Here we don't strictly follow the JavaScript language model, but try 1:1 mapping.
#[derive(Debug)]
struct ExpressionFormatter;

impl<'a> DescribeType<'a> for (TypeDesc<'a>, String, u32) {
    fn type_desc(&self) -> TypeDesc<'a> {
        self.0.clone()
    }
}

impl<'a> ExpressionVisitor<'a> for ExpressionFormatter {
    type Item = (TypeDesc<'a>, String, u32);
    type Error = ExpressionError;

    fn visit_number(&self, value: f64) -> Result<Self::Item, Self::Error> {
        Ok((TypeDesc::Number, value.to_string(), PREC_TERM))
    }

    fn visit_string(&self, value: String) -> Result<Self::Item, Self::Error> {
        Ok((TypeDesc::String, format!("{:?}", value), PREC_TERM)) // TODO: escape per C spec)
    }

    fn visit_bool(&self, value: bool) -> Result<Self::Item, Self::Error> {
        Ok((
            TypeDesc::Bool,
            if value {
                "true".to_owned()
            } else {
                "false".to_owned()
            },
            PREC_TERM,
        ))
    }

    fn visit_enum(&self, enum_ty: Enum<'a>, variant: &str) -> Result<Self::Item, Self::Error> {
        let res_expr = enum_ty.qualify_variant_name(variant);
        Ok((TypeDesc::Enum(enum_ty), res_expr, PREC_SCOPE))
    }

    fn visit_call_expression(
        &self,
        function: &str,
        arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        match function {
            "qsTr" if arguments.len() == 1 => {
                if let (TypeDesc::String, expr, _prec) = &arguments[0] {
                    Ok((TypeDesc::String, format!("qsTr({})", expr), PREC_CALL))
                } else {
                    Err(ExpressionError::UnsupportedFunctionCall)
                }
            }
            _ => Err(ExpressionError::UnsupportedFunctionCall),
        }
    }

    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        (arg_t, arg_expr, arg_prec): Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        use UnaryOperator::*;
        let type_error = {
            let arg_t = arg_t.clone();
            move || ExpressionError::UnsupportedUnaryOperationOnType(arg_t.qualified_name().into())
        };
        let res_prec = unary_operator_precedence(operator);
        let res_expr = [
            unary_operator_str(operator),
            &maybe_paren(res_prec, arg_expr, arg_prec),
        ]
        .concat();
        match arg_t {
            TypeDesc::Bool => match operator {
                LogicalNot => Ok((TypeDesc::Bool, res_expr, res_prec)),
                BitwiseNot => Err(type_error()),
                Minus | Plus => Err(type_error()),
                Typeof | Void | Delete => Err(type_error()),
            },
            TypeDesc::Number => match operator {
                LogicalNot => Ok((TypeDesc::Bool, res_expr, res_prec)),
                BitwiseNot | Minus | Plus => Ok((TypeDesc::Number, res_expr, res_prec)),
                Typeof | Void | Delete => Err(type_error()),
            },
            TypeDesc::String => Err(type_error()),
            TypeDesc::Enum(en) => match operator {
                LogicalNot => Ok((TypeDesc::Bool, res_expr, res_prec)),
                BitwiseNot => Ok((TypeDesc::Enum(en), res_expr, res_prec)),
                Minus | Plus => Err(type_error()),
                Typeof | Void | Delete => Err(type_error()),
            },
        }
    }

    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        (left_t, left_expr, left_prec): Self::Item,
        (right_t, right_expr, right_prec): Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        use BinaryOperator::*;
        let type_error = {
            let left_t = left_t.clone();
            let right_t = right_t.clone();
            move || {
                ExpressionError::UnsupportedBinaryOperationOnType(
                    left_t.qualified_name().into(),
                    right_t.qualified_name().into(),
                )
            }
        };
        let res_prec = binary_operator_precedence(operator);
        match (left_t, right_t) {
            (TypeDesc::Bool, TypeDesc::Bool) => {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                    BitwiseAnd | BitwiseXor | BitwiseOr => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    Add | Sub | Mul | Div | Rem | Exp => Err(type_error()),
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (TypeDesc::Number, TypeDesc::Number) => {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Err(type_error()),
                    // TODO: >>, (unsigned)>>, <<
                    RightShift => Err(type_error()),
                    UnsignedRightShift => Err(type_error()),
                    LeftShift => Err(type_error()),
                    // TODO: &, ^, |
                    BitwiseAnd => Err(type_error()),
                    BitwiseXor => Err(type_error()),
                    BitwiseOr => Err(type_error()),
                    Add | Sub | Mul | Div | Rem => Ok((TypeDesc::Number, res_expr, res_prec)),
                    Exp => Err(type_error()), // TODO
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (TypeDesc::String, TypeDesc::String) => {
                let (left_expr, left_prec) = (format!("QString({})", left_expr), PREC_CALL);
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Err(type_error()),
                    RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                    BitwiseAnd | BitwiseXor | BitwiseOr => Err(type_error()),
                    Add => Ok((TypeDesc::String, res_expr, res_prec)),
                    Sub | Mul | Div | Rem | Exp => Err(type_error()),
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::Bool, res_expr, res_prec)),

                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (TypeDesc::Enum(left_en), TypeDesc::Enum(right_en))
                if is_compatible_enum(&left_en, &right_en) =>
            {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                    BitwiseAnd | BitwiseXor | BitwiseOr => {
                        Ok((TypeDesc::Enum(left_en), res_expr, res_prec))
                    }
                    Add | Sub | Mul | Div | Rem | Exp => Err(type_error()),
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            _ => Err(type_error()),
        }
    }
}

fn is_compatible_enum(left_en: &Enum, right_en: &Enum) -> bool {
    left_en == right_en
        || left_en.alias_enum().map_or(false, |en| &en == right_en)
        || right_en.alias_enum().map_or(false, |en| &en == left_en)
}

fn maybe_paren(res_prec: u32, arg_expr: String, arg_prec: u32) -> String {
    if res_prec >= arg_prec {
        arg_expr
    } else {
        format!("({})", arg_expr)
    }
}

// 1-17: https://en.cppreference.com/w/cpp/language/operator_precedence
const PREC_TERM: u32 = 0;
const PREC_SCOPE: u32 = 1;
const PREC_CALL: u32 = 2;

fn unary_operator_precedence(operator: UnaryOperator) -> u32 {
    use UnaryOperator::*;
    match operator {
        LogicalNot => 3,
        BitwiseNot => 3,
        Minus => 3,
        Plus => 3,
        Typeof | Void | Delete => u32::MAX, // unsupported
    }
}

fn binary_operator_precedence(operator: BinaryOperator) -> u32 {
    use BinaryOperator::*;
    match operator {
        LogicalAnd => 14,
        LogicalOr => 15,
        RightShift | UnsignedRightShift => 7,
        LeftShift => 7,
        BitwiseAnd => 11,
        BitwiseXor => 12,
        BitwiseOr => 13,
        Add => 6,
        Sub => 6,
        Mul => 5,
        Div => 5,
        Rem => 5,
        Exp => u32::MAX, // unsupported
        Equal | StrictEqual => 10,
        NotEqual | StrictNotEqual => 10,
        LessThan => 9,
        LessThanEqual => 9,
        GreaterThan => 9,
        GreaterThanEqual => 9,
        NullishCoalesce | Instanceof | In => u32::MAX, // unsupported
    }
}

fn unary_operator_str(operator: UnaryOperator) -> &'static str {
    use UnaryOperator::*;
    match operator {
        LogicalNot => "!",
        BitwiseNot => "~",
        Minus => "-",
        Plus => "+",
        Typeof | Void | Delete => "/* unsupported */",
    }
}

fn binary_operator_str(operator: BinaryOperator) -> &'static str {
    use BinaryOperator::*;
    match operator {
        LogicalAnd => "&&",
        LogicalOr => "||",
        RightShift | UnsignedRightShift => ">>",
        LeftShift => "<<",
        BitwiseAnd => "&",
        BitwiseXor => "^",
        BitwiseOr => "|",
        Add => "+",
        Sub => "-",
        Mul => "*",
        Div => "/",
        Rem => "%",
        Exp => "/* unsupported */",
        Equal | StrictEqual => "==",
        NotEqual | StrictNotEqual => "!=",
        LessThan => "<",
        LessThanEqual => "<=",
        GreaterThan => ">",
        GreaterThanEqual => ">=",
        NullishCoalesce | Instanceof | In => "/* unsupported */",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Diagnostics;
    use crate::metatype;
    use crate::qmlast::{UiDocument, UiObjectDefinition, UiProgram};
    use crate::typemap::TypeMap;

    struct Env {
        doc: UiDocument,
        type_map: TypeMap,
    }

    impl Env {
        fn new(expr_source: &str) -> Self {
            let mut type_map = TypeMap::with_primitive_types();
            let mut foo_meta = metatype::Class::new("Foo");
            foo_meta
                .enums
                .push(metatype::Enum::with_values("Bar", ["Bar0", "Bar1", "Bar2"]));
            type_map.extend([foo_meta]);
            Env {
                doc: UiDocument::parse(format!("A {{ a: {expr_source}}}"), None),
                type_map,
            }
        }

        fn node(&self) -> Node {
            let program = UiProgram::from_node(self.doc.root_node()).unwrap();
            let obj = UiObjectDefinition::from_node(program.root_object_node(), self.doc.source())
                .unwrap();
            let map = obj.build_binding_map(self.doc.source()).unwrap();
            map.get("a").unwrap().get_node().unwrap()
        }

        fn format(&self) -> (TypeDesc, String, u32) {
            self.try_format().unwrap()
        }

        fn try_format(&self) -> Result<(TypeDesc, String, u32), Diagnostics> {
            let mut diagnostics = Diagnostics::new();
            let type_space = self.type_map.root();
            let node = self.node();
            typedexpr::walk(
                &type_space,
                node,
                self.doc.source(),
                &ExpressionFormatter,
                &mut diagnostics,
            )
            .ok_or(diagnostics)
        }
    }

    fn format_expr(expr_source: &str) -> String {
        let (_, expr, _) = Env::new(expr_source).format();
        expr
    }

    #[test]
    fn format_flags() {
        assert_eq!(format_expr("Foo.Bar0"), "Foo::Bar0");
        assert_eq!(
            format_expr("Foo.Bar0 | Foo.Bar1 | Foo.Bar2"),
            "Foo::Bar0|Foo::Bar1|Foo::Bar2"
        );
        assert_eq!(format_expr("Foo.Bar0 & ~Foo.Bar1"), "Foo::Bar0&~Foo::Bar1");
    }

    #[test]
    fn format_operator_precedence() {
        assert_eq!(format_expr("1 + (2 * 3) + 4"), "1+2*3+4");
        assert_eq!(format_expr("((1 + 2) * 3) + 4"), "(1+2)*3+4");
        assert_eq!(format_expr("1 + (2 * (3 + 4))"), "1+2*(3+4)");
        assert_eq!(format_expr("+(-1)"), "+-1");
        assert_eq!(format_expr("+((-1) + 1)"), "+(-1+1)");
    }

    #[test]
    fn format_operator_str() {
        assert_eq!(format_expr("1 === 1"), "1==1");
        assert_eq!(format_expr("1 !== 2"), "1!=2");
        assert_eq!(format_expr("'foo' + 'bar'"), r#"QString("foo")+"bar""#);
    }
}
