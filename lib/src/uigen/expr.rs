use super::gadget::{Gadget, SizePolicy};
use super::xmlutil;
use super::{BuildDocContext, XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{BinaryOperator, Node, UiBindingMap, UiBindingValue, UnaryOperator};
use crate::typedexpr::{self, DescribeType, ExpressionVisitor, TypeDesc};
use crate::typemap::{Class, Enum, NamedType, PrimitiveType, TypeKind, TypeSpace};
use quick_xml::events::{BytesStart, BytesText, Event};
use std::fmt;
use std::io;
use thiserror::Error;

/// Variant for the property values that can or cannot be serialized to UI XML.
#[derive(Clone, Debug)]
pub(super) enum PropertyValue {
    Serializable(Value),
}

impl PropertyValue {
    /// Generates constant expression of `ty` type from the given `binding_value`.
    pub(super) fn from_binding_value(
        ctx: &BuildDocContext,
        ty: &TypeKind,
        binding_value: &UiBindingValue,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match binding_value {
            UiBindingValue::Node(n) => match ty {
                TypeKind::Just(t) => SimpleValue::from_expression(ctx, t, *n, diagnostics)
                    .map(|v| PropertyValue::Serializable(Value::Simple(v))),
                TypeKind::Pointer(NamedType::Class(cls)) => {
                    parse_object_reference(ctx, cls, *n, diagnostics)
                        .map(|v| PropertyValue::Serializable(Value::Simple(v)))
                }
                TypeKind::PointerList(NamedType::Class(cls)) => {
                    parse_object_reference_list(ctx, cls, *n, diagnostics)
                        .map(|v| PropertyValue::Serializable(Value::CstringList(v)))
                }
                TypeKind::Pointer(_) | TypeKind::PointerList(_) => {
                    diagnostics.push(Diagnostic::error(
                        n.byte_range(),
                        format!(
                            "unsupported value expression of type '{}'",
                            ty.qualified_cxx_name(),
                        ),
                    ));
                    None
                }
            },
            UiBindingValue::Map(n, m) => match ty {
                TypeKind::Just(NamedType::Class(cls)) => {
                    Self::from_binding_map(ctx, cls, *n, m, diagnostics)
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        n.byte_range(),
                        format!(
                            "binding map cannot be parsed as non-class type '{}'",
                            ty.qualified_cxx_name()
                        ),
                    ));
                    None
                }
            },
        }
    }

    /// Generates constant expression of `cls` type from the given `binding_map`.
    fn from_binding_map(
        ctx: &BuildDocContext,
        cls: &Class,
        node: Node,
        binding_map: &UiBindingMap,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match cls.name() {
            "QSizePolicy" => {
                let policy = SizePolicy::from_binding_map(ctx, cls, binding_map, diagnostics);
                Some(PropertyValue::Serializable(Value::SizePolicy(policy)))
            }
            _ => Gadget::from_binding_map(ctx, cls, node, binding_map, diagnostics)
                .map(|v| PropertyValue::Serializable(Value::Gadget(v))),
        }
    }
}

/// Variant for the constant expressions which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub enum Value {
    Simple(SimpleValue),
    CstringList(Vec<String>),
    Gadget(Gadget),
    SizePolicy(SizePolicy),
}

impl Value {
    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use Value::*;
        match self {
            Simple(x) => x.serialize_to_xml(writer),
            // TODO: if we add support for <stringlist/>, maybe better to map CstringList to it
            CstringList(_) => panic!("CstringList cannot be serialized"),
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
            // TODO: if we add support for <stringlist/>, maybe better to map CstringList to it
            CstringList(_) => panic!("CstringList cannot be serialized"),
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
    Cstring(String),
    Enum(String),
    Set(String),
}

impl SimpleValue {
    /// Generates value of `ty` type from the given expression `node`.
    fn from_expression(
        ctx: &BuildDocContext,
        ty: &NamedType,
        node: Node,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match ty {
            NamedType::Class(_) | NamedType::QmlComponent(_) => {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!(
                        "unsupported constant value expression: class '{}'",
                        ty.qualified_cxx_name(),
                    ),
                ));
                None
            }
            NamedType::Enum(en) => {
                let (res_t, res_expr, _) = typedexpr::walk(
                    &ctx.type_space,
                    &ctx.object_tree,
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
                                ty.qualified_cxx_name(),
                                res_t.qualified_name()
                            ),
                        ));
                        None
                    }
                }
            }
            NamedType::Namespace(_) => {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!(
                        "unsupported constant value expression: namespace '{}'",
                        ty.qualified_cxx_name(),
                    ),
                ));
                None
            }
            NamedType::Primitive(p) => {
                let res = typedexpr::walk(
                    &ctx.type_space,
                    &ctx.object_tree,
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
                            ty.qualified_cxx_name(),
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
            Cstring(_) => "cstring",
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
            String { s, .. } | Cstring(s) | Enum(s) | Set(s) => write!(f, "{}", s),
        }
    }
}

fn parse_object_reference(
    ctx: &BuildDocContext,
    expected_cls: &Class,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<SimpleValue> {
    let (res_cls, obj_ref) = typedexpr::walk(
        &ctx.type_space,
        &ctx.object_tree,
        node,
        ctx.source,
        &ObjectRefCollector,
        diagnostics,
    )?;
    match obj_ref {
        ObjectRef::Just(name) if res_cls.is_derived_from(expected_cls) => {
            Some(SimpleValue::Cstring(name))
        }
        ObjectRef::Just(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "reference type mismatch (expected: {}, actual: {})",
                    expected_cls.qualified_cxx_name(),
                    res_cls.qualified_cxx_name()
                ),
            ));
            None
        }
        ObjectRef::List(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "reference type mismatch (expected: {}, actual: list)",
                    expected_cls.qualified_cxx_name(),
                ),
            ));
            None
        }
    }
}

fn parse_object_reference_list(
    ctx: &BuildDocContext,
    expected_cls: &Class,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<Vec<String>> {
    let (res_cls, obj_ref) = typedexpr::walk(
        &ctx.type_space,
        &ctx.object_tree,
        node,
        ctx.source,
        &ObjectRefCollector,
        diagnostics,
    )?;
    match obj_ref {
        ObjectRef::Just(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "reference type mismatch (expected: list, actual: {})",
                    res_cls.qualified_cxx_name(),
                ),
            ));
            None
        }
        ObjectRef::List(names) if res_cls.is_derived_from(expected_cls) => Some(names),
        ObjectRef::List(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "element type mismatch (expected: {}, actual: {})",
                    expected_cls.qualified_cxx_name(),
                    res_cls.qualified_cxx_name()
                ),
            ));
            None
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
    #[error("unsupported reference")]
    UnsupportedReference,
    #[error("unsupported function call")]
    UnsupportedFunctionCall,
    #[error("unsupported unary operation on type: {0} <{1}>")]
    UnsupportedUnaryOperationOnType(UnaryOperator, String),
    #[error("unsupported binary operation on types: <{1}> {0} <{2}>")]
    UnsupportedBinaryOperationOnType(BinaryOperator, String, String),
    #[error("cannot evaluate as constant")]
    CannotEvaluateAsConstant,
    #[error("cannot deduce type from '{0}' and '{1}'")]
    CannotDeduceType(String, String),
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

    fn visit_array(&self, _elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("array"))
    }

    fn visit_object_ref(&self, _cls: Class<'a>, _name: &str) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedReference)
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
            move || {
                ExpressionError::UnsupportedUnaryOperationOnType(
                    operator,
                    arg_t.qualified_name().into(),
                )
            }
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
            EvaluatedValue::TrString(_) => Err(ExpressionError::CannotEvaluateAsConstant),
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
                    operator,
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
            (EvaluatedValue::TrString(_), _) => Err(ExpressionError::CannotEvaluateAsConstant),
            (_, EvaluatedValue::TrString(_)) => Err(ExpressionError::CannotEvaluateAsConstant),
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
        let res_expr = enum_ty.qualify_cxx_variant_name(variant);
        Ok((TypeDesc::Enum(enum_ty), res_expr, PREC_SCOPE))
    }

    fn visit_array(&self, _elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("array"))
    }

    fn visit_object_ref(&self, cls: Class<'a>, name: &str) -> Result<Self::Item, Self::Error> {
        Ok((TypeDesc::ObjectRef(cls), name.to_owned(), PREC_TERM))
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
            move || {
                ExpressionError::UnsupportedUnaryOperationOnType(
                    operator,
                    arg_t.qualified_name().into(),
                )
            }
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
            TypeDesc::ObjectRef(_) | TypeDesc::ObjectRefList(_) => Err(type_error()),
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
                    operator,
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

/// Collects object references from identifier or array expression.
#[derive(Debug)]
struct ObjectRefCollector;

#[derive(Clone, Debug)]
enum ObjectRef {
    Just(String),
    List(Vec<String>),
}

impl<'a> DescribeType<'a> for (Class<'a>, ObjectRef) {
    fn type_desc(&self) -> TypeDesc<'a> {
        match self {
            (cls, ObjectRef::Just(_)) => TypeDesc::ObjectRef(cls.clone()),
            (cls, ObjectRef::List(_)) => TypeDesc::ObjectRefList(cls.clone()),
        }
    }
}

impl<'a> ExpressionVisitor<'a> for ObjectRefCollector {
    type Item = (Class<'a>, ObjectRef);
    type Error = ExpressionError;

    fn visit_number(&self, _value: f64) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("number"))
    }

    fn visit_string(&self, _value: String) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("string"))
    }

    fn visit_bool(&self, _value: bool) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("bool"))
    }

    fn visit_enum(&self, _enum_ty: Enum<'a>, _variant: &str) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("enum"))
    }

    fn visit_array(&self, elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error> {
        if let Some((cls, _)) = elements.first() {
            let mut base_cls = cls.clone();
            let mut names = Vec::with_capacity(elements.len());
            for (cls, r) in elements {
                match r {
                    ObjectRef::Just(s) => {
                        if base_cls != cls {
                            base_cls = base_cls.common_base_class(&cls).ok_or_else(|| {
                                ExpressionError::CannotDeduceType(
                                    base_cls.qualified_cxx_name().into(),
                                    cls.qualified_cxx_name().into(),
                                )
                            })?;
                        }
                        names.push(s);
                    }
                    ObjectRef::List(_) => {
                        return Err(ExpressionError::UnsupportedLiteral("nested array"));
                    }
                }
            }
            Ok((base_cls, ObjectRef::List(names)))
        } else {
            Err(ExpressionError::UnsupportedLiteral("empty array"))
        }
    }

    fn visit_object_ref(&self, cls: Class<'a>, name: &str) -> Result<Self::Item, Self::Error> {
        Ok((cls, ObjectRef::Just(name.to_owned())))
    }

    fn visit_call_expression(
        &self,
        _function: &str,
        _arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedFunctionCall)
    }

    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        argument: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedUnaryOperationOnType(
            operator,
            argument.type_desc().qualified_name().into(),
        ))
    }

    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedBinaryOperationOnType(
            operator,
            left.type_desc().qualified_name().into(),
            right.type_desc().qualified_name().into(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Diagnostics;
    use crate::metatype;
    use crate::objtree::ObjectTree;
    use crate::qmlast::{UiObjectDefinition, UiProgram};
    use crate::qmldoc::UiDocument;
    use crate::typemap::{ModuleData, ModuleId, TypeMap};

    struct Env {
        doc: UiDocument,
        type_map: TypeMap,
        module_id: ModuleId<'static>,
    }

    impl Env {
        fn new(expr_source: &str) -> Self {
            let mut type_map = TypeMap::with_primitive_types();
            let module_id = ModuleId::Named("foo".into());
            type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
            let mut foo_meta = metatype::Class::new("Foo");
            foo_meta
                .enums
                .push(metatype::Enum::with_values("Bar", ["Bar0", "Bar1", "Bar2"]));
            type_map
                .get_module_data_mut(&module_id)
                .unwrap()
                .extend([foo_meta, metatype::Class::new("A")]);
            Env {
                doc: UiDocument::parse(format!("A {{ a: {expr_source}}}"), None),
                type_map,
                module_id,
            }
        }

        fn root_expr_nodes(&self) -> (Node, Node) {
            let program = UiProgram::from_node(self.doc.root_node(), self.doc.source()).unwrap();
            let obj = UiObjectDefinition::from_node(program.root_object_node(), self.doc.source())
                .unwrap();
            let map = obj.build_binding_map(self.doc.source()).unwrap();
            (
                program.root_object_node(),
                map.get("a").unwrap().get_node().unwrap(),
            )
        }

        fn format(&self) -> (TypeDesc, String, u32) {
            self.try_format().unwrap()
        }

        fn try_format(&self) -> Result<(TypeDesc, String, u32), Diagnostics> {
            let mut diagnostics = Diagnostics::new();
            let type_space = self.type_map.get_module(&self.module_id).unwrap();
            let (root_node, node) = self.root_expr_nodes();
            let object_tree =
                ObjectTree::build(root_node, self.doc.source(), &type_space, &mut diagnostics)
                    .unwrap();
            typedexpr::walk(
                &type_space,
                &object_tree,
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
