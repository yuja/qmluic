use super::gadget::{ConstantGadget, ConstantSizePolicy};
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{
    BinaryOperator, Expression, Identifier, Node, ParseError, ParseErrorKind, UiBindingMap,
    UiBindingValue, UnaryOperator,
};
use crate::typedexpr::{self, DescribeType, ExpressionVisitor, TypeDesc};
use crate::typemap::{Class, Enum, PrimitiveType, Type, TypeSpace};
use std::fmt;
use std::io;

/// Variant for the constant expressions which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub enum ConstantExpression {
    Value(ConstantValue),
    Gadget(ConstantGadget),
    SizePolicy(ConstantSizePolicy),
}

impl ConstantExpression {
    /// Generates constant expression of `ty` type from the given `binding_value`.
    pub fn from_binding_value<'a, P>(
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        ty: &Type,
        binding_value: &UiBindingValue,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self>
    where
        P: TypeSpace<'a>,
    {
        match binding_value {
            UiBindingValue::Node(n) => {
                Self::from_expression(parent_space, ty, *n, source, diagnostics)
            }
            UiBindingValue::Map(n, m) => match ty {
                Type::Class(cls) => Self::from_binding_map(cls, *n, m, source, diagnostics),
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
    pub fn from_binding_map(
        cls: &Class,
        node: Node,
        binding_map: &UiBindingMap,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match cls.name() {
            "QSizePolicy" => {
                ConstantSizePolicy::from_binding_map(cls, node, binding_map, source, diagnostics)
                    .map(ConstantExpression::SizePolicy)
            }
            _ => ConstantGadget::from_binding_map(cls, node, binding_map, source, diagnostics)
                .map(ConstantExpression::Gadget),
        }
    }

    /// Generates constant expression of `ty` type from the given expression `node`.
    pub fn from_expression<'a, P>(
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        ty: &Type,
        node: Node,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self>
    where
        P: TypeSpace<'a>,
    {
        ConstantValue::from_expression(parent_space, ty, node, source, diagnostics)
            .map(ConstantExpression::Value)
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use ConstantExpression::*;
        match self {
            Value(x) => x.serialize_to_xml(writer),
            Gadget(x) => x.serialize_to_xml(writer),
            SizePolicy(x) => x.serialize_to_xml(writer),
        }
    }
}

/// Constant expression which can be serialized to UI XML as a simple tagged value.
#[derive(Clone, Debug)]
pub enum ConstantValue {
    Bool(bool),
    Number(f64),
    String(String),
    Enum(String),
    Set(String),
}

impl ConstantValue {
    /// Generates value of `ty` type from the given expression `node`.
    pub fn from_expression<'a, P>(
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        ty: &Type,
        node: Node,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self>
    where
        P: TypeSpace<'a>,
    {
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
                let (res_t, res_expr) = typedexpr::walk(
                    parent_space,
                    node,
                    source,
                    &ExpressionFormatter,
                    diagnostics,
                )?;
                match &res_t {
                    TypeDesc::Enum(res_en) if is_compatible_enum(res_en, en) => {
                        if en.is_flag() {
                            // try to format x|y|z without parens for the Qt Designer compatibility
                            let set_expr =
                                format_as_identifier_set(node, source).unwrap_or(res_expr);
                            Some(ConstantValue::Set(set_expr))
                        } else {
                            Some(ConstantValue::Enum(res_expr))
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
                    parent_space,
                    node,
                    source,
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
                    EvaluatedValue::Bool(v) => Some(ConstantValue::Bool(v)),
                    EvaluatedValue::Number(v) => Some(ConstantValue::Number(v)),
                    EvaluatedValue::String(v) => Some(ConstantValue::String(v)),
                }
            }
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use ConstantValue::*;
        match self {
            Bool(_) => xmlutil::write_tagged_str(writer, "bool", self.to_string()),
            Number(_) => xmlutil::write_tagged_str(writer, "number", self.to_string()),
            String(_) => xmlutil::write_tagged_str(writer, "string", self.to_string()),
            Enum(_) => xmlutil::write_tagged_str(writer, "enum", self.to_string()),
            Set(_) => xmlutil::write_tagged_str(writer, "set", self.to_string()),
        }
    }
}

impl fmt::Display for ConstantValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ConstantValue::*;
        match self {
            Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Number(d) => write!(f, "{}", d),
            String(s) | Enum(s) | Set(s) => write!(f, "{}", s),
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

fn format_constant_value<'tree>(
    ty: &Type,
    node: Node<'tree>,
    source: &str,
) -> Result<ConstantValue, ParseError<'tree>> {
    let value = match ty {
        Type::Class(_) => return Err(unexpected_node(node)),
        Type::Enum(en) => {
            // TODO: check if the identifier is a constant in that context.
            // try to format x|y|z without parens for the Qt Designer compatibility
            let expr = format_as_identifier_set(node, source)
                .or_else(|_| format_as_expression(node, source))?;
            if en.is_flag() {
                ConstantValue::Set(expr)
            } else {
                ConstantValue::Enum(expr)
            }
        }
        Type::Namespace(_) => return Err(unexpected_node(node)),
        Type::Primitive(PrimitiveType::Bool) => {
            // TODO: handle values that can be evaluated as bool
            match Expression::from_node(node, source)? {
                Expression::Bool(b) => ConstantValue::Bool(b),
                _ => return Err(unexpected_node(node)),
            }
        }
        Type::Primitive(PrimitiveType::Int | PrimitiveType::QReal | PrimitiveType::UInt) => {
            ConstantValue::Number(eval_number(node, source)?)
        }
        Type::Primitive(PrimitiveType::QString) => {
            ConstantValue::String(parse_as_string(node, source)?)
        }
        Type::Primitive(PrimitiveType::Void) => return Err(unexpected_node(node)),
    };
    Ok(value)
}

/// Formats node as an constant expression without evaluation.
///
/// Here we don't strictly follow the JavaScript language model, but try 1:1 mapping.
fn format_as_expression<'tree>(
    node: Node<'tree>,
    source: &str,
) -> Result<String, ParseError<'tree>> {
    let formatted = match Expression::from_node(node, source)? {
        Expression::Identifier(_) => format_as_nested_identifier(node, source)?,
        Expression::Number(v) => v.to_string(),
        Expression::String(s) => format!("{:?}", s), // TODO: escape per C spec
        Expression::Bool(false) => "false".to_owned(),
        Expression::Bool(true) => "true".to_owned(),
        Expression::Array(_) => return Err(unexpected_node(node)), // TODO
        Expression::MemberExpression(_) => format_as_nested_identifier(node, source)?,
        Expression::CallExpression(_) => return Err(unexpected_node(node)), // TODO
        Expression::UnaryExpression(x) => {
            let arg = format_as_expression(x.argument, source)?;
            match x.operator {
                UnaryOperator::LogicalNot
                | UnaryOperator::BitwiseNot
                | UnaryOperator::Minus
                | UnaryOperator::Plus => {
                    format!("{}({})", x.operator, arg)
                }
                UnaryOperator::Typeof | UnaryOperator::Void | UnaryOperator::Delete => {
                    return Err(unexpected_node(node))
                }
            }
        }
        Expression::BinaryExpression(x) => {
            let left = format_as_expression(x.left, source)?;
            let right = format_as_expression(x.right, source)?;
            match x.operator {
                BinaryOperator::LogicalAnd
                | BinaryOperator::LogicalOr
                | BinaryOperator::RightShift
                | BinaryOperator::LeftShift
                | BinaryOperator::BitwiseAnd
                | BinaryOperator::BitwiseXor
                | BinaryOperator::BitwiseOr
                | BinaryOperator::Add
                | BinaryOperator::Sub
                | BinaryOperator::Mul
                | BinaryOperator::Div
                | BinaryOperator::Rem
                | BinaryOperator::Equal
                | BinaryOperator::NotEqual
                | BinaryOperator::LessThan
                | BinaryOperator::LessThanEqual
                | BinaryOperator::GreaterThan
                | BinaryOperator::GreaterThanEqual => {
                    format!("({}){}({})", left, x.operator, right)
                }
                BinaryOperator::StrictEqual => format!("({}){}({})", left, "==", right),
                BinaryOperator::StrictNotEqual => format!("({}){}({})", left, "!=", right),
                BinaryOperator::UnsignedRightShift
                | BinaryOperator::Exp
                | BinaryOperator::NullishCoalesce
                | BinaryOperator::Instanceof
                | BinaryOperator::In => return Err(unexpected_node(node)), // TODO
            }
        }
    };
    Ok(formatted)
}

fn format_as_identifier_set<'tree>(
    node: Node<'tree>,
    source: &str,
) -> Result<String, ParseError<'tree>> {
    match Expression::from_node(node, source)? {
        Expression::Identifier(n) => Ok(n.to_str(source).to_owned()),
        Expression::MemberExpression(_) => format_as_nested_identifier(node, source),
        Expression::BinaryExpression(x) if x.operator == BinaryOperator::BitwiseOr => {
            let left = format_as_identifier_set(x.left, source)?;
            let right = format_as_identifier_set(x.right, source)?;
            Ok(format!("{}|{}", left, right))
        }
        _ => Err(unexpected_node(node)),
    }
}

fn format_as_nested_identifier<'tree>(
    node: Node<'tree>,
    source: &str,
) -> Result<String, ParseError<'tree>> {
    match Expression::from_node(node, source)? {
        Expression::Identifier(n) => Ok(n.to_str(source).to_owned()),
        Expression::MemberExpression(x) => {
            let object = format_as_nested_identifier(x.object, source)?;
            Ok(format!("{}::{}", object, x.property.to_str(source)))
        }
        _ => Err(unexpected_node(node)),
    }
}

fn parse_as_identifier<'tree>(
    node: Node<'tree>,
    source: &str,
) -> Result<Identifier<'tree>, ParseError<'tree>> {
    match Expression::from_node(node, source)? {
        Expression::Identifier(n) => Ok(n),
        _ => Err(unexpected_node(node)),
    }
}

fn parse_as_string<'tree>(node: Node<'tree>, source: &str) -> Result<String, ParseError<'tree>> {
    let s = match Expression::from_node(node, source)? {
        Expression::CallExpression(x) => {
            match parse_as_identifier(x.function, source)?.to_str(source) {
                "qsTr" if x.arguments.len() == 1 => parse_as_string(x.arguments[0], source)?,
                _ => return Err(unexpected_node(node)),
            }
        }
        Expression::String(s) => s,
        _ => return Err(unexpected_node(node)),
    };
    Ok(s)
}

fn eval_number<'tree>(node: Node<'tree>, source: &str) -> Result<f64, ParseError<'tree>> {
    // TODO: handle overflow, etc.
    let v = match Expression::from_node(node, source)? {
        Expression::Number(v) => v,
        Expression::UnaryExpression(x) => {
            let arg = eval_number(x.argument, source);
            match x.operator {
                UnaryOperator::Minus => -arg?,
                UnaryOperator::Plus => arg?,
                // TODO: ...
                _ => return Err(unexpected_node(node)),
            }
        }
        Expression::BinaryExpression(x) => {
            let left = eval_number(x.left, source);
            let right = eval_number(x.right, source);
            match x.operator {
                BinaryOperator::Add => left? + right?,
                BinaryOperator::Sub => left? - right?,
                BinaryOperator::Mul => left? * right?,
                BinaryOperator::Div => left? / right?,
                BinaryOperator::Exp => left?.powf(right?),
                // TODO: ...
                _ => return Err(unexpected_node(node)),
            }
        }
        _ => return Err(unexpected_node(node)),
    };
    Ok(v)
}

fn unexpected_node(node: Node) -> ParseError {
    ParseError::new(node, ParseErrorKind::UnexpectedNodeKind)
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
}

impl DescribeType<'_> for EvaluatedValue {
    fn type_desc(&self) -> TypeDesc<'static> {
        match self {
            EvaluatedValue::Bool(_) => TypeDesc::Bool,
            EvaluatedValue::Number(_) => TypeDesc::Number,
            EvaluatedValue::String(_) => TypeDesc::String,
        }
    }
}

impl<'a> ExpressionVisitor<'a> for ExpressionEvaluator {
    type Item = EvaluatedValue;

    fn visit_number(&self, value: f64) -> Option<Self::Item> {
        Some(EvaluatedValue::Number(value))
    }

    fn visit_string(&self, value: String) -> Option<Self::Item> {
        Some(EvaluatedValue::String(value))
    }

    fn visit_bool(&self, value: bool) -> Option<Self::Item> {
        Some(EvaluatedValue::Bool(value))
    }

    fn visit_enum(&self, _enum_ty: Enum<'a>, _variant: &str) -> Option<Self::Item> {
        None // enum value is unknown
    }

    fn visit_call_expression(
        &self,
        function: &str,
        mut arguments: Vec<Self::Item>,
    ) -> Option<Self::Item> {
        match function {
            "qsTr" if arguments.len() == 1 => arguments.pop(),
            _ => None,
        }
    }

    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        argument: Self::Item,
    ) -> Option<Self::Item> {
        use UnaryOperator::*;
        match argument {
            EvaluatedValue::Bool(a) => match operator {
                LogicalNot => Some(EvaluatedValue::Bool(!a)),
                BitwiseNot => None,
                Minus | Plus => None,
                Typeof | Void | Delete => None,
            },
            EvaluatedValue::Number(a) => match operator {
                // TODO: handle overflow, etc.
                LogicalNot => None, // TODO: !
                BitwiseNot => None, // TODO: ~
                Minus => Some(EvaluatedValue::Number(-a)),
                Plus => Some(EvaluatedValue::Number(a)),
                Typeof | Void | Delete => None,
            },
            EvaluatedValue::String(_) => None,
        }
    }

    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
    ) -> Option<Self::Item> {
        use BinaryOperator::*;
        match (left, right) {
            #[allow(clippy::bool_comparison)]
            (EvaluatedValue::Bool(l), EvaluatedValue::Bool(r)) => match operator {
                LogicalAnd => Some(EvaluatedValue::Bool(l && r)),
                LogicalOr => Some(EvaluatedValue::Bool(l || r)),
                RightShift | UnsignedRightShift | LeftShift => None,
                BitwiseAnd => Some(EvaluatedValue::Bool(l & r)),
                BitwiseXor => Some(EvaluatedValue::Bool(l ^ r)),
                BitwiseOr => Some(EvaluatedValue::Bool(l | r)),
                Add | Sub | Mul | Div | Rem | Exp => None,
                Equal => Some(EvaluatedValue::Bool(l == r)),
                StrictEqual => Some(EvaluatedValue::Bool(l == r)),
                NotEqual => Some(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Some(EvaluatedValue::Bool(l != r)),
                LessThan => Some(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Some(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Some(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Some(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce | Instanceof | In => None,
            },
            (EvaluatedValue::Number(l), EvaluatedValue::Number(r)) => match operator {
                // TODO: handle overflow, etc.
                LogicalAnd | LogicalOr => None,
                // TODO: >>, (unsigned)>>, <<
                RightShift => None,
                UnsignedRightShift => None,
                LeftShift => None,
                // TODO: &, ^, |
                BitwiseAnd => None,
                BitwiseXor => None,
                BitwiseOr => None,
                Add => Some(EvaluatedValue::Number(l + r)),
                Sub => Some(EvaluatedValue::Number(l - r)),
                Mul => Some(EvaluatedValue::Number(l * r)),
                Div => Some(EvaluatedValue::Number(l / r)),
                Rem => Some(EvaluatedValue::Number(l % r)),
                Exp => Some(EvaluatedValue::Number(l.powf(r))),
                Equal => Some(EvaluatedValue::Bool(l == r)),
                StrictEqual => Some(EvaluatedValue::Bool(l == r)),
                NotEqual => Some(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Some(EvaluatedValue::Bool(l != r)),
                LessThan => Some(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Some(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Some(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Some(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce => None,
                Instanceof => None,
                In => None,
            },
            (EvaluatedValue::String(l), EvaluatedValue::String(r)) => match operator {
                LogicalAnd | LogicalOr => None,
                RightShift | UnsignedRightShift | LeftShift => None,
                BitwiseAnd | BitwiseXor | BitwiseOr => None,
                Add => Some(EvaluatedValue::String(format!("{l}{r}"))),
                Sub | Mul | Div | Rem | Exp => None,
                Equal => Some(EvaluatedValue::Bool(l == r)),
                StrictEqual => Some(EvaluatedValue::Bool(l == r)),
                NotEqual => Some(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Some(EvaluatedValue::Bool(l != r)),
                LessThan => Some(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Some(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Some(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Some(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce => None,
                Instanceof => None,
                In => None,
            },
            _ => None,
        }
    }
}

/// Formats expression tree as arbitrary constant value expression.
///
/// Here we don't strictly follow the JavaScript language model, but try 1:1 mapping.
#[derive(Debug)]
struct ExpressionFormatter;

impl<'a> DescribeType<'a> for (TypeDesc<'a>, String) {
    fn type_desc(&self) -> TypeDesc<'a> {
        self.0.clone()
    }
}

impl<'a> ExpressionVisitor<'a> for ExpressionFormatter {
    type Item = (TypeDesc<'a>, String);

    fn visit_number(&self, value: f64) -> Option<Self::Item> {
        Some((TypeDesc::Number, value.to_string()))
    }

    fn visit_string(&self, value: String) -> Option<Self::Item> {
        Some((TypeDesc::String, format!("{:?}", value))) // TODO: escape per C spec)
    }

    fn visit_bool(&self, value: bool) -> Option<Self::Item> {
        Some((
            TypeDesc::Bool,
            if value {
                "true".to_owned()
            } else {
                "false".to_owned()
            },
        ))
    }

    fn visit_enum(&self, enum_ty: Enum<'a>, variant: &str) -> Option<Self::Item> {
        let res_expr = enum_ty.qualify_variant_name(variant);
        Some((TypeDesc::Enum(enum_ty), res_expr))
    }

    fn visit_call_expression(
        &self,
        function: &str,
        arguments: Vec<Self::Item>,
    ) -> Option<Self::Item> {
        match function {
            "qsTr" if arguments.len() == 1 => {
                if let (TypeDesc::String, expr) = &arguments[0] {
                    Some((TypeDesc::String, format!("qsTr({})", expr)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        (arg_t, ref arg_expr): Self::Item,
    ) -> Option<Self::Item> {
        use UnaryOperator::*;
        let res_expr = format!("{}({})", operator, arg_expr);
        match arg_t {
            TypeDesc::Bool => match operator {
                LogicalNot => Some((TypeDesc::Bool, res_expr)),
                BitwiseNot => None,
                Minus | Plus => None,
                Typeof | Void | Delete => None,
            },
            TypeDesc::Number => match operator {
                LogicalNot => Some((TypeDesc::Bool, res_expr)),
                BitwiseNot | Minus | Plus => Some((TypeDesc::Number, res_expr)),
                Typeof | Void | Delete => None,
            },
            TypeDesc::String => None,
            TypeDesc::Enum(en) => match operator {
                LogicalNot => Some((TypeDesc::Bool, res_expr)),
                BitwiseNot => Some((TypeDesc::Enum(en), res_expr)),
                Minus | Plus => None,
                Typeof | Void | Delete => None,
            },
        }
    }

    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        (left_t, ref left_expr): Self::Item,
        (right_t, ref right_expr): Self::Item,
    ) -> Option<Self::Item> {
        use BinaryOperator::*;
        match (left_t, right_t) {
            (TypeDesc::Bool, TypeDesc::Bool) => {
                let res_expr = format!("({}){}({})", left_expr, operator, right_expr);
                match operator {
                    LogicalAnd | LogicalOr => Some((TypeDesc::Bool, res_expr)),
                    RightShift | UnsignedRightShift | LeftShift => None,
                    BitwiseAnd | BitwiseXor | BitwiseOr => Some((TypeDesc::Bool, res_expr)),
                    Add | Sub | Mul | Div | Rem | Exp => None,
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => {
                        format_comparison_expression(operator, left_expr, right_expr)
                    }
                    NullishCoalesce | Instanceof | In => None,
                }
            }
            (TypeDesc::Number, TypeDesc::Number) => {
                let res_expr = format!("({}){}({})", left_expr, operator, right_expr);
                match operator {
                    LogicalAnd | LogicalOr => None,
                    // TODO: >>, (unsigned)>>, <<
                    RightShift => None,
                    UnsignedRightShift => None,
                    LeftShift => None,
                    // TODO: &, ^, |
                    BitwiseAnd => None,
                    BitwiseXor => None,
                    BitwiseOr => None,
                    Add | Sub | Mul | Div | Rem => Some((TypeDesc::Number, res_expr)),
                    Exp => None, // TODO
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => {
                        format_comparison_expression(operator, left_expr, right_expr)
                    }
                    NullishCoalesce | Instanceof | In => None,
                }
            }
            (TypeDesc::String, TypeDesc::String) => {
                let res_expr = format!("(QString({})){}({})", left_expr, operator, right_expr);
                match operator {
                    LogicalAnd | LogicalOr => None,
                    RightShift | UnsignedRightShift | LeftShift => None,
                    BitwiseAnd | BitwiseXor | BitwiseOr => None,
                    Add => Some((TypeDesc::String, res_expr)),
                    Sub | Mul | Div | Rem | Exp => None,
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => format_comparison_expression(
                        operator,
                        &format!("QString({})", left_expr),
                        right_expr,
                    ),
                    NullishCoalesce | Instanceof | In => None,
                }
            }
            (TypeDesc::Enum(left_en), TypeDesc::Enum(right_en))
                if is_compatible_enum(&left_en, &right_en) =>
            {
                let res_expr = format!("({}){}({})", left_expr, operator, right_expr);
                match operator {
                    LogicalAnd | LogicalOr => Some((TypeDesc::Bool, res_expr)),
                    RightShift | UnsignedRightShift | LeftShift => None,
                    BitwiseAnd | BitwiseXor | BitwiseOr => {
                        Some((TypeDesc::Enum(left_en), res_expr))
                    }
                    Add | Sub | Mul | Div | Rem | Exp => None,
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => {
                        format_comparison_expression(operator, left_expr, right_expr)
                    }
                    NullishCoalesce | Instanceof | In => None,
                }
            }
            _ => None,
        }
    }
}

fn format_comparison_expression(
    operator: BinaryOperator,
    left_expr: &str,
    right_expr: &str,
) -> Option<(TypeDesc<'static>, String)> {
    use BinaryOperator::*;
    match operator {
        Equal | NotEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual => Some((
            TypeDesc::Bool,
            format!("({}){}({})", left_expr, operator, right_expr),
        )),
        StrictEqual => Some((
            TypeDesc::Bool,
            format!("({}){}({})", left_expr, "==", right_expr),
        )),
        StrictNotEqual => Some((
            TypeDesc::Bool,
            format!("({}){}({})", left_expr, "!=", right_expr),
        )),
        _ => panic!("unhandled binary op: {operator}"),
    }
}

fn is_compatible_enum(left_en: &Enum, right_en: &Enum) -> bool {
    left_en == right_en
        || left_en.alias_enum().map_or(false, |en| &en == right_en)
        || right_en.alias_enum().map_or(false, |en| &en == left_en)
}
