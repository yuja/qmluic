use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::qmlast::{
    BinaryOperator, Expression, Identifier, Node, ParseError, ParseErrorKind, UnaryOperator,
};
use crate::typemap::{PrimitiveType, Type};
use std::fmt;
use std::io;

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
    pub fn from_expression<'tree>(
        ty: &Type,
        node: Node<'tree>,
        source: &str,
        diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
    ) -> Option<Self> {
        match format_constant_value(ty, node, source) {
            Ok(v) => Some(v),
            Err(e) => {
                diagnostics.push(e);
                None
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
