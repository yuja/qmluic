use super::astutil;
use super::term::Identifier;
use super::{ParseError, ParseErrorKind};
use std::fmt;
use tree_sitter::Node;

/// Tagged union that wraps an expression.
#[derive(Clone, Debug)]
pub enum Expression<'tree, 'source> {
    Identifier(Identifier<'source>),
    Number(f64),
    String(String),
    Bool(bool),
    Array(Vec<Node<'tree>>),
    MemberExpression(MemberExpression<'tree, 'source>),
    CallExpression(CallExpression<'tree>),
    UnaryExpression(UnaryExpression<'tree>),
    BinaryExpression(BinaryExpression<'tree>),
    // TODO: ...
}

impl<'tree, 'source> Expression<'tree, 'source> {
    pub fn from_node(
        mut node: Node<'tree>,
        source: &'source str,
    ) -> Result<Self, ParseError<'tree>> {
        if node.kind() == "expression_statement" {
            node = node
                .named_child(0)
                .ok_or_else(|| ParseError::new(node, ParseErrorKind::InvalidSyntax))?;
        }
        while node.kind() == "parenthesized_expression" {
            node = node
                .named_child(0)
                .ok_or_else(|| ParseError::new(node, ParseErrorKind::InvalidSyntax))?;
        }

        let expr = match node.kind() {
            "identifier" => Expression::Identifier(Identifier::from_node(node, source)?),
            "number" => Expression::Number(astutil::parse_number(node, source)?),
            "string" => Expression::String(astutil::parse_string(node, source)?),
            "true" => Expression::Bool(true),
            "false" => Expression::Bool(false),
            "array" => {
                let items = node
                    .named_children(&mut node.walk())
                    .filter(|n| !(n.is_extra() && !n.is_error()))
                    .collect();
                Expression::Array(items)
            }
            "member_expression" => {
                let object = astutil::get_child_by_field_name(node, "object")?;
                let property = Identifier::from_node(
                    astutil::get_child_by_field_name(node, "property")?,
                    source,
                )?;
                // TODO: <object>?.<property>
                Expression::MemberExpression(MemberExpression { object, property })
            }
            "call_expression" => {
                let mut cursor = node.walk();
                let function = astutil::get_child_by_field_name(node, "function")?;
                let arguments = astutil::get_child_by_field_name(node, "arguments")?
                    .named_children(&mut cursor)
                    .filter(|n| !(n.is_extra() && !n.is_error()))
                    .collect();
                // TODO: <func>?.(<arg>...)
                Expression::CallExpression(CallExpression {
                    function,
                    arguments,
                })
            }
            "unary_expression" => {
                let operator =
                    UnaryOperator::from_node(astutil::get_child_by_field_name(node, "operator")?)?;
                let argument = astutil::get_child_by_field_name(node, "argument")?;
                Expression::UnaryExpression(UnaryExpression { operator, argument })
            }
            "binary_expression" => {
                let operator =
                    BinaryOperator::from_node(astutil::get_child_by_field_name(node, "operator")?)?;
                let left = astutil::get_child_by_field_name(node, "left")?;
                let right = astutil::get_child_by_field_name(node, "right")?;
                Expression::BinaryExpression(BinaryExpression {
                    operator,
                    left,
                    right,
                })
            }
            // TODO: ...
            _ => {
                return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
            }
        };
        Ok(expr)
    }

    pub fn get_identifier(&self) -> Option<Identifier> {
        match self {
            Expression::Identifier(x) => Some(*x),
            _ => None,
        }
    }

    pub fn get_number(&self) -> Option<f64> {
        match self {
            Expression::Number(x) => Some(*x),
            _ => None,
        }
    }

    pub fn get_string(&self) -> Option<&String> {
        match self {
            Expression::String(x) => Some(x),
            _ => None,
        }
    }

    pub fn get_bool(&self) -> Option<bool> {
        match self {
            Expression::Bool(x) => Some(*x),
            _ => None,
        }
    }

    pub fn get_array(&self) -> Option<&[Node<'tree>]> {
        match self {
            Expression::Array(xs) => Some(xs),
            _ => None,
        }
    }
}

/// Represents a member expression.
#[derive(Clone, Debug)]
pub struct MemberExpression<'tree, 'source> {
    pub object: Node<'tree>,
    pub property: Identifier<'source>,
}

/// Represents a call expression.
#[derive(Clone, Debug)]
pub struct CallExpression<'tree> {
    pub function: Node<'tree>,
    pub arguments: Vec<Node<'tree>>,
}

/// Represents a unary expression.
#[derive(Clone, Debug)]
pub struct UnaryExpression<'tree> {
    pub operator: UnaryOperator,
    pub argument: Node<'tree>,
}

/// Unary operation type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryOperator {
    /// `!`
    LogicalNot,
    /// `~`
    BitwiseNot,
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `typeof`
    Typeof,
    /// `void`
    Void,
    /// `delete`
    Delete,
}

impl UnaryOperator {
    pub fn from_node<'tree>(node: Node<'tree>) -> Result<Self, ParseError<'tree>> {
        use UnaryOperator::*;
        if node.is_named() {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }
        let op = match node.kind() {
            "!" => LogicalNot,
            "~" => BitwiseNot,
            "-" => Minus,
            "+" => Plus,
            "typeof" => Typeof,
            "void" => Void,
            "delete" => Delete,
            _ => return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind)),
        };
        Ok(op)
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnaryOperator::*;
        let s = match self {
            LogicalNot => "!",
            BitwiseNot => "~",
            Minus => "-",
            Plus => "+",
            Typeof => "typeof",
            Void => "void",
            Delete => "delete",
        };
        write!(f, "{}", s)
    }
}

/// Represents a binary expression.
#[derive(Clone, Debug)]
pub struct BinaryExpression<'tree> {
    pub operator: BinaryOperator,
    pub left: Node<'tree>,
    pub right: Node<'tree>,
}

/// Binary operation type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BinaryOperator {
    /// `&&`
    LogicalAnd,
    /// `||`
    LogicalOr,
    /// `>>`
    RightShift,
    /// `>>>`
    UnsignedRightShift,
    /// `<<`
    LeftShift,
    /// `&`
    BitwiseAnd,
    /// `^`
    BitwiseXor,
    /// `|`
    BitwiseOr,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
    /// `**`
    Exp,
    /// `==`
    Equal,
    /// `===`
    StrictEqual,
    /// `!=`
    NotEqual,
    /// `!==`
    StrictNotEqual,
    /// `<`
    LessThan,
    /// `<=`
    LessThanEqual,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanEqual,
    /// `??`
    NullishCoalesce,
    /// `instanceof`
    Instanceof,
    /// `in`
    In,
}

impl BinaryOperator {
    pub fn from_node<'tree>(node: Node<'tree>) -> Result<Self, ParseError<'tree>> {
        use BinaryOperator::*;
        if node.is_named() {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }
        let op = match node.kind() {
            "&&" => LogicalAnd,
            "||" => LogicalOr,
            ">>" => RightShift,
            ">>>" => UnsignedRightShift,
            "<<" => LeftShift,
            "&" => BitwiseAnd,
            "^" => BitwiseXor,
            "|" => BitwiseOr,
            "+" => Add,
            "-" => Sub,
            "*" => Mul,
            "/" => Div,
            "%" => Rem,
            "**" => Exp,
            "==" => Equal,
            "===" => StrictEqual,
            "!=" => NotEqual,
            "!==" => StrictNotEqual,
            "<" => LessThan,
            "<=" => LessThanEqual,
            ">" => GreaterThan,
            ">=" => GreaterThanEqual,
            "??" => NullishCoalesce,
            "instanceof" => Instanceof,
            "in" => In,
            _ => return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind)),
        };
        Ok(op)
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryOperator::*;
        let s = match self {
            LogicalAnd => "&&",
            LogicalOr => "||",
            RightShift => ">>",
            UnsignedRightShift => ">>>",
            LeftShift => "<<",
            BitwiseAnd => "&",
            BitwiseXor => "^",
            BitwiseOr => "|",
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Rem => "%",
            Exp => "**",
            Equal => "==",
            StrictEqual => "===",
            NotEqual => "!=",
            StrictNotEqual => "!==",
            LessThan => "<",
            LessThanEqual => "<=",
            GreaterThan => ">",
            GreaterThanEqual => ">=",
            NullishCoalesce => "??",
            Instanceof => "instanceof",
            In => "in",
        };
        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::qmlast::{UiDocument, UiObjectDefinition, UiProgram};

    fn parse(source: &str) -> UiDocument {
        UiDocument::parse(source.to_owned(), None)
    }

    fn extract_expr<'a>(
        doc: &'a UiDocument,
        name: &str,
    ) -> Result<Expression<'a, 'a>, ParseError<'a>> {
        let program = UiProgram::from_node(doc.root_node()).unwrap();
        let obj = UiObjectDefinition::from_node(program.root_object_node(), doc.source()).unwrap();
        let map = obj.build_binding_map(doc.source()).unwrap();
        let node = map.get(&Identifier::new(name)).unwrap().get_node().unwrap();
        Expression::from_node(node, doc.source())
    }

    #[test]
    fn trivial_expressions() {
        let doc = parse(
            r###"
            Foo {
                identifier: foo
                number_: 123
                string_: "whatever"
                escaped_string: "foo\nbar"
                true_: true
                false_: false
                array: [0, 1, /*garbage*/ 2]
                paren1: (foo)
                paren2: ((foo))
            }
            "###,
        );
        assert_eq!(
            extract_expr(&doc, "identifier")
                .unwrap()
                .get_identifier()
                .unwrap(),
            Identifier::new("foo")
        );
        assert_eq!(
            extract_expr(&doc, "number_").unwrap().get_number().unwrap(),
            123.
        );
        assert_eq!(
            extract_expr(&doc, "string_").unwrap().get_string().unwrap(),
            "whatever"
        );
        assert_eq!(
            extract_expr(&doc, "escaped_string")
                .unwrap()
                .get_string()
                .unwrap(),
            "foo\nbar"
        );
        assert_eq!(
            extract_expr(&doc, "true_").unwrap().get_bool().unwrap(),
            true
        );
        assert_eq!(
            extract_expr(&doc, "false_").unwrap().get_bool().unwrap(),
            false
        );
        assert_eq!(
            extract_expr(&doc, "array")
                .unwrap()
                .get_array()
                .unwrap()
                .iter()
                .map(|&n| Expression::from_node(n, doc.source())
                    .unwrap()
                    .get_number()
                    .unwrap())
                .collect::<Vec<_>>(),
            [0., 1., 2.]
        );
        assert!(extract_expr(&doc, "paren1").is_ok());
        assert!(extract_expr(&doc, "paren2").is_ok());
    }

    #[test]
    fn member_expression() {
        let doc = parse(
            r###"
            Foo {
                member: foo.bar
            }
            "###,
        );
        match extract_expr(&doc, "member").unwrap() {
            Expression::MemberExpression(x) => {
                assert_eq!(
                    Identifier::from_node(x.object, doc.source()).unwrap(),
                    Identifier::new("foo")
                );
                assert_eq!(x.property, Identifier::new("bar"));
            }
            expr => panic!("unexpected expression: {expr:?}"),
        }
    }

    #[test]
    fn call_expression() {
        let doc = parse(
            r###"
            Foo {
                call: foo(1, /*garbage*/ 2)
            }
            "###,
        );
        match extract_expr(&doc, "call").unwrap() {
            Expression::CallExpression(x) => {
                assert_eq!(
                    Identifier::from_node(x.function, doc.source()).unwrap(),
                    Identifier::new("foo")
                );
                assert_eq!(x.arguments.len(), 2);
            }
            expr => panic!("unexpected expression: {expr:?}"),
        }
    }

    #[test]
    fn unary_expression() {
        let doc = parse(
            r###"
            Foo {
                logical_not: !whatever
            }
            "###,
        );
        match extract_expr(&doc, "logical_not").unwrap() {
            Expression::UnaryExpression(x) => {
                assert_eq!(x.operator, UnaryOperator::LogicalNot);
            }
            expr => panic!("unexpected expression: {expr:?}"),
        }
    }

    #[test]
    fn binary_expression() {
        let doc = parse(
            r###"
            Foo {
                add: 1 + 2
            }
            "###,
        );
        match extract_expr(&doc, "add").unwrap() {
            Expression::BinaryExpression(x) => {
                assert_eq!(x.operator, BinaryOperator::Add);
                assert_ne!(x.left, x.right);
            }
            expr => panic!("unexpected expression: {expr:?}"),
        }
    }
}
