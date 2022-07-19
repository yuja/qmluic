use super::astutil::{self, Number};
use super::term::Identifier;
use super::{ParseError, ParseErrorKind};
use std::fmt;
use tree_sitter::{Node, TreeCursor};

/// Tagged union that wraps an expression.
#[derive(Clone, Debug)]
pub enum Expression<'tree> {
    Identifier(Identifier<'tree>),
    Integer(u64),
    Float(f64),
    String(String),
    Bool(bool),
    Array(Vec<Node<'tree>>),
    Member(MemberExpression<'tree>),
    Call(CallExpression<'tree>),
    Assignment(AssignmentExpression<'tree>),
    Unary(UnaryExpression<'tree>),
    Binary(BinaryExpression<'tree>),
    Ternary(TernaryExpression<'tree>),
    // TODO: ...
}

impl<'tree> Expression<'tree> {
    pub fn from_node(node: Node<'tree>, source: &str) -> Result<Self, ParseError<'tree>> {
        Self::with_cursor(&mut node.walk(), source)
    }

    pub(super) fn with_cursor(
        cursor: &mut TreeCursor<'tree>,
        source: &str,
    ) -> Result<Self, ParseError<'tree>> {
        while cursor.node().kind() == "parenthesized_expression" {
            if !cursor.goto_first_child() {
                return Err(ParseError::new(
                    cursor.node(),
                    ParseErrorKind::InvalidSyntax,
                ));
            }
            astutil::skip_until_named(cursor)?;
        }

        let node = cursor.node();
        let expr = match node.kind() {
            "identifier" => Expression::Identifier(Identifier::from_node(node)?),
            "number" => {
                // A number is floating point in JavaScript, but we want to discriminate
                // integer/float to process numeric operations in more C/Rust-like way.
                // It will help avoid unexpected integer->float conversion by division.
                match astutil::parse_number(node, source)? {
                    Number::Integer(n) => Expression::Integer(n),
                    Number::Float(n) => Expression::Float(n),
                }
            }
            "string" => Expression::String(astutil::parse_string(node, source)?),
            "true" => Expression::Bool(true),
            "false" => Expression::Bool(false),
            "array" => {
                let items = node
                    .named_children(cursor)
                    .filter(|n| !n.is_extra())
                    .collect();
                Expression::Array(items)
            }
            "member_expression" => {
                let object = astutil::get_child_by_field_name(node, "object")?;
                let property =
                    Identifier::from_node(astutil::get_child_by_field_name(node, "property")?)?;
                // TODO: <object>?.<property>
                Expression::Member(MemberExpression { object, property })
            }
            "call_expression" => {
                let function = astutil::get_child_by_field_name(node, "function")?;
                let arguments = astutil::get_child_by_field_name(node, "arguments")?
                    .named_children(cursor)
                    .filter(|n| !n.is_extra())
                    .collect();
                // TODO: <func>?.(<arg>...)
                Expression::Call(CallExpression {
                    function,
                    arguments,
                })
            }
            "assignment_expression" => {
                let left = astutil::get_child_by_field_name(node, "left")?;
                let right = astutil::get_child_by_field_name(node, "right")?;
                Expression::Assignment(AssignmentExpression { left, right })
            }
            "unary_expression" => {
                let operator =
                    UnaryOperator::from_node(astutil::get_child_by_field_name(node, "operator")?)?;
                let argument = astutil::get_child_by_field_name(node, "argument")?;
                Expression::Unary(UnaryExpression { operator, argument })
            }
            "binary_expression" => {
                let operator =
                    BinaryOperator::from_node(astutil::get_child_by_field_name(node, "operator")?)?;
                let left = astutil::get_child_by_field_name(node, "left")?;
                let right = astutil::get_child_by_field_name(node, "right")?;
                Expression::Binary(BinaryExpression {
                    operator,
                    left,
                    right,
                })
            }
            "ternary_expression" => {
                let condition = astutil::get_child_by_field_name(node, "condition")?;
                let consequence = astutil::get_child_by_field_name(node, "consequence")?;
                let alternative = astutil::get_child_by_field_name(node, "alternative")?;
                Expression::Ternary(TernaryExpression {
                    condition,
                    consequence,
                    alternative,
                })
            }
            // TODO: ...
            _ => {
                return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
            }
        };
        Ok(expr)
    }
}

/// Represents a member expression.
#[derive(Clone, Debug)]
pub struct MemberExpression<'tree> {
    pub object: Node<'tree>,
    pub property: Identifier<'tree>,
}

/// Represents a call expression.
#[derive(Clone, Debug)]
pub struct CallExpression<'tree> {
    pub function: Node<'tree>,
    pub arguments: Vec<Node<'tree>>,
}

/// Represents an assignment expression.
#[derive(Clone, Debug)]
pub struct AssignmentExpression<'tree> {
    pub left: Node<'tree>,
    pub right: Node<'tree>,
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
    pub fn from_node(node: Node) -> Result<Self, ParseError> {
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
    pub fn from_node(node: Node) -> Result<Self, ParseError> {
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

/// Represents a ternary (or conditional) expression.
#[derive(Clone, Debug)]
pub struct TernaryExpression<'tree> {
    pub condition: Node<'tree>,
    pub consequence: Node<'tree>,
    pub alternative: Node<'tree>,
}

#[cfg(test)]
mod tests {
    use super::super::stmt::Statement;
    use super::*;
    use crate::qmlast::{UiObjectDefinition, UiProgram};
    use crate::qmldoc::UiDocument;

    macro_rules! impl_unwrap_fn {
        ($name:ident, $pat:path, $ty:ty) => {
            fn $name(self) -> $ty {
                match self {
                    $pat(x) => x,
                    _ => panic!("unexpected expression: {self:?}"),
                }
            }
        };
    }

    impl<'tree> Expression<'tree> {
        impl_unwrap_fn!(unwrap_identifier, Expression::Identifier, Identifier<'tree>);
        impl_unwrap_fn!(unwrap_integer, Expression::Integer, u64);
        impl_unwrap_fn!(unwrap_float, Expression::Float, f64);
        impl_unwrap_fn!(unwrap_string, Expression::String, String);
        impl_unwrap_fn!(unwrap_bool, Expression::Bool, bool);
        impl_unwrap_fn!(unwrap_array, Expression::Array, Vec<Node<'tree>>);
        impl_unwrap_fn!(unwrap_member, Expression::Member, MemberExpression<'tree>);
        impl_unwrap_fn!(unwrap_call, Expression::Call, CallExpression<'tree>);
        impl_unwrap_fn!(
            unwrap_assignment,
            Expression::Assignment,
            AssignmentExpression<'tree>
        );
        impl_unwrap_fn!(unwrap_unary, Expression::Unary, UnaryExpression<'tree>);
        impl_unwrap_fn!(unwrap_binary, Expression::Binary, BinaryExpression<'tree>);
        impl_unwrap_fn!(
            unwrap_ternary,
            Expression::Ternary,
            TernaryExpression<'tree>
        );
    }

    fn parse(source: &str) -> UiDocument {
        UiDocument::parse(source, "MyType", None)
    }

    fn extract_expr<'a>(doc: &'a UiDocument, name: &str) -> Result<Expression<'a>, ParseError<'a>> {
        let program = UiProgram::from_node(doc.root_node(), doc.source()).unwrap();
        let obj = UiObjectDefinition::from_node(program.root_object_node(), doc.source()).unwrap();
        let map = obj.build_binding_map(doc.source()).unwrap();
        let node = map.get(name).unwrap().get_node().unwrap();
        match Statement::from_node(node).unwrap() {
            Statement::Expression(n) => Expression::from_node(n, doc.source()),
            stmt => panic!("expression statement node must be specified, but got {stmt:?}"),
        }
    }

    fn unwrap_expr<'a>(doc: &'a UiDocument, name: &str) -> Expression<'a> {
        extract_expr(doc, name).unwrap()
    }

    #[test]
    fn trivial_expressions() {
        let doc = parse(
            r###"
            Foo {
                identifier: foo
                integer: 123
                float: 123.
                string_: "whatever"
                escaped_string: "foo\nbar"
                true_: true
                false_: false
                array: [0, 1, /*garbage*/ 2]
                paren1: (foo)
                paren2: ((foo))
                paren_comment: (/*garbage*/ foo)
            }
            "###,
        );
        assert_eq!(
            unwrap_expr(&doc, "identifier")
                .unwrap_identifier()
                .to_str(doc.source()),
            "foo"
        );
        assert_eq!(unwrap_expr(&doc, "integer").unwrap_integer(), 123);
        assert_eq!(unwrap_expr(&doc, "float").unwrap_float(), 123.);
        assert_eq!(unwrap_expr(&doc, "string_").unwrap_string(), "whatever");
        assert_eq!(
            unwrap_expr(&doc, "escaped_string").unwrap_string(),
            "foo\nbar"
        );
        assert_eq!(unwrap_expr(&doc, "true_").unwrap_bool(), true);
        assert_eq!(unwrap_expr(&doc, "false_").unwrap_bool(), false);
        assert_eq!(
            unwrap_expr(&doc, "array")
                .unwrap_array()
                .iter()
                .map(|&n| Expression::from_node(n, doc.source())
                    .unwrap()
                    .unwrap_integer())
                .collect::<Vec<_>>(),
            [0, 1, 2]
        );
        assert!(extract_expr(&doc, "paren1").is_ok());
        assert!(extract_expr(&doc, "paren2").is_ok());
        assert!(extract_expr(&doc, "paren_comment").is_ok());
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
        let x = unwrap_expr(&doc, "member").unwrap_member();
        assert_eq!(
            Identifier::from_node(x.object)
                .unwrap()
                .to_str(doc.source()),
            "foo"
        );
        assert_eq!(x.property.to_str(doc.source()), "bar");
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
        let x = unwrap_expr(&doc, "call").unwrap_call();
        assert_eq!(
            Identifier::from_node(x.function)
                .unwrap()
                .to_str(doc.source()),
            "foo"
        );
        assert_eq!(x.arguments.len(), 2);
    }

    #[test]
    fn assignment_expression() {
        let doc = parse(
            r###"
            Foo {
                assign: foo = 1
            }
            "###,
        );
        let x = unwrap_expr(&doc, "assign").unwrap_assignment();
        assert_eq!(
            Identifier::from_node(x.left).unwrap().to_str(doc.source()),
            "foo"
        );
        assert_ne!(x.left, x.right);
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
        let x = unwrap_expr(&doc, "logical_not").unwrap_unary();
        assert_eq!(x.operator, UnaryOperator::LogicalNot);
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
        let x = unwrap_expr(&doc, "add").unwrap_binary();
        assert_eq!(x.operator, BinaryOperator::Add);
        assert_ne!(x.left, x.right);
    }

    #[test]
    fn ternary_expression() {
        let doc = parse(
            r###"
            Foo {
                ternary: true ? "true" : "false"
            }
            "###,
        );
        let x = unwrap_expr(&doc, "ternary").unwrap_ternary();
        assert_ne!(x.condition, x.consequence);
        assert_ne!(x.consequence, x.alternative);
    }
}
