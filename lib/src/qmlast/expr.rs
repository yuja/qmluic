use super::astutil::{self, Number};
use super::node::StatementNode;
use super::term::{self, Identifier, NestedIdentifier};
use super::{ParseError, ParseErrorKind};
use std::fmt;
use tree_sitter::{Node, TreeCursor};

/// Tagged union that wraps an expression.
#[derive(Clone, Debug)]
pub enum Expression<'tree> {
    Identifier(Identifier<'tree>),
    This,
    Integer(u64),
    Float(f64),
    String(String),
    Bool(bool),
    Null,
    Array(Vec<Node<'tree>>),
    Function(Function<'tree>),
    ArrowFunction(Function<'tree>),
    Member(MemberExpression<'tree>),
    Subscript(SubscriptExpression<'tree>),
    Call(CallExpression<'tree>),
    Assignment(AssignmentExpression<'tree>),
    Unary(UnaryExpression<'tree>),
    Binary(BinaryExpression<'tree>),
    As(AsExpression<'tree>),
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
            astutil::goto_first_named_child(cursor)?;
        }

        let node = cursor.node();
        let expr = match node.kind() {
            "identifier" => Expression::Identifier(Identifier::from_node(node)?),
            "this" => Expression::This,
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
            "null" => Expression::Null,
            "array" => {
                let items = node
                    .named_children(cursor)
                    .filter(|n| !n.is_extra())
                    .collect();
                Expression::Array(items)
            }
            "function" => Function::with_cursor(cursor).map(Expression::Function)?,
            "arrow_function" => Function::with_cursor(cursor).map(Expression::ArrowFunction)?,
            "member_expression" => {
                let object = astutil::get_child_by_field_name(node, "object")?;
                let property =
                    Identifier::from_node(astutil::get_child_by_field_name(node, "property")?)?;
                // reject optional chaining: <object>?.<property>
                if let Some((n, _)) =
                    astutil::children_between_field_names(node, cursor, "object", "property")
                        .find(|(n, _)| !n.is_extra() && n.kind() != ".")
                {
                    return Err(ParseError::new(n, ParseErrorKind::UnexpectedNodeKind));
                }
                Expression::Member(MemberExpression { object, property })
            }
            "subscript_expression" => {
                let object = astutil::get_child_by_field_name(node, "object")?;
                let index = astutil::get_child_by_field_name(node, "index")?;
                // reject optional chaining: <object>?.[<index>]
                if let Some((n, _)) =
                    astutil::children_between_field_names(node, cursor, "object", "index")
                        .find(|(n, _)| !n.is_extra() && n.kind() != "[")
                {
                    return Err(ParseError::new(n, ParseErrorKind::UnexpectedNodeKind));
                }
                Expression::Subscript(SubscriptExpression { object, index })
            }
            "call_expression" => {
                let function = astutil::get_child_by_field_name(node, "function")?;
                let arguments = astutil::get_child_by_field_name(node, "arguments")?
                    .named_children(cursor)
                    .filter(|n| !n.is_extra())
                    .collect();
                // reject optional chaining: <function>?.(<arguments>)
                if let Some((n, _)) =
                    astutil::children_between_field_names(node, cursor, "function", "arguments")
                        .find(|(n, _)| !n.is_extra())
                {
                    return Err(ParseError::new(n, ParseErrorKind::UnexpectedNodeKind));
                }
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
            "as_expression" => {
                let invalid_syntax = || ParseError::new(node, ParseErrorKind::InvalidSyntax);
                let mut children = node.children(cursor).filter(|n| !n.is_extra());
                let value = children.next().ok_or_else(invalid_syntax)?;
                let as_node = children.next().ok_or_else(invalid_syntax)?;
                let ty_node = children.next().ok_or_else(invalid_syntax)?;
                if let Some(n) = children.next() {
                    return Err(ParseError::new(n, ParseErrorKind::UnexpectedNodeKind));
                }
                if as_node.kind() != "as" {
                    return Err(ParseError::new(as_node, ParseErrorKind::UnexpectedNodeKind));
                }
                Expression::As(AsExpression {
                    value,
                    ty: NestedIdentifier::from_node(ty_node)?,
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

/// Represents a function or arrow function expression.
#[derive(Clone, Debug)]
pub struct Function<'tree> {
    pub name: Option<Identifier<'tree>>,
    pub parameters: Vec<FormalParameter<'tree>>,
    pub return_ty: Option<NestedIdentifier<'tree>>,
    pub body: FunctionBody<'tree>,
}

impl<'tree> Function<'tree> {
    pub fn from_node(node: Node<'tree>) -> Result<Self, ParseError<'tree>> {
        Self::with_cursor(&mut node.walk())
    }

    fn with_cursor(cursor: &mut TreeCursor<'tree>) -> Result<Self, ParseError<'tree>> {
        let node = cursor.node();
        match node.kind() {
            "function" => {
                // reject unsupported keywords like 'async'
                if let Some(n) = node.children(cursor).find(|n| !n.is_extra()) {
                    if n.kind() != "function" {
                        return Err(ParseError::new(n, ParseErrorKind::UnexpectedNodeKind));
                    }
                }
                let name = node
                    .child_by_field_name("name")
                    .map(Identifier::from_node)
                    .transpose()?;
                let parameters = astutil::get_child_by_field_name(node, "parameters")?
                    .named_children(cursor)
                    .filter(|n| !n.is_extra())
                    .map(FormalParameter::from_node)
                    .collect::<Result<Vec<_>, _>>()?;
                let return_ty = node
                    .child_by_field_name("return_type")
                    .map(term::extract_type_annotation)
                    .transpose()?;
                let body_node = astutil::get_child_by_field_name(node, "body")?;
                Ok(Function {
                    name,
                    parameters,
                    return_ty,
                    body: FunctionBody::Statement(StatementNode(body_node)),
                })
            }
            "arrow_function" => {
                // reject unsupported keywords like 'async'
                if let Some((n, _)) = astutil::children_with_field_name(node, cursor)
                    .take_while(|(_, f)| !matches!(f, Some("parameter" | "parameters")))
                    .find(|(n, _)| !n.is_extra())
                {
                    return Err(ParseError::new(n, ParseErrorKind::UnexpectedNodeKind));
                }
                let parameters = if let Some(n) = node.child_by_field_name("parameter") {
                    vec![FormalParameter {
                        name: Identifier::from_node(n)?,
                        ty: None,
                    }]
                } else {
                    astutil::get_child_by_field_name(node, "parameters")?
                        .named_children(cursor)
                        .filter(|n| !n.is_extra())
                        .map(FormalParameter::from_node)
                        .collect::<Result<Vec<_>, _>>()?
                };
                let return_ty = node
                    .child_by_field_name("return_type")
                    .map(term::extract_type_annotation)
                    .transpose()?;
                let body_node = astutil::get_child_by_field_name(node, "body")?;
                let body = if body_node.kind() == "statement_block" {
                    FunctionBody::Statement(StatementNode(body_node))
                } else {
                    FunctionBody::Expression(body_node)
                };
                Ok(Function {
                    name: None,
                    parameters,
                    return_ty,
                    body,
                })
            }
            _ => Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind)),
        }
    }
}

/// Variant for function body.
///
/// If the function isn't an arrow function, the body should be `Statement` kind.
#[derive(Clone, Copy, Debug)]
pub enum FunctionBody<'tree> {
    Expression(Node<'tree>),
    Statement(StatementNode<'tree>),
}

/// Represents a function parameter.
///
/// This does not support destructuring pattern, default initializer, etc.
#[derive(Clone, Debug)]
pub struct FormalParameter<'tree> {
    pub name: Identifier<'tree>,
    pub ty: Option<NestedIdentifier<'tree>>,
}

impl<'tree> FormalParameter<'tree> {
    fn from_node(node: Node<'tree>) -> Result<Self, ParseError<'tree>> {
        if node.kind() != "required_parameter" {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }
        let name =
            astutil::get_child_by_field_name(node, "pattern").and_then(Identifier::from_node)?;
        let ty = node
            .child_by_field_name("type")
            .map(term::extract_type_annotation)
            .transpose()?;
        Ok(FormalParameter { name, ty })
    }

    pub fn node(&self) -> Node<'tree> {
        self.name
            .node()
            .parent()
            .expect("formal parameter name node should have parent")
    }
}

/// Represents a member expression.
#[derive(Clone, Debug)]
pub struct MemberExpression<'tree> {
    pub object: Node<'tree>,
    pub property: Identifier<'tree>,
}

/// Represents a subscript expression.
#[derive(Clone, Debug)]
pub struct SubscriptExpression<'tree> {
    pub object: Node<'tree>,
    pub index: Node<'tree>,
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

/// Represents an `as` (or type cast) expression.
#[derive(Clone, Debug)]
pub struct AsExpression<'tree> {
    pub value: Node<'tree>,
    pub ty: NestedIdentifier<'tree>,
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
        impl_unwrap_fn!(unwrap_function, Expression::Function, Function<'tree>);
        impl_unwrap_fn!(
            unwrap_arrow_function,
            Expression::ArrowFunction,
            Function<'tree>
        );
        impl_unwrap_fn!(unwrap_member, Expression::Member, MemberExpression<'tree>);
        impl_unwrap_fn!(
            unwrap_subscript,
            Expression::Subscript,
            SubscriptExpression<'tree>
        );
        impl_unwrap_fn!(unwrap_call, Expression::Call, CallExpression<'tree>);
        impl_unwrap_fn!(
            unwrap_assignment,
            Expression::Assignment,
            AssignmentExpression<'tree>
        );
        impl_unwrap_fn!(unwrap_unary, Expression::Unary, UnaryExpression<'tree>);
        impl_unwrap_fn!(unwrap_binary, Expression::Binary, BinaryExpression<'tree>);
        impl_unwrap_fn!(unwrap_as, Expression::As, AsExpression<'tree>);
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
                this_: this
                integer: 123
                float: 123.
                string_: "whatever"
                escaped_string: "foo\nbar"
                true_: true
                false_: false
                null_: null
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
        assert!(matches!(unwrap_expr(&doc, "this_"), Expression::This));
        assert_eq!(unwrap_expr(&doc, "integer").unwrap_integer(), 123);
        assert_eq!(unwrap_expr(&doc, "float").unwrap_float(), 123.);
        assert_eq!(unwrap_expr(&doc, "string_").unwrap_string(), "whatever");
        assert_eq!(
            unwrap_expr(&doc, "escaped_string").unwrap_string(),
            "foo\nbar"
        );
        assert_eq!(unwrap_expr(&doc, "true_").unwrap_bool(), true);
        assert_eq!(unwrap_expr(&doc, "false_").unwrap_bool(), false);
        assert!(matches!(unwrap_expr(&doc, "null_"), Expression::Null));
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
    fn function() {
        let doc = parse(
            r###"
            Foo {
                unnamed_no_arg: /*garbage*/ function /*garbage*/ (/*garbage*/) { 0 }
                unnamed_one_arg: function (/*garbage*/ a) {}
                named_no_arg: function foo() {}
                named_arg_typed: function foo(a: int, /*garbage*/ b: bar.bool): string {}
                async_fn: async function foo() {}
            }
            "###,
        );

        let x = unwrap_expr(&doc, "unnamed_no_arg").unwrap_function();
        assert!(x.name.is_none());
        assert!(x.parameters.is_empty());
        assert!(x.return_ty.is_none());
        assert!(matches!(x.body, FunctionBody::Statement(_)));

        let x = unwrap_expr(&doc, "unnamed_one_arg").unwrap_function();
        assert!(x.name.is_none());
        assert_eq!(x.parameters.len(), 1);
        assert_eq!(x.parameters[0].name.to_str(doc.source()), "a");
        assert!(x.parameters[0].ty.is_none());
        assert!(x.return_ty.is_none());

        let x = unwrap_expr(&doc, "named_no_arg").unwrap_function();
        assert_eq!(x.name.unwrap().to_str(doc.source()), "foo");
        assert!(x.parameters.is_empty());
        assert!(x.return_ty.is_none());

        let x = unwrap_expr(&doc, "named_arg_typed").unwrap_function();
        assert_eq!(x.name.unwrap().to_str(doc.source()), "foo");
        assert_eq!(x.parameters.len(), 2);
        assert_eq!(x.parameters[0].name.to_str(doc.source()), "a");
        assert_eq!(
            x.parameters[0].ty.as_ref().unwrap().to_string(doc.source()),
            "int"
        );
        assert_eq!(x.parameters[1].name.to_str(doc.source()), "b");
        assert_eq!(
            x.parameters[1].ty.as_ref().unwrap().to_string(doc.source()),
            "bar.bool"
        );
        assert_eq!(
            x.return_ty.as_ref().unwrap().to_string(doc.source()),
            "string"
        );

        assert!(extract_expr(&doc, "async_fn").is_err());
    }

    #[test]
    fn arrow_function() {
        let doc = parse(
            r###"
            Foo {
                no_arg_expr: /*garbage*/ (/*garbage*/) => /*garbage*/ 0
                no_arg_stmt: /*garbage*/ (/*garbage*/) => { 0 }
                bare_arg: /*garbage*/ a => a
                one_arg: (/*garbage*/ a) => {}
                arg_typed: (a: int, /*garbage*/ b: bool): string => {}
                async_bare: async a => a
                async_paren: async () => {}
            }
            "###,
        );

        let x = unwrap_expr(&doc, "no_arg_expr").unwrap_arrow_function();
        assert!(x.name.is_none());
        assert!(x.parameters.is_empty());
        assert!(x.return_ty.is_none());
        assert!(matches!(x.body, FunctionBody::Expression(_)));

        let x = unwrap_expr(&doc, "no_arg_stmt").unwrap_arrow_function();
        assert!(x.name.is_none());
        assert!(x.parameters.is_empty());
        assert!(x.return_ty.is_none());
        assert!(matches!(x.body, FunctionBody::Statement(_)));

        let x = unwrap_expr(&doc, "bare_arg").unwrap_arrow_function();
        assert_eq!(x.parameters.len(), 1);
        assert_eq!(x.parameters[0].name.to_str(doc.source()), "a");
        assert!(x.parameters[0].ty.is_none());
        assert!(x.return_ty.is_none());

        let x = unwrap_expr(&doc, "one_arg").unwrap_arrow_function();
        assert_eq!(x.parameters.len(), 1);
        assert_eq!(x.parameters[0].name.to_str(doc.source()), "a");
        assert!(x.parameters[0].ty.is_none());
        assert!(x.return_ty.is_none());

        let x = unwrap_expr(&doc, "arg_typed").unwrap_arrow_function();
        assert_eq!(x.parameters.len(), 2);
        assert_eq!(x.parameters[0].name.to_str(doc.source()), "a");
        assert_eq!(
            x.parameters[0].ty.as_ref().unwrap().to_string(doc.source()),
            "int"
        );
        assert_eq!(x.parameters[1].name.to_str(doc.source()), "b");
        assert_eq!(
            x.parameters[1].ty.as_ref().unwrap().to_string(doc.source()),
            "bool"
        );
        assert_eq!(
            x.return_ty.as_ref().unwrap().to_string(doc.source()),
            "string"
        );

        assert!(extract_expr(&doc, "async_bare").is_err());
        assert!(extract_expr(&doc, "async_paren").is_err());
    }

    #[test]
    fn member_expression() {
        let doc = parse(
            r###"
            Foo {
                member: foo /*garbage*/ .  /*garbage*/ bar
                member_opt: foo?.bar
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

        assert!(extract_expr(&doc, "member_opt").is_err());
    }

    #[test]
    fn subscript_expression() {
        let doc = parse(
            r###"
            Foo {
                subscript: foo /*garbage*/ [ /*garbage*/ bar ]
                subscript_opt: foo?.[bar]
            }
            "###,
        );

        let x = unwrap_expr(&doc, "subscript").unwrap_subscript();
        assert_eq!(
            Identifier::from_node(x.object)
                .unwrap()
                .to_str(doc.source()),
            "foo"
        );
        assert_eq!(
            Identifier::from_node(x.index).unwrap().to_str(doc.source()),
            "bar"
        );

        assert!(extract_expr(&doc, "subscript_opt").is_err());
    }

    #[test]
    fn call_expression() {
        let doc = parse(
            r###"
            Foo {
                call: foo /*garbage*/ (1, /*garbage*/ 2)
                call_opt: foo.bar?.()
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

        assert!(extract_expr(&doc, "call_opt").is_err());
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
    fn as_expression() {
        let doc = parse(
            r###"
            Foo {
                as_: /*garbage*/ 1.0 /*garbage*/ as /*garbage*/ int /*garbage*/
            }
            "###,
        );
        let x = unwrap_expr(&doc, "as_").unwrap_as();
        assert_ne!(x.value, x.ty.node());
        assert_eq!(x.ty.to_string(doc.source()), "int");
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
