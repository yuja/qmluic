//! Expression tree visitor with type information.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{BinaryOperator, Expression, Identifier, Node, UnaryOperator};
use crate::typemap::{Enum, Type, TypeSpace};
use std::borrow::Cow;
use std::fmt::Debug;

/// Expression type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeDesc<'a> {
    Bool,
    Number,
    String,
    Enum(Enum<'a>),
}

impl TypeDesc<'_> {
    pub fn qualified_name(&self) -> Cow<'_, str> {
        match self {
            TypeDesc::Bool => "bool".into(),
            TypeDesc::Number => "number".into(),
            TypeDesc::String => "string".into(),
            TypeDesc::Enum(en) => en.qualified_name(),
        }
    }
}

/// Provides type of the translated item.
pub trait DescribeType<'a> {
    fn type_desc(&self) -> TypeDesc<'a>;
}

/// Translates each expression node to [`Self::Item`].
pub trait ExpressionVisitor<'a> {
    type Item: DescribeType<'a>;

    fn visit_number(&self, value: f64) -> Option<Self::Item>;
    fn visit_string(&self, value: String) -> Option<Self::Item>;
    fn visit_bool(&self, value: bool) -> Option<Self::Item>;
    fn visit_enum(&self, enum_ty: Enum<'a>, variant: &str) -> Option<Self::Item>;

    fn visit_call_expression(
        &self,
        function: &str,
        arguments: Vec<Self::Item>,
    ) -> Option<Self::Item>;
    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        argument: Self::Item,
    ) -> Option<Self::Item>;
    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
    ) -> Option<Self::Item>;
}

/// Walks expression nodes recursively from the specified `node`.
///
/// `parent_space` is the context where an identifier expression is resolved.
pub fn walk<'a, P, V>(
    parent_space: &P, // TODO: should be QML space, not C++ metatype space
    node: Node,
    source: &str,
    visitor: &V,
    diagnostics: &mut Diagnostics,
) -> Option<V::Item>
where
    P: TypeSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    match diagnostics.consume_err(Expression::from_node(node, source))? {
        Expression::Identifier(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                "unsupported constant expression: bare identifier",
            ));
            None
        }
        Expression::Number(v) => {
            let res = visitor.visit_number(v);
            if res.is_none() {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    "unsupported constant expression: number",
                ));
            }
            res
        }
        Expression::String(v) => {
            let res = visitor.visit_string(v);
            if res.is_none() {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    "unsupported constant expression: string",
                ));
            }
            res
        }
        Expression::Bool(v) => {
            let res = visitor.visit_bool(v);
            if res.is_none() {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    "unsupported constant expression: bool",
                ));
            }
            res
        }
        Expression::Array(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                "unsupported constant expression: array",
            ));
            None
        }
        Expression::MemberExpression(x) => {
            let mid_ty = parse_type(parent_space, x.object, source, diagnostics)?;
            let name = x.property.to_str(source);
            if let Some(en) = mid_ty.get_enum_by_variant(name) {
                let res = visitor.visit_enum(en, name);
                if res.is_none() {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        "unsupported constant expression: enum",
                    ));
                }
                res
            } else {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!(
                        "enum variant not found in '{}': {}",
                        mid_ty.qualified_name(),
                        name
                    ),
                ));
                None
            }
        }
        Expression::CallExpression(x) => {
            // TODO: resolve function for the current context
            let function =
                expect_identifier(x.function, source, diagnostics).map(|n| n.to_str(source))?;
            let arguments = x
                .arguments
                .iter()
                .map(|&n| walk(parent_space, n, source, visitor, diagnostics))
                .collect::<Option<Vec<_>>>()?;
            // TODO: confine type error?
            let res = visitor.visit_call_expression(function, arguments);
            if res.is_none() {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    "unsupported function call",
                ));
            }
            res
        }
        Expression::UnaryExpression(x) => {
            let argument = walk(parent_space, x.argument, source, visitor, diagnostics)?;
            let argument_t = argument.type_desc();
            // TODO: confine type error?
            let res = visitor.visit_unary_expression(x.operator, argument);
            if res.is_none() {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!(
                        "unsupported unary operation on type: {}",
                        argument_t.qualified_name()
                    ),
                ));
            }
            res
        }
        Expression::BinaryExpression(x) => {
            let left = walk(parent_space, x.left, source, visitor, diagnostics)?;
            let right = walk(parent_space, x.right, source, visitor, diagnostics)?;
            let left_t = left.type_desc();
            let right_t = right.type_desc();
            // TODO: confine type error?
            let res = visitor.visit_binary_expression(x.operator, left, right);
            if res.is_none() {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!(
                        "unsupported binary operation on types: {} and {}",
                        left_t.qualified_name(),
                        right_t.qualified_name(),
                    ),
                ));
            }
            res
        }
    }
}

/// Parses the given `node` as type expression.
fn parse_type<'a, P>(
    parent_space: &P, // TODO: should be QML space, not C++ metatype space
    node: Node,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<Type<'a>>
where
    P: TypeSpace<'a>,
{
    match diagnostics.consume_err(Expression::from_node(node, source))? {
        Expression::Identifier(n) => {
            resolve_type_by_identifier(parent_space, n, source, diagnostics)
        }
        Expression::MemberExpression(x) => {
            let mid_space = parse_type(parent_space, x.object, source, diagnostics)?;
            get_type_by_identifier(&mid_space, x.property, source, diagnostics)
        }
        _ => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                "not a type identifier",
            ));
            None
        }
    }
}

fn get_type_by_identifier<'a, P>(
    parent_space: &P, // TODO: should be QML space, not C++ metatype space
    ident: Identifier,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<Type<'a>>
where
    P: TypeSpace<'a>,
{
    let name = ident.to_str(source);
    if let Some(ty) = parent_space.get_type(name) {
        Some(ty)
    } else {
        diagnostics.push(Diagnostic::error(
            ident.node().byte_range(),
            format!(
                "type not found as direct child of '{}': {}",
                parent_space.qualified_name(),
                name
            ),
        ));
        None
    }
}

fn resolve_type_by_identifier<'a, P>(
    parent_space: &P, // TODO: should be QML space, not C++ metatype space
    ident: Identifier,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<Type<'a>>
where
    P: TypeSpace<'a>,
{
    let name = ident.to_str(source);
    if let Some(ty) = parent_space.resolve_type(name) {
        Some(ty)
    } else {
        diagnostics.push(Diagnostic::error(
            ident.node().byte_range(),
            format!(
                "type not resolved from '{}': {}",
                parent_space.qualified_name(),
                name
            ),
        ));
        None
    }
}

fn expect_identifier<'t>(
    node: Node<'t>,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<Identifier<'t>> {
    match diagnostics.consume_err(Expression::from_node(node, source))? {
        Expression::Identifier(n) => Some(n),
        _ => {
            diagnostics.push(Diagnostic::error(node.byte_range(), "not an identifier"));
            None
        }
    }
}
