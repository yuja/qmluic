//! Expression tree visitor with type information.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectTree;
use crate::qmlast::{BinaryOperator, Expression, Identifier, Node, UnaryOperator};
use crate::typemap::{Enum, NamedType, TypeSpace};
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
            TypeDesc::Enum(en) => en.qualified_cxx_name(),
        }
    }
}

/// Provides type of the translated item.
pub trait DescribeType<'a> {
    fn type_desc(&self) -> TypeDesc<'a>;
}

/// Translates each expression node to [`Self::Item`].
pub trait ExpressionVisitor<'a> {
    type Item: DescribeType<'a>; // TODO: do we want to check type error by walk()?
    type Error: ToString;

    fn visit_number(&self, value: f64) -> Result<Self::Item, Self::Error>;
    fn visit_string(&self, value: String) -> Result<Self::Item, Self::Error>;
    fn visit_bool(&self, value: bool) -> Result<Self::Item, Self::Error>;
    fn visit_enum(&self, enum_ty: Enum<'a>, variant: &str) -> Result<Self::Item, Self::Error>;

    fn visit_call_expression(
        &self,
        function: &str,
        arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        argument: Self::Item,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
    ) -> Result<Self::Item, Self::Error>;
}

/// Walks expression nodes recursively from the specified `node`.
///
/// `parent_space` is the context where an identifier expression is resolved.
pub fn walk<'a, P, V>(
    // TODO: maybe extract an abstraction over (parent_space, object_tree):
    // resolve(name) -> Kind::Type(ty)|ObjRef(ty)|ThisProperty(..)|..
    parent_space: &P,
    object_tree: &ObjectTree<'a, '_>,
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
        Expression::Number(v) => diagnostics.consume_node_err(node, visitor.visit_number(v)),
        Expression::String(v) => diagnostics.consume_node_err(node, visitor.visit_string(v)),
        Expression::Bool(v) => diagnostics.consume_node_err(node, visitor.visit_bool(v)),
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
                diagnostics.consume_node_err(node, visitor.visit_enum(en, name))
            } else {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!(
                        "enum variant not found in '{}': {}",
                        mid_ty.qualified_cxx_name(),
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
                .map(|&n| walk(parent_space, object_tree, n, source, visitor, diagnostics))
                .collect::<Option<Vec<_>>>()?;
            // TODO: confine type error?
            diagnostics.consume_node_err(node, visitor.visit_call_expression(function, arguments))
        }
        Expression::UnaryExpression(x) => {
            let argument = walk(
                parent_space,
                object_tree,
                x.argument,
                source,
                visitor,
                diagnostics,
            )?;
            // TODO: confine type error?
            diagnostics.consume_node_err(node, visitor.visit_unary_expression(x.operator, argument))
        }
        Expression::BinaryExpression(x) => {
            let left = walk(
                parent_space,
                object_tree,
                x.left,
                source,
                visitor,
                diagnostics,
            )?;
            let right = walk(
                parent_space,
                object_tree,
                x.right,
                source,
                visitor,
                diagnostics,
            )?;
            // TODO: confine type error?
            diagnostics.consume_node_err(
                node,
                visitor.visit_binary_expression(x.operator, left, right),
            )
        }
    }
}

/// Parses the given `node` as type expression.
fn parse_type<'a, P>(
    parent_space: &P,
    node: Node,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<NamedType<'a>>
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
    parent_space: &P,
    ident: Identifier,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<NamedType<'a>>
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
                parent_space.qualified_cxx_name(),
                name
            ),
        ));
        None
    }
}

fn resolve_type_by_identifier<'a, P>(
    parent_space: &P,
    ident: Identifier,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<NamedType<'a>>
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
                parent_space.qualified_cxx_name(),
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

impl Diagnostics {
    fn consume_node_err<T, E>(&mut self, node: Node, result: Result<T, E>) -> Option<T>
    where
        E: ToString,
    {
        self.consume_err(result.map_err(|e| Diagnostic::error(node.byte_range(), e.to_string())))
    }
}
