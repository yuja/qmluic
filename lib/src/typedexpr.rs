//! Expression tree visitor with type information.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{BinaryOperator, Expression, Identifier, Node, UnaryOperator};
use crate::typemap::{Class, Enum, NamedType, PrimitiveType, Property, TypeKind, TypeSpace};
use std::borrow::Cow;
use std::fmt::Debug;

/// Expression type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeDesc<'a> {
    Number,
    String,
    Enum(Enum<'a>),
    ObjectRef(Class<'a>),
    ObjectRefList(Class<'a>),
    StringList,
    EmptyList,
    /// Type that has been determined.
    Concrete(TypeKind<'a>),
}

impl<'a> TypeDesc<'a> {
    pub const BOOL: Self =
        TypeDesc::Concrete(TypeKind::Just(NamedType::Primitive(PrimitiveType::Bool)));

    // TODO: refactor type handling to eliminate this kind of repacking?
    pub fn from_type_kind(type_kind: TypeKind<'a>) -> Option<Self> {
        match type_kind {
            TypeKind::Just(NamedType::Enum(en)) => Some(TypeDesc::Enum(en)),
            TypeKind::Just(NamedType::Primitive(p)) => Self::from_primitive_type(p),
            TypeKind::Just(
                NamedType::Class(_) | NamedType::Namespace(_) | NamedType::QmlComponent(_),
            ) => None,
            TypeKind::Pointer(NamedType::Class(cls)) => Some(TypeDesc::ObjectRef(cls)),
            TypeKind::Pointer(
                NamedType::Enum(_)
                | NamedType::Namespace(_)
                | NamedType::Primitive(_)
                | NamedType::QmlComponent(_),
            ) => None,
            TypeKind::PointerList(NamedType::Class(cls)) => Some(TypeDesc::ObjectRefList(cls)),
            TypeKind::PointerList(
                NamedType::Enum(_)
                | NamedType::Namespace(_)
                | NamedType::Primitive(_)
                | NamedType::QmlComponent(_),
            ) => None,
        }
    }

    fn from_primitive_type(p: PrimitiveType) -> Option<Self> {
        match p {
            PrimitiveType::Int | PrimitiveType::QReal | PrimitiveType::UInt => {
                Some(TypeDesc::Number)
            }
            PrimitiveType::QString => Some(TypeDesc::String),
            PrimitiveType::QStringList => Some(TypeDesc::StringList),
            PrimitiveType::Void => None,
            p => Some(TypeDesc::Concrete(TypeKind::Just(NamedType::Primitive(p)))),
        }
    }

    pub fn qualified_name(&self) -> Cow<'_, str> {
        match self {
            TypeDesc::Number => "number".into(),
            TypeDesc::String => "string".into(),
            TypeDesc::Enum(en) => en.qualified_cxx_name(),
            TypeDesc::ObjectRef(cls) => cls.qualified_cxx_name(),
            TypeDesc::ObjectRefList(cls) => format!("list<{}>", cls.qualified_cxx_name()).into(),
            TypeDesc::StringList => "list<string>".into(),
            TypeDesc::EmptyList => "list".into(),
            TypeDesc::Concrete(k) => k.qualified_cxx_name(),
        }
    }
}

/// Provides type of the translated item.
pub trait DescribeType<'a> {
    fn type_desc(&self) -> TypeDesc<'a>;
}

/// Builtin functions.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltinFunctionKind {
    /// `qsTr()`
    Tr,
}

/// Resolved type/object reference.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RefKind<'a> {
    /// Builtin function.
    BuiltinFunction(BuiltinFunctionKind),
    /// Type reference.
    Type(NamedType<'a>),
    /// Enum variant of the type.
    EnumVariant(Enum<'a>),
    /// Object reference of the type.
    Object(Class<'a>),
    // TODO: Function, Property, etc.?
}

/// Top-level context or object which supports name lookup.
pub trait RefSpace<'a> {
    /// Looks up reference by name.
    fn get_ref(&self, name: &str) -> Option<RefKind<'a>>;
}

impl<'a, T: TypeSpace<'a>> RefSpace<'a> for T {
    fn get_ref(&self, name: &str) -> Option<RefKind<'a>> {
        #[allow(clippy::manual_map)]
        if let Some(ty) = self.get_type(name) {
            Some(RefKind::Type(ty))
        } else if let Some(en) = self.get_enum_by_variant(name) {
            Some(RefKind::EnumVariant(en))
        } else {
            None
        }
    }
}

/// Translates each expression node to [`Self::Item`].
pub trait ExpressionVisitor<'a> {
    type Item: DescribeType<'a>; // TODO: do we want to check type error by walk()?
    type Error: ToString;

    fn visit_number(&mut self, value: f64) -> Result<Self::Item, Self::Error>;
    fn visit_string(&mut self, value: String) -> Result<Self::Item, Self::Error>;
    fn visit_bool(&mut self, value: bool) -> Result<Self::Item, Self::Error>;
    fn visit_enum(&mut self, enum_ty: Enum<'a>, variant: &str) -> Result<Self::Item, Self::Error>;

    fn visit_array(&mut self, elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error>;

    fn visit_object_ref(&mut self, cls: Class<'a>, name: &str) -> Result<Self::Item, Self::Error>;
    fn visit_object_property(
        &mut self,
        object: Self::Item,
        property: Property<'a>,
    ) -> Result<Self::Item, Self::Error>;

    fn visit_builtin_call(
        &mut self,
        function: BuiltinFunctionKind,
        arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_unary_expression(
        &mut self,
        operator: UnaryOperator,
        argument: Self::Item,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_binary_expression(
        &mut self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_ternary_expression(
        &mut self,
        condition: Self::Item,
        consequence: Self::Item,
        alternative: Self::Item,
    ) -> Result<Self::Item, Self::Error>;
}

#[derive(Debug)]
enum Intermediate<'a, T> {
    Item(T),
    BuiltinFunction(BuiltinFunctionKind),
    Type(NamedType<'a>),
}

/// Walks expression nodes recursively from the specified `node`.
///
/// `parent_space` is the context where an identifier expression is resolved.
pub fn walk<'a, C, V>(
    ctx: &C,
    node: Node,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<V::Item>
where
    C: RefSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    match walk_inner(ctx, node, source, visitor, diagnostics)? {
        Intermediate::Item(x) => Some(x),
        Intermediate::BuiltinFunction(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                "bare function reference",
            ));
            None
        }
        Intermediate::Type(_) => {
            diagnostics.push(Diagnostic::error(node.byte_range(), "bare type reference"));
            None
        }
    }
}

fn walk_inner<'a, C, V>(
    ctx: &C,
    node: Node,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<Intermediate<'a, V::Item>>
where
    C: RefSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    match diagnostics.consume_err(Expression::from_node(node, source))? {
        Expression::Identifier(x) => process_identifier(ctx, x, source, visitor, diagnostics),
        Expression::Number(v) => diagnostics
            .consume_node_err(node, visitor.visit_number(v))
            .map(Intermediate::Item),
        Expression::String(v) => diagnostics
            .consume_node_err(node, visitor.visit_string(v))
            .map(Intermediate::Item),
        Expression::Bool(v) => diagnostics
            .consume_node_err(node, visitor.visit_bool(v))
            .map(Intermediate::Item),
        Expression::Array(ns) => {
            let elements = ns
                .iter()
                .map(|&n| walk(ctx, n, source, visitor, diagnostics))
                .collect::<Option<Vec<_>>>()?;
            diagnostics
                .consume_node_err(node, visitor.visit_array(elements))
                .map(Intermediate::Item)
        }
        Expression::MemberExpression(x) => {
            match walk_inner(ctx, x.object, source, visitor, diagnostics)? {
                Intermediate::Item(it) => {
                    process_item_property(it, x.property, source, visitor, diagnostics)
                }
                Intermediate::BuiltinFunction(_) => {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        "function has no property/method",
                    ));
                    None
                }
                Intermediate::Type(ty) => {
                    process_identifier(&ty, x.property, source, visitor, diagnostics)
                }
            }
        }
        Expression::CallExpression(x) => {
            let arguments = x
                .arguments
                .iter()
                .map(|&n| walk(ctx, n, source, visitor, diagnostics))
                .collect::<Option<Vec<_>>>()?;
            match walk_inner(ctx, x.function, source, visitor, diagnostics)? {
                Intermediate::BuiltinFunction(f) => {
                    // TODO: confine type error?
                    diagnostics
                        .consume_node_err(node, visitor.visit_builtin_call(f, arguments))
                        .map(Intermediate::Item)
                }
                Intermediate::Item(_) | Intermediate::Type(_) => {
                    diagnostics.push(Diagnostic::error(x.function.byte_range(), "not callable"));
                    None
                }
            }
        }
        Expression::UnaryExpression(x) => {
            let argument = walk(ctx, x.argument, source, visitor, diagnostics)?;
            // TODO: confine type error?
            diagnostics
                .consume_node_err(node, visitor.visit_unary_expression(x.operator, argument))
                .map(Intermediate::Item)
        }
        Expression::BinaryExpression(x) => {
            let left = walk(ctx, x.left, source, visitor, diagnostics)?;
            let right = walk(ctx, x.right, source, visitor, diagnostics)?;
            // TODO: confine type error?
            diagnostics
                .consume_node_err(
                    node,
                    visitor.visit_binary_expression(x.operator, left, right),
                )
                .map(Intermediate::Item)
        }
        Expression::TernaryExpression(x) => {
            let condition = walk(ctx, x.condition, source, visitor, diagnostics)?;
            let consequence = walk(ctx, x.consequence, source, visitor, diagnostics)?;
            let alternative = walk(ctx, x.alternative, source, visitor, diagnostics)?;
            diagnostics
                .consume_node_err(
                    node,
                    visitor.visit_ternary_expression(condition, consequence, alternative),
                )
                .map(Intermediate::Item)
        }
    }
}

fn process_identifier<'a, C, V>(
    ctx: &C,
    id: Identifier,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<Intermediate<'a, V::Item>>
where
    C: RefSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    let name = id.to_str(source);
    match ctx.get_ref(name) {
        Some(RefKind::BuiltinFunction(f)) => Some(Intermediate::BuiltinFunction(f)),
        Some(RefKind::Type(ty)) => Some(Intermediate::Type(ty)),
        Some(RefKind::EnumVariant(en)) => diagnostics
            .consume_node_err(id.node(), visitor.visit_enum(en, name))
            .map(Intermediate::Item),
        Some(RefKind::Object(cls)) => diagnostics
            .consume_node_err(id.node(), visitor.visit_object_ref(cls, name))
            .map(Intermediate::Item),
        None => {
            diagnostics.push(Diagnostic::error(
                id.node().byte_range(),
                "undefined reference",
            ));
            None
        }
    }
}

fn process_item_property<'a, V>(
    item: V::Item,
    id: Identifier,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<Intermediate<'a, V::Item>>
where
    V: ExpressionVisitor<'a>,
{
    let not_found = || Diagnostic::error(id.node().byte_range(), "no property/method found");
    let name = id.to_str(source);
    match item.type_desc() {
        // simple value types
        TypeDesc::Number
        | TypeDesc::String
        | TypeDesc::Enum(_)
        | TypeDesc::Concrete(TypeKind::Just(NamedType::Primitive(
            PrimitiveType::Bool
            | PrimitiveType::Int
            | PrimitiveType::QReal
            | PrimitiveType::QString
            | PrimitiveType::UInt,
        )))
        | TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(_))) => {
            diagnostics.push(not_found());
            None
        }
        // gadget types
        TypeDesc::Concrete(TypeKind::Just(NamedType::Class(_))) => {
            diagnostics.push(not_found());
            None
        }
        // object types
        TypeDesc::ObjectRef(cls) | TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(cls))) => {
            if let Some(p) = cls.get_property(name) {
                diagnostics
                    .consume_node_err(id.node(), visitor.visit_object_property(item, p))
                    .map(Intermediate::Item)
            } else {
                diagnostics.push(not_found());
                None
            }
        }
        // list types
        TypeDesc::ObjectRefList(_)
        | TypeDesc::StringList
        | TypeDesc::EmptyList
        | TypeDesc::Concrete(TypeKind::Just(NamedType::Primitive(PrimitiveType::QStringList)))
        | TypeDesc::Concrete(TypeKind::PointerList(_)) => {
            diagnostics.push(not_found());
            None
        }
        // unsupported/invalid types
        TypeDesc::Concrete(TypeKind::Just(
            NamedType::Namespace(_)
            | NamedType::Primitive(PrimitiveType::Void)
            | NamedType::QmlComponent(_),
        ))
        | TypeDesc::Concrete(TypeKind::Pointer(
            NamedType::Enum(_)
            | NamedType::Namespace(_)
            | NamedType::Primitive(_)
            | NamedType::QmlComponent(_),
        )) => {
            diagnostics.push(not_found());
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
