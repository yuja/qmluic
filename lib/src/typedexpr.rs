//! Expression tree visitor with type information.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::opcode::{BinaryOp, BuiltinFunctionKind, BuiltinMethodKind, ComparisonOp, UnaryOp};
use crate::qmlast::{Expression, Identifier, LexicalDeclarationKind, Node, Statement};
use crate::typemap::{
    Class, Enum, MethodMatches, NamedType, PrimitiveType, Property, TypeKind, TypeMapError,
    TypeSpace,
};
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Range;

/// Expression type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeDesc<'a> {
    /// Integer literal or constant expression without concrete type.
    ConstInteger,
    /// String literal or constant expression without concrete type.
    ConstString,
    /// Empty array literal or constant expression without concrete type.
    EmptyList,
    /// Type that has been determined.
    Concrete(TypeKind<'a>),
}

impl<'a> TypeDesc<'a> {
    pub const BOOL: Self = TypeDesc::Concrete(TypeKind::BOOL);
    pub const DOUBLE: Self = TypeDesc::Concrete(TypeKind::DOUBLE);
    pub const INT: Self = TypeDesc::Concrete(TypeKind::INT);
    pub const UINT: Self = TypeDesc::Concrete(TypeKind::UINT);
    pub const STRING: Self = TypeDesc::Concrete(TypeKind::STRING);
    pub const STRING_LIST: Self = TypeDesc::Concrete(TypeKind::STRING_LIST);
    pub const VOID: Self = TypeDesc::Concrete(TypeKind::VOID);

    pub fn qualified_name(&self) -> Cow<'_, str> {
        match self {
            TypeDesc::ConstInteger => "integer".into(),
            TypeDesc::ConstString => "string".into(),
            TypeDesc::EmptyList => "list".into(),
            TypeDesc::Concrete(k) => k.qualified_cxx_name(),
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, TypeDesc::Concrete(TypeKind::Pointer(_)))
    }
}

/// Provides type of the translated item.
pub trait DescribeType<'a> {
    fn type_desc(&self) -> TypeDesc<'a>;
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
    /// Property of implicit this object reference.
    ObjectProperty(Class<'a>, String, Property<'a>),
    /// Method of implicit this object reference.
    ObjectMethod(Class<'a>, String, MethodMatches<'a>),
}

/// Top-level context or object which supports name lookup.
pub trait RefSpace<'a> {
    /// Looks up reference by name.
    fn get_ref(&self, name: &str) -> Option<Result<RefKind<'a>, TypeMapError>>;
    /// Looks up "this" object type and name.
    fn this_object(&self) -> Option<(Class<'a>, String)>;
}

impl<'a, T: TypeSpace<'a>> RefSpace<'a> for T {
    fn get_ref(&self, name: &str) -> Option<Result<RefKind<'a>, TypeMapError>> {
        #[allow(clippy::manual_map)]
        if let Some(r) = self.get_type(name) {
            Some(r.map(RefKind::Type))
        } else if let Some(r) = self.get_enum_by_variant(name) {
            Some(r.map(RefKind::EnumVariant))
        } else {
            None
        }
    }

    fn this_object(&self) -> Option<(Class<'a>, String)> {
        None
    }
}

/// Translates each expression node to [`Self::Item`].
pub trait ExpressionVisitor<'a> {
    type Item: DescribeType<'a> + Clone;
    type Local: Copy;
    type Label: Copy;
    type Error: ToString;

    /// Creates an item that represents a valid, but not meaningful value. `return;` statement
    /// will generate this value.
    fn make_void(&self, byte_range: Range<usize>) -> Self::Item;

    fn visit_integer(
        &mut self,
        value: u64,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_float(
        &mut self,
        value: f64,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_string(
        &mut self,
        value: String,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_bool(
        &mut self,
        value: bool,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_enum(
        &mut self,
        enum_ty: Enum<'a>,
        variant: &str,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;

    fn visit_array(
        &mut self,
        elements: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;

    fn visit_local_ref(&mut self, name: Self::Local) -> Result<Self::Item, Self::Error>;
    fn visit_local_declaration(
        &mut self,
        value: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Local, Self::Error>;
    fn visit_local_assignment(
        &mut self,
        name: Self::Local,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;

    fn visit_object_ref(
        &mut self,
        cls: Class<'a>,
        name: &str,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_object_property(
        &mut self,
        object: Self::Item,
        property: Property<'a>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_object_property_assignment(
        &mut self,
        object: Self::Item,
        property: Property<'a>,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_object_method_call(
        &mut self,
        object: Self::Item,
        methods: MethodMatches<'a>,
        arguments: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_object_builtin_method_call(
        &mut self,
        object: Self::Item,
        function: BuiltinMethodKind,
        arguments: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;

    fn visit_builtin_call(
        &mut self,
        function: BuiltinFunctionKind,
        arguments: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_unary_expression(
        &mut self,
        unary: UnaryOp,
        argument: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_binary_expression(
        &mut self,
        binary: BinaryOp,
        left: Self::Item,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;
    fn visit_ternary_expression(
        &mut self,
        condition: (Self::Item, Self::Label),
        consequence: (Self::Item, Self::Label),
        alternative: (Self::Item, Self::Label),
        byte_range: Range<usize>,
    ) -> Result<Self::Item, Self::Error>;

    fn visit_expression_statement(&mut self, value: Self::Item) -> Result<(), Self::Error>;
    fn visit_if_statement(
        &mut self,
        condition: (Self::Item, Self::Label),
        consequence_ref: Self::Label,
        alternative_ref: Option<Self::Label>,
    ) -> Result<(), Self::Error>;
    fn visit_switch_statement(
        &mut self,
        cases: Vec<(Self::Item, Self::Label, Self::Label)>,
        default: Option<(usize, Self::Label)>,
        head_ref: Self::Label,
        exit_ref: Self::Label,
    ) -> Result<(), Self::Error>;
    fn visit_break_statement(&mut self, exit_ref: Self::Label) -> Result<(), Self::Error>;
    fn visit_return_statement(&mut self, value: Self::Item) -> Result<(), Self::Error>;

    fn mark_branch_point(&mut self) -> Self::Label;
}

#[derive(Debug)]
enum Intermediate<'a, T, L> {
    Item(T),
    Local(L, LexicalDeclarationKind),
    BoundProperty(T, Property<'a>),
    BoundMethod(T, MethodMatches<'a>),
    BoundBuiltinMethod(T, BuiltinMethodKind),
    BuiltinFunction(BuiltinFunctionKind),
    Type(NamedType<'a>),
}

/// Walks statement nodes recursively from the specified `node`.
///
/// `ctx` is the space where an identifier expression is resolved.
pub fn walk<'a, C, V>(
    ctx: &C,
    node: Node,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<()>
where
    C: RefSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    let mut locals = HashMap::new();
    walk_stmt(ctx, &mut locals, None, node, source, visitor, diagnostics)
}

/// Walks statement nodes recursively.
fn walk_stmt<'a, C, V>(
    ctx: &C,
    locals: &mut HashMap<String, (V::Local, LexicalDeclarationKind)>,
    break_label: Option<V::Label>,
    node: Node,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<()>
where
    C: RefSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    match diagnostics.consume_err(Statement::from_node(node))? {
        Statement::Expression(n) => {
            let value = walk_rvalue(ctx, locals, n, source, visitor, diagnostics)?;
            diagnostics.consume_node_err(node, visitor.visit_expression_statement(value))
        }
        Statement::Block(ns) => {
            let mut locals = locals.clone(); // inner scope inheriting outer
            walk_stmt_nodes(
                ctx,
                &mut locals,
                break_label,
                &ns,
                source,
                visitor,
                diagnostics,
            )
        }
        Statement::LexicalDeclaration(x) => {
            for decl in &x.variables {
                if let Some(n) = decl.value {
                    let value = walk_rvalue(ctx, locals, n, source, visitor, diagnostics)?;
                    let local = diagnostics.consume_node_err(
                        decl.name.node(),
                        visitor.visit_local_declaration(value, decl.name.node().byte_range()),
                    )?;
                    locals.insert(decl.name.to_str(source).to_owned(), (local, x.kind));
                } else {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        "variable declaration without initializer is not supported",
                    ));
                    return None;
                }
            }
            Some(())
        }
        Statement::If(x) => {
            let condition = walk_rvalue(ctx, locals, x.condition, source, visitor, diagnostics)?;
            let condition_label = visitor.mark_branch_point();
            walk_stmt(
                ctx,
                locals,
                break_label,
                x.consequence,
                source,
                visitor,
                diagnostics,
            )?;
            let consequence_label = visitor.mark_branch_point();
            let alternative_label = if let Some(n) = x.alternative {
                walk_stmt(ctx, locals, break_label, n, source, visitor, diagnostics)?;
                Some(visitor.mark_branch_point())
            } else {
                None
            };
            diagnostics.consume_node_err(
                node,
                visitor.visit_if_statement(
                    (condition, condition_label),
                    consequence_label,
                    alternative_label,
                ),
            )
        }
        Statement::Switch(x) => {
            let left = walk_rvalue(ctx, locals, x.value, source, visitor, diagnostics)?;
            // allocate empty block where "break" will be directed
            let head_ref = visitor.mark_branch_point();
            let exit_ref = visitor.mark_branch_point();
            let break_label = Some(exit_ref);
            let cases: Vec<_> = x
                .cases
                .iter()
                .filter_map(|c| {
                    let right = walk_rvalue(ctx, locals, c.value, source, visitor, diagnostics)?;
                    let condition = diagnostics.consume_node_err(
                        c.value,
                        visitor.visit_binary_expression(
                            BinaryOp::Comparison(ComparisonOp::Equal),
                            left.clone(),
                            right,
                            c.value.byte_range(),
                        ),
                    )?;
                    let condition_label = visitor.mark_branch_point();
                    walk_stmt_nodes(
                        ctx,
                        locals,
                        break_label,
                        &c.body,
                        source,
                        visitor,
                        diagnostics,
                    )?;
                    let body_label = visitor.mark_branch_point();
                    Some((condition, condition_label, body_label))
                })
                .collect();
            let default = if let Some(d) = &x.default {
                walk_stmt_nodes(
                    ctx,
                    locals,
                    break_label,
                    &d.body,
                    source,
                    visitor,
                    diagnostics,
                )?;
                let body_label = visitor.mark_branch_point();
                Some((d.position, body_label))
            } else {
                None
            };
            if x.cases.len() == cases.len() {
                diagnostics.consume_node_err(
                    node,
                    visitor.visit_switch_statement(cases, default, head_ref, exit_ref),
                )
            } else {
                None
            }
        }
        Statement::Break(l) => {
            if let Some(n) = l {
                diagnostics.push(Diagnostic::error(
                    n.node().byte_range(),
                    "labeled break is not supported",
                ));
                None
            } else if let Some(l) = break_label {
                diagnostics.consume_node_err(node, visitor.visit_break_statement(l))
            } else {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    "break not in loop or switch statement",
                ));
                None
            }
        }
        Statement::Return(x) => {
            let value = if let Some(n) = x {
                walk_rvalue(ctx, locals, n, source, visitor, diagnostics)?
            } else {
                visitor.make_void(node.byte_range())
            };
            diagnostics.consume_node_err(node, visitor.visit_return_statement(value))
        }
    }
}

fn walk_stmt_nodes<'a, C, V>(
    ctx: &C,
    locals: &mut HashMap<String, (V::Local, LexicalDeclarationKind)>,
    break_label: Option<V::Label>,
    nodes: &[Node],
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<()>
where
    C: RefSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    let mut res = Some(());
    for &n in nodes {
        // visit all to report as many errors as possible
        let r = walk_stmt(ctx, locals, break_label, n, source, visitor, diagnostics);
        res = res.and(r);
    }
    res
}

/// Walks expression nodes recursively and returns item to be used as an rvalue.
fn walk_rvalue<'a, C, V>(
    ctx: &C,
    locals: &mut HashMap<String, (V::Local, LexicalDeclarationKind)>,
    node: Node,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<V::Item>
where
    C: RefSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    match walk_expr(ctx, locals, node, source, visitor, diagnostics)? {
        Intermediate::Item(x) => Some(x),
        Intermediate::Local(l, _) => diagnostics.consume_node_err(node, visitor.visit_local_ref(l)),
        Intermediate::BoundProperty(it, p) => diagnostics.consume_node_err(
            node,
            visitor.visit_object_property(it, p, node.byte_range()),
        ),
        Intermediate::BoundMethod(..)
        | Intermediate::BoundBuiltinMethod(..)
        | Intermediate::BuiltinFunction(_) => {
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

/// Walks expression nodes recursively and returns intermediate result.
fn walk_expr<'a, C, V>(
    ctx: &C,
    locals: &mut HashMap<String, (V::Local, LexicalDeclarationKind)>,
    node: Node,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<Intermediate<'a, V::Item, V::Local>>
where
    C: RefSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    match diagnostics.consume_err(Expression::from_node(node, source))? {
        Expression::Identifier(x) => {
            process_identifier(ctx, locals, x, source, visitor, diagnostics)
        }
        Expression::This => {
            if let Some((cls, name)) = ctx.this_object() {
                diagnostics
                    .consume_node_err(
                        node,
                        visitor.visit_object_ref(cls, &name, node.byte_range()),
                    )
                    .map(Intermediate::Item)
            } else {
                diagnostics.push(Diagnostic::error(node.byte_range(), "undefined reference"));
                None
            }
        }
        Expression::Integer(v) => diagnostics
            .consume_node_err(node, visitor.visit_integer(v, node.byte_range()))
            .map(Intermediate::Item),
        Expression::Float(v) => diagnostics
            .consume_node_err(node, visitor.visit_float(v, node.byte_range()))
            .map(Intermediate::Item),
        Expression::String(v) => diagnostics
            .consume_node_err(node, visitor.visit_string(v, node.byte_range()))
            .map(Intermediate::Item),
        Expression::Bool(v) => diagnostics
            .consume_node_err(node, visitor.visit_bool(v, node.byte_range()))
            .map(Intermediate::Item),
        Expression::Array(ns) => {
            let elements = ns
                .iter()
                .map(|&n| walk_rvalue(ctx, locals, n, source, visitor, diagnostics))
                .collect::<Option<Vec<_>>>()?;
            diagnostics
                .consume_node_err(node, visitor.visit_array(elements, node.byte_range()))
                .map(Intermediate::Item)
        }
        Expression::Member(x) => {
            match walk_expr(ctx, locals, x.object, source, visitor, diagnostics)? {
                Intermediate::Item(it) => {
                    process_item_property(it, x.property, source, diagnostics)
                }
                Intermediate::Local(l, _) => {
                    let it = diagnostics.consume_node_err(node, visitor.visit_local_ref(l))?;
                    process_item_property(it, x.property, source, diagnostics)
                }
                Intermediate::BoundProperty(it, p) => {
                    let obj = diagnostics.consume_node_err(
                        x.object,
                        visitor.visit_object_property(it, p, x.object.byte_range()),
                    )?;
                    process_item_property(obj, x.property, source, diagnostics)
                }
                Intermediate::BoundMethod(..)
                | Intermediate::BoundBuiltinMethod(..)
                | Intermediate::BuiltinFunction(_) => {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        "function has no property/method",
                    ));
                    None
                }
                Intermediate::Type(ty) => {
                    process_identifier(&ty, locals, x.property, source, visitor, diagnostics)
                }
            }
        }
        Expression::Call(x) => {
            let arguments = x
                .arguments
                .iter()
                .map(|&n| walk_rvalue(ctx, locals, n, source, visitor, diagnostics))
                .collect::<Option<Vec<_>>>()?;
            match walk_expr(ctx, locals, x.function, source, visitor, diagnostics)? {
                Intermediate::BoundMethod(it, ms) => diagnostics
                    .consume_node_err(
                        node,
                        visitor.visit_object_method_call(it, ms, arguments, node.byte_range()),
                    )
                    .map(Intermediate::Item),
                Intermediate::BoundBuiltinMethod(it, f) => diagnostics
                    .consume_node_err(
                        node,
                        visitor.visit_object_builtin_method_call(
                            it,
                            f,
                            arguments,
                            node.byte_range(),
                        ),
                    )
                    .map(Intermediate::Item),
                Intermediate::BuiltinFunction(f) => diagnostics
                    .consume_node_err(
                        node,
                        visitor.visit_builtin_call(f, arguments, node.byte_range()),
                    )
                    .map(Intermediate::Item),
                Intermediate::Item(_)
                | Intermediate::Local(..)
                | Intermediate::BoundProperty(..)
                | Intermediate::Type(_) => {
                    diagnostics.push(Diagnostic::error(x.function.byte_range(), "not callable"));
                    None
                }
            }
        }
        Expression::Assignment(x) => {
            let right = walk_rvalue(ctx, locals, x.right, source, visitor, diagnostics)?;
            match walk_expr(ctx, locals, x.left, source, visitor, diagnostics)? {
                Intermediate::Local(l, k) => match k {
                    LexicalDeclarationKind::Let => diagnostics
                        .consume_node_err(
                            node,
                            visitor.visit_local_assignment(l, right, node.byte_range()),
                        )
                        .map(Intermediate::Item),
                    LexicalDeclarationKind::Const => {
                        diagnostics.push(Diagnostic::error(
                            x.left.byte_range(),
                            "cannot assign to const variable",
                        ));
                        None
                    }
                },
                Intermediate::BoundProperty(it, p) => diagnostics
                    .consume_node_err(
                        node,
                        visitor.visit_object_property_assignment(it, p, right, node.byte_range()),
                    )
                    .map(Intermediate::Item),
                Intermediate::Item(_)
                | Intermediate::BoundMethod(..)
                | Intermediate::BoundBuiltinMethod(..)
                | Intermediate::BuiltinFunction(_)
                | Intermediate::Type(_) => {
                    diagnostics.push(Diagnostic::error(x.left.byte_range(), "not assignable"));
                    None
                }
            }
        }
        Expression::Unary(x) => {
            let argument = walk_rvalue(ctx, locals, x.argument, source, visitor, diagnostics)?;
            let unary = UnaryOp::try_from(x.operator)
                .map_err(|()| {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!("unsupported operation '{}'", x.operator),
                    ))
                })
                .ok()?;
            diagnostics
                .consume_node_err(
                    node,
                    visitor.visit_unary_expression(unary, argument, node.byte_range()),
                )
                .map(Intermediate::Item)
        }
        Expression::Binary(x) => {
            let left = walk_rvalue(ctx, locals, x.left, source, visitor, diagnostics)?;
            let right = walk_rvalue(ctx, locals, x.right, source, visitor, diagnostics)?;
            let binary = BinaryOp::try_from(x.operator)
                .map_err(|()| {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!("unsupported operation '{}'", x.operator),
                    ))
                })
                .ok()?;
            diagnostics
                .consume_node_err(
                    node,
                    visitor.visit_binary_expression(binary, left, right, node.byte_range()),
                )
                .map(Intermediate::Item)
        }
        Expression::Ternary(x) => {
            let condition = walk_rvalue(ctx, locals, x.condition, source, visitor, diagnostics)?;
            let condition_label = visitor.mark_branch_point();
            let consequence =
                walk_rvalue(ctx, locals, x.consequence, source, visitor, diagnostics)?;
            let consequence_label = visitor.mark_branch_point();
            let alternative =
                walk_rvalue(ctx, locals, x.alternative, source, visitor, diagnostics)?;
            let alternative_label = visitor.mark_branch_point();
            diagnostics
                .consume_node_err(
                    node,
                    visitor.visit_ternary_expression(
                        (condition, condition_label),
                        (consequence, consequence_label),
                        (alternative, alternative_label),
                        node.byte_range(),
                    ),
                )
                .map(Intermediate::Item)
        }
    }
}

fn process_identifier<'a, C, V>(
    ctx: &C,
    locals: &HashMap<String, (V::Local, LexicalDeclarationKind)>,
    id: Identifier,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<Intermediate<'a, V::Item, V::Local>>
where
    C: RefSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    let name = id.to_str(source);
    if let Some(&(l, k)) = locals.get(name) {
        Some(Intermediate::Local(l, k))
    } else if let Some(r) = ctx.get_ref(name) {
        match r {
            Ok(RefKind::BuiltinFunction(f)) => Some(Intermediate::BuiltinFunction(f)),
            Ok(RefKind::Type(ty)) => Some(Intermediate::Type(ty)),
            Ok(RefKind::EnumVariant(en)) => diagnostics
                .consume_node_err(
                    id.node(),
                    visitor.visit_enum(en, name, id.node().byte_range()),
                )
                .map(Intermediate::Item),
            Ok(RefKind::Object(cls)) => diagnostics
                .consume_node_err(
                    id.node(),
                    visitor.visit_object_ref(cls, name, id.node().byte_range()),
                )
                .map(Intermediate::Item),
            Ok(RefKind::ObjectProperty(obj_cls, obj_name, p)) => diagnostics
                .consume_node_err(
                    id.node(),
                    visitor.visit_object_ref(obj_cls, &obj_name, id.node().byte_range()),
                )
                .map(|item| Intermediate::BoundProperty(item, p)),
            Ok(RefKind::ObjectMethod(obj_cls, obj_name, m)) => diagnostics
                .consume_node_err(
                    id.node(),
                    visitor.visit_object_ref(obj_cls, &obj_name, id.node().byte_range()),
                )
                .map(|item| Intermediate::BoundMethod(item, m)),
            Err(e) => {
                diagnostics.push(Diagnostic::error(
                    id.node().byte_range(),
                    format!("type resolution failed: {e}"),
                ));
                None
            }
        }
    } else {
        diagnostics.push(Diagnostic::error(
            id.node().byte_range(),
            "undefined reference",
        ));
        None
    }
}

fn process_item_property<'a, T, L>(
    item: T,
    id: Identifier,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<Intermediate<'a, T, L>>
where
    T: DescribeType<'a>,
{
    let not_found = || Diagnostic::error(id.node().byte_range(), "no property/method found");
    let name = id.to_str(source);
    match item.type_desc() {
        // simple value types
        TypeDesc::ConstInteger
        | TypeDesc::Concrete(TypeKind::Just(NamedType::Primitive(
            PrimitiveType::Bool | PrimitiveType::Double | PrimitiveType::Int | PrimitiveType::Uint,
        )))
        | TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(_))) => {
            diagnostics.push(not_found());
            None
        }
        TypeDesc::ConstString | TypeDesc::STRING => match name {
            "arg" => Some(Intermediate::BoundBuiltinMethod(
                item,
                BuiltinMethodKind::Arg,
            )),
            _ => {
                diagnostics.push(not_found());
                None
            }
        },
        // gadget or object types
        TypeDesc::Concrete(TypeKind::Just(NamedType::Class(cls)))
        | TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(cls))) => {
            if let Some(r) = cls.get_property(name) {
                match r {
                    Ok(p) => Some(Intermediate::BoundProperty(item, p)),
                    Err(e) => {
                        diagnostics.push(Diagnostic::error(
                            id.node().byte_range(),
                            format!("property resolution failed: {e}"),
                        ));
                        None
                    }
                }
            } else if let Some(r) = cls.get_public_method(name) {
                match r {
                    Ok(m) => Some(Intermediate::BoundMethod(item, m)),
                    Err(e) => {
                        diagnostics.push(Diagnostic::error(
                            id.node().byte_range(),
                            format!("method resolution failed: {e}"),
                        ));
                        None
                    }
                }
            } else {
                diagnostics.push(not_found());
                None
            }
        }
        // list types
        TypeDesc::EmptyList
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
