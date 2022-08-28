//! Expression tree visitor with type information.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::opcode::{
    BinaryLogicalOp, BinaryOp, BuiltinFunctionKind, ComparisonOp, UnaryLogicalOp, UnaryOp,
};
use crate::qmlast::{
    Expression, ExpressionNode, Function, FunctionBody, Identifier, LexicalDeclarationKind,
    NestedIdentifier, Node, Statement, StatementNode,
};
use crate::typemap::{
    Class, Enum, MethodMatches, NamedType, Property, TypeKind, TypeMapError, TypeSpace,
};
use crate::typeutil;
use itertools::Itertools as _;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::num::TryFromIntError;
use std::ops::Range;
use thiserror::Error;

/// Expression type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeDesc<'a> {
    /// Integer literal or constant expression without concrete type.
    ConstInteger,
    /// String literal or constant expression without concrete type.
    ConstString,
    /// Null pointer literal without concrete type.
    NullPointer,
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
    pub const VARIANT: Self = TypeDesc::Concrete(TypeKind::VARIANT);
    pub const VOID: Self = TypeDesc::Concrete(TypeKind::VOID);

    pub fn qualified_name(&self) -> Cow<'_, str> {
        match self {
            TypeDesc::ConstInteger => "integer".into(),
            TypeDesc::ConstString => "string".into(),
            TypeDesc::NullPointer => "nullptr_t".into(),
            TypeDesc::EmptyList => "list".into(),
            TypeDesc::Concrete(k) => k.qualified_cxx_name(),
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            TypeDesc::ConstInteger | TypeDesc::ConstString | TypeDesc::EmptyList => false,
            TypeDesc::NullPointer => true,
            TypeDesc::Concrete(k) => k.is_pointer(),
        }
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

/// Context which supports type annotation lookup.
pub trait TypeAnnotationSpace<'a> {
    /// Looks up type by scoped name and wraps it with the appropriate `TypeKind`.
    fn get_annotated_type_scoped(
        &self,
        scoped_name: &str,
    ) -> Option<Result<TypeKind<'a>, TypeMapError>>;
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

    /// Creates an item that represents a valid, but not meaningful value. `return;` statement
    /// will generate this value.
    fn make_void(&self, byte_range: Range<usize>) -> Self::Item;

    fn visit_integer(
        &mut self,
        value: u64,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_float(
        &mut self,
        value: f64,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_string(
        &mut self,
        value: String,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_bool(
        &mut self,
        value: bool,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_null(&mut self, byte_range: Range<usize>) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_enum(
        &mut self,
        enum_ty: Enum<'a>,
        variant: &str,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;

    fn visit_array(
        &mut self,
        elements: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;

    fn visit_local_ref(&mut self, name: Self::Local) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_local_declaration(
        &mut self,
        ty: TypeKind<'a>,
        byte_range: Range<usize>,
    ) -> Result<Self::Local, ExpressionError<'a>>;
    fn visit_local_assignment(
        &mut self,
        name: Self::Local,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_function_parameter(
        &mut self,
        ty: TypeKind<'a>,
        byte_range: Range<usize>,
    ) -> Result<Self::Local, ExpressionError<'a>>;

    fn visit_object_ref(
        &mut self,
        cls: Class<'a>,
        name: &str,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_object_property(
        &mut self,
        object: Self::Item,
        property: Property<'a>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_object_property_assignment(
        &mut self,
        object: Self::Item,
        property: Property<'a>,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_object_subscript(
        &mut self,
        object: Self::Item,
        index: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_object_subscript_assignment(
        &mut self,
        object: Self::Item,
        index: Self::Item,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_object_method_call(
        &mut self,
        object: Self::Item,
        methods: MethodMatches<'a>,
        arguments: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;

    fn visit_builtin_call(
        &mut self,
        function: BuiltinFunctionKind,
        arguments: Vec<Self::Item>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_unary_expression(
        &mut self,
        unary: UnaryOp,
        argument: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_binary_expression(
        &mut self,
        binary: BinaryOp,
        left: Self::Item,
        right: Self::Item,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_binary_logical_expression(
        &mut self,
        op: BinaryLogicalOp,
        left: (Self::Item, Self::Label),
        right: (Self::Item, Self::Label),
        byte_range: Range<usize>,
    ) -> Self::Item;
    fn visit_as_expression(
        &mut self,
        value: Self::Item,
        ty: TypeKind<'a>,
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;
    fn visit_ternary_expression(
        &mut self,
        condition: (Self::Item, Self::Label),
        consequence: (Self::Item, Self::Label),
        alternative: (Self::Item, Self::Label),
        byte_range: Range<usize>,
    ) -> Result<Self::Item, ExpressionError<'a>>;

    fn visit_expression_statement(&mut self, value: Self::Item);
    fn visit_if_statement(
        &mut self,
        condition: (Self::Item, Self::Label),
        consequence_ref: Self::Label,
        alternative_ref: Option<Self::Label>,
    );
    fn visit_switch_statement(
        &mut self,
        case_conditions: Vec<(Self::Item, Self::Label)>,
        bodies: Vec<Self::Label>,
        default_pos: Option<usize>,
        head_ref: Self::Label,
        exit_ref: Self::Label,
    );
    fn visit_break_statement(&mut self, exit_ref: Self::Label);
    fn visit_return_statement(&mut self, value: Self::Item);

    fn mark_branch_point(&mut self) -> Self::Label;
}

#[derive(Clone, Debug, Error)]
pub enum ExpressionError<'a> {
    #[error("integer conversion failed: {0}")]
    IntegerConversion(#[from] TryFromIntError),
    #[error("integer overflow")]
    IntegerOverflow,
    #[error("type resolution failed: {0}")]
    TypeResolution(#[from] TypeMapError),
    #[error(
        "incompatible array element types at index {0}: {} and {}",
        .1.qualified_name(),
        .2.qualified_name(),
    )]
    IncompatibleArrayElementType(usize, TypeDesc<'a>, TypeDesc<'a>),
    #[error("index must be of integer type, but got: {}", .0.qualified_name())]
    IncompatibleIndexType(TypeDesc<'a>),
    #[error("invalid argument: {0}")]
    InvalidArgument(String),
    #[error(
        "operation '{0}' on incompatible types: {} and {}",
        .1.qualified_name(),
        .2.qualified_name(),
    )]
    OperationOnIncompatibleTypes(String, TypeDesc<'a>, TypeDesc<'a>),
    #[error("operation '{0}' on undetermined type: {}", .1.qualified_name())]
    OperationOnUndeterminedType(String, TypeDesc<'a>),
    #[error("operation '{0}' on unsupported type: {}", .1.qualified_name())]
    OperationOnUnsupportedType(String, TypeDesc<'a>),
    #[error("unsupported operation '{0}'")]
    UnsupportedOperation(String),
    #[error("not a readable property")]
    UnreadableProperty,
    #[error("not a writable property")]
    UnwritableProperty,
}

#[derive(Debug)]
enum Intermediate<'a, T, L> {
    Item(T),
    Local(L, LexicalDeclarationKind),
    BoundProperty(T, Property<'a>, ReceiverKind),
    BoundSubscript(T, T, ExprKind),
    BoundMethod(T, MethodMatches<'a>),
    BuiltinFunction(BuiltinFunctionKind),
    Type(NamedType<'a>),
}

/// Category of expression.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ExprKind {
    Lvalue,
    Rvalue,
}

/// Receiver category of bound property.
///
/// Object (or pointer) property is always assignable, but rvalue gadget property isn't.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ReceiverKind {
    Object,
    Gadget(ExprKind),
}

/// Walks statement nodes recursively from the specified `node`.
///
/// `ctx` is the space where an identifier expression is resolved.
pub fn walk<'a, C, V>(
    ctx: &C,
    node: StatementNode,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<()>
where
    C: RefSpace<'a> + TypeAnnotationSpace<'a>,
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
    node: StatementNode,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<()>
where
    C: RefSpace<'a> + TypeAnnotationSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    match diagnostics.consume_err(node.parse())? {
        Statement::Expression(n) => {
            let value = walk_rvalue(ctx, locals, n, source, visitor, diagnostics)?;
            visitor.visit_expression_statement(value);
            Some(())
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
                let rvalue_node = if let Some(n) = decl.value {
                    let v = walk_rvalue(ctx, locals, n, source, visitor, diagnostics)?;
                    Some((v, n))
                } else if x.kind == LexicalDeclarationKind::Const {
                    diagnostics.push(Diagnostic::error(
                        decl.node().byte_range(),
                        "const declaration must have initializer",
                    ));
                    return None;
                } else {
                    None
                };
                let ty = if let Some(n) = &decl.ty {
                    process_type_annotation(ctx, n, source, diagnostics)?
                } else if let Some((v, n)) = &rvalue_node {
                    diagnostics.consume_expr_err(*n, typeutil::to_concrete_type(v.type_desc()))?
                } else {
                    diagnostics.push(Diagnostic::error(
                        decl.node().byte_range(),
                        "variable declaration must have type annotation or initializer",
                    ));
                    return None;
                };
                let local = diagnostics.consume_node_err(
                    decl.node(),
                    visitor.visit_local_declaration(ty, decl.node().byte_range()),
                )?;
                locals.insert(decl.name.to_str(source).to_owned(), (local, x.kind));
                if let Some((v, _)) = rvalue_node {
                    diagnostics.consume_node_err(
                        decl.node(),
                        visitor.visit_local_assignment(local, v, decl.node().byte_range()),
                    )?;
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
            check_condition_type(&condition, x.condition, diagnostics)?;
            visitor.visit_if_statement(
                (condition, condition_label),
                consequence_label,
                alternative_label,
            );
            Some(())
        }
        Statement::Switch(x) => {
            // build separate set of condition blocks and body blocks so the compiler can
            // easily detect multiple branches
            let left = walk_rvalue(ctx, locals, x.value, source, visitor, diagnostics)?;
            let case_conditions: Vec<_> = x
                .cases
                .iter()
                .filter_map(|c| {
                    let right = walk_rvalue(ctx, locals, c.value, source, visitor, diagnostics)?;
                    let condition = diagnostics.consume_expr_err(
                        c.value,
                        visitor.visit_binary_expression(
                            BinaryOp::Comparison(ComparisonOp::Equal),
                            left.clone(),
                            right,
                            c.value.byte_range(),
                        ),
                    )?;
                    let condition_label = visitor.mark_branch_point();
                    Some((condition, condition_label))
                })
                .collect();
            let mut body_statements: Vec<_> = x.cases.iter().map(|c| &c.body).collect();
            let default_pos = if let Some(d) = &x.default {
                body_statements.insert(d.position, &d.body);
                Some(d.position)
            } else {
                None
            };
            // allocate empty block where "break" will be directed
            let head_ref = visitor.mark_branch_point();
            let exit_ref = visitor.mark_branch_point();
            let break_label = Some(exit_ref);
            let bodies: Vec<_> = body_statements
                .iter()
                .filter_map(|nodes| {
                    walk_stmt_nodes(
                        ctx,
                        locals,
                        break_label,
                        nodes,
                        source,
                        visitor,
                        diagnostics,
                    )?;
                    let body_label = visitor.mark_branch_point();
                    Some(body_label)
                })
                .collect();
            if x.cases.len() == case_conditions.len() && body_statements.len() == bodies.len() {
                visitor.visit_switch_statement(
                    case_conditions,
                    bodies,
                    default_pos,
                    head_ref,
                    exit_ref,
                );
                Some(())
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
                visitor.visit_break_statement(l);
                Some(())
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
            visitor.visit_return_statement(value);
            Some(())
        }
    }
}

fn walk_stmt_nodes<'a, C, V>(
    ctx: &C,
    locals: &mut HashMap<String, (V::Local, LexicalDeclarationKind)>,
    break_label: Option<V::Label>,
    nodes: &[StatementNode],
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<()>
where
    C: RefSpace<'a> + TypeAnnotationSpace<'a>,
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

/// Walks statement or unnamed function block from the specified `node`.
///
/// `ctx` is the space where an identifier expression is resolved.
pub fn walk_callback<'a, C, V>(
    ctx: &C,
    node: StatementNode,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<()>
where
    C: RefSpace<'a> + TypeAnnotationSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    match diagnostics.consume_err(node.parse())? {
        // explicitly test the node kind since any expression surrounded by parentheses
        // shouldn't be parsed as a top-level function block.
        Statement::Expression(n) if n.is_bare_function() => {
            walk_callback_function(ctx, n, source, visitor, diagnostics)
        }
        _ => walk(ctx, node, source, visitor, diagnostics),
    }
}

fn walk_callback_function<'a, C, V>(
    ctx: &C,
    node: ExpressionNode,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<()>
where
    C: RefSpace<'a> + TypeAnnotationSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    let func = diagnostics.consume_err(Function::from_node(node))?;
    if let Some(n) = func.name {
        diagnostics.push(Diagnostic::error(
            n.node().byte_range(),
            "named function isn't allowed",
        ));
        return None;
    }
    if let Some(n) = func.return_ty {
        diagnostics.push(Diagnostic::warning(
            n.node().byte_range(),
            "return type is ignored (which may be changed later)",
        ));
    }

    let mut locals = HashMap::new();
    for param in &func.parameters {
        let name = param.name.to_str(source);
        if locals.contains_key(name) {
            diagnostics.push(Diagnostic::error(
                param.name.node().byte_range(),
                format!("redefinition of parameter: {name}"),
            ));
        } else if let Some(n) = &param.ty {
            let ty = process_type_annotation(ctx, n, source, diagnostics)?;
            let local = diagnostics.consume_node_err(
                param.node(),
                visitor.visit_function_parameter(ty, param.node().byte_range()),
            )?;
            locals.insert(name.to_owned(), (local, LexicalDeclarationKind::Let));
        } else {
            diagnostics.push(Diagnostic::error(
                param.node().byte_range(),
                "function parameter must have type annotation",
            ));
        }
    }
    if locals.len() != func.parameters.len() {
        return None;
    }

    match func.body {
        FunctionBody::Expression(n) => {
            let value = walk_rvalue(ctx, &mut locals, n, source, visitor, diagnostics)?;
            visitor.visit_expression_statement(value);
            Some(())
        }
        FunctionBody::Statement(n) => {
            walk_stmt(ctx, &mut locals, None, n, source, visitor, diagnostics)
        }
    }
}

/// Walks expression nodes recursively and returns item to be used as an rvalue.
fn walk_rvalue<'a, C, V>(
    ctx: &C,
    locals: &mut HashMap<String, (V::Local, LexicalDeclarationKind)>,
    node: ExpressionNode,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<V::Item>
where
    C: RefSpace<'a> + TypeAnnotationSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    match walk_expr(ctx, locals, node, source, visitor, diagnostics)? {
        Intermediate::Item(x) => Some(x),
        Intermediate::Local(l, _) => diagnostics.consume_expr_err(node, visitor.visit_local_ref(l)),
        Intermediate::BoundProperty(it, p, _) => diagnostics.consume_expr_err(
            node,
            visitor.visit_object_property(it, p, node.byte_range()),
        ),
        Intermediate::BoundSubscript(it, i, _) => diagnostics.consume_expr_err(
            node,
            visitor.visit_object_subscript(it, i, node.byte_range()),
        ),
        Intermediate::BoundMethod(..) | Intermediate::BuiltinFunction(_) => {
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
    node: ExpressionNode,
    source: &str,
    visitor: &mut V,
    diagnostics: &mut Diagnostics,
) -> Option<Intermediate<'a, V::Item, V::Local>>
where
    C: RefSpace<'a> + TypeAnnotationSpace<'a>,
    V: ExpressionVisitor<'a>,
{
    match diagnostics.consume_err(node.parse(source))? {
        Expression::Identifier(x) => {
            process_identifier(ctx, locals, x, source, visitor, diagnostics)
        }
        Expression::This => {
            if let Some((cls, name)) = ctx.this_object() {
                diagnostics
                    .consume_expr_err(
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
            .consume_expr_err(node, visitor.visit_integer(v, node.byte_range()))
            .map(Intermediate::Item),
        Expression::Float(v) => diagnostics
            .consume_expr_err(node, visitor.visit_float(v, node.byte_range()))
            .map(Intermediate::Item),
        Expression::String(v) => diagnostics
            .consume_expr_err(node, visitor.visit_string(v, node.byte_range()))
            .map(Intermediate::Item),
        Expression::Bool(v) => diagnostics
            .consume_expr_err(node, visitor.visit_bool(v, node.byte_range()))
            .map(Intermediate::Item),
        Expression::Null => diagnostics
            .consume_expr_err(node, visitor.visit_null(node.byte_range()))
            .map(Intermediate::Item),
        Expression::Array(ns) => {
            let elements = ns
                .iter()
                .map(|&n| walk_rvalue(ctx, locals, n, source, visitor, diagnostics))
                .collect::<Option<Vec<_>>>()?;
            match visitor.visit_array(elements, node.byte_range()) {
                Ok(it) => Some(Intermediate::Item(it)),
                Err(e) => {
                    let mut diag = Diagnostic::error(node.byte_range(), e.to_string());
                    if let ExpressionError::IncompatibleArrayElementType(i, l, r) = &e {
                        typeutil::diagnose_incompatible_types(
                            &mut diag,
                            ns[*i - 1].byte_range(),
                            l,
                            ns[*i].byte_range(),
                            r,
                        );
                    }
                    diagnostics.push(diag);
                    None
                }
            }
        }
        Expression::Function(_) | Expression::ArrowFunction(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                "unsupported expression",
            ));
            None
        }
        Expression::Member(x) => {
            match walk_expr(ctx, locals, x.object, source, visitor, diagnostics)? {
                Intermediate::Item(it) => {
                    process_item_property(it, x.property, ExprKind::Rvalue, source, diagnostics)
                }
                Intermediate::Local(l, _) => {
                    let it = diagnostics.consume_expr_err(node, visitor.visit_local_ref(l))?;
                    process_item_property(it, x.property, ExprKind::Lvalue, source, diagnostics)
                }
                Intermediate::BoundProperty(it, p, _) => {
                    let obj = diagnostics.consume_expr_err(
                        x.object,
                        visitor.visit_object_property(it, p, x.object.byte_range()),
                    )?;
                    process_item_property(obj, x.property, ExprKind::Rvalue, source, diagnostics)
                }
                Intermediate::BoundSubscript(it, i, _) => {
                    let obj = diagnostics.consume_expr_err(
                        x.object,
                        visitor.visit_object_subscript(it, i, x.object.byte_range()),
                    )?;
                    process_item_property(obj, x.property, ExprKind::Rvalue, source, diagnostics)
                }
                Intermediate::BoundMethod(..) | Intermediate::BuiltinFunction(_) => {
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
        Expression::Subscript(x) => {
            let (obj, k) = match walk_expr(ctx, locals, x.object, source, visitor, diagnostics)? {
                Intermediate::Item(it) => (it, ExprKind::Rvalue),
                Intermediate::Local(l, _) => {
                    let it = diagnostics.consume_expr_err(node, visitor.visit_local_ref(l))?;
                    (it, ExprKind::Lvalue)
                }
                Intermediate::BoundProperty(it, p, _) => {
                    let obj = diagnostics.consume_expr_err(
                        x.object,
                        visitor.visit_object_property(it, p, x.object.byte_range()),
                    )?;
                    (obj, ExprKind::Rvalue)
                }
                Intermediate::BoundSubscript(it, i, _) => {
                    let obj = diagnostics.consume_expr_err(
                        x.object,
                        visitor.visit_object_subscript(it, i, x.object.byte_range()),
                    )?;
                    (obj, ExprKind::Rvalue)
                }
                Intermediate::BoundMethod(..) | Intermediate::BuiltinFunction(_) => {
                    diagnostics.push(Diagnostic::error(
                        x.object.byte_range(),
                        "bare function reference",
                    ));
                    return None;
                }
                Intermediate::Type(_) => {
                    diagnostics.push(Diagnostic::error(
                        x.object.byte_range(),
                        "bare type reference",
                    ));
                    return None;
                }
            };
            let index = walk_rvalue(ctx, locals, x.index, source, visitor, diagnostics)?;
            Some(Intermediate::BoundSubscript(obj, index, k))
        }
        Expression::Call(x) => {
            let arguments = x
                .arguments
                .iter()
                .map(|&n| walk_rvalue(ctx, locals, n, source, visitor, diagnostics))
                .collect::<Option<Vec<_>>>()?;
            match walk_expr(ctx, locals, x.function, source, visitor, diagnostics)? {
                Intermediate::BoundMethod(it, ms) => diagnostics
                    .consume_expr_err(
                        node,
                        visitor.visit_object_method_call(it, ms, arguments, node.byte_range()),
                    )
                    .map(Intermediate::Item),
                Intermediate::BuiltinFunction(f) => diagnostics
                    .consume_expr_err(
                        node,
                        visitor.visit_builtin_call(f, arguments, node.byte_range()),
                    )
                    .map(Intermediate::Item),
                Intermediate::Item(_)
                | Intermediate::Local(..)
                | Intermediate::BoundProperty(..)
                | Intermediate::BoundSubscript(..)
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
                        .consume_expr_err(
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
                Intermediate::BoundProperty(
                    it,
                    p,
                    ReceiverKind::Object | ReceiverKind::Gadget(ExprKind::Lvalue),
                ) => diagnostics
                    .consume_expr_err(
                        node,
                        visitor.visit_object_property_assignment(it, p, right, node.byte_range()),
                    )
                    .map(Intermediate::Item),
                Intermediate::BoundProperty(_, _, ReceiverKind::Gadget(ExprKind::Rvalue)) => {
                    diagnostics.push(Diagnostic::error(
                        x.left.byte_range(),
                        "rvalue gadget property is not assignable",
                    ));
                    None
                }
                Intermediate::BoundSubscript(it, i, ExprKind::Lvalue) => diagnostics
                    .consume_expr_err(
                        node,
                        visitor.visit_object_subscript_assignment(it, i, right, node.byte_range()),
                    )
                    .map(Intermediate::Item),
                Intermediate::BoundSubscript(_, _, ExprKind::Rvalue) => {
                    diagnostics.push(Diagnostic::error(
                        x.left.byte_range(),
                        "rvalue subscript is not assignable",
                    ));
                    None
                }
                Intermediate::Item(_)
                | Intermediate::BoundMethod(..)
                | Intermediate::BuiltinFunction(_)
                | Intermediate::Type(_) => {
                    diagnostics.push(Diagnostic::error(x.left.byte_range(), "not assignable"));
                    None
                }
            }
        }
        Expression::Unary(x) => {
            let argument = walk_rvalue(ctx, locals, x.argument, source, visitor, diagnostics)?;
            let argument_t = argument.type_desc();
            let unary = UnaryOp::try_from(x.operator)
                .map_err(|()| {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!("unsupported operation '{}'", x.operator),
                    ))
                })
                .ok()?;
            match visitor.visit_unary_expression(unary, argument, node.byte_range()) {
                Ok(it) => Some(Intermediate::Item(it)),
                Err(e) => {
                    let mut diag = Diagnostic::error(node.byte_range(), e.to_string());
                    if unary == UnaryOp::Logical(UnaryLogicalOp::Not)
                        && matches!(&e, ExpressionError::UnsupportedOperation(_))
                    {
                        typeutil::diagnose_bool_conversion(
                            &mut diag,
                            x.argument.byte_range(),
                            &argument_t,
                            false, // truthy
                        );
                    }
                    diagnostics.push(diag);
                    None
                }
            }
        }
        Expression::Binary(x) => {
            let binary = BinaryOp::try_from(x.operator)
                .map_err(|()| {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!("unsupported operation '{}'", x.operator),
                    ))
                })
                .ok()?;
            match binary {
                BinaryOp::Arith(_)
                | BinaryOp::Bitwise(_)
                | BinaryOp::Shift(_)
                | BinaryOp::Comparison(_) => {
                    let left = walk_rvalue(ctx, locals, x.left, source, visitor, diagnostics)?;
                    let right = walk_rvalue(ctx, locals, x.right, source, visitor, diagnostics)?;
                    match visitor.visit_binary_expression(binary, left, right, node.byte_range()) {
                        Ok(it) => Some(Intermediate::Item(it)),
                        Err(e) => {
                            let mut diag = Diagnostic::error(node.byte_range(), e.to_string());
                            if let ExpressionError::OperationOnIncompatibleTypes(_, l, r) = &e {
                                typeutil::diagnose_incompatible_types(
                                    &mut diag,
                                    x.left.byte_range(),
                                    l,
                                    x.right.byte_range(),
                                    r,
                                );
                            }
                            diagnostics.push(diag);
                            None
                        }
                    }
                }
                BinaryOp::Logical(op) => {
                    let left = walk_rvalue(ctx, locals, x.left, source, visitor, diagnostics)?;
                    let left_label = visitor.mark_branch_point();
                    let right = walk_rvalue(ctx, locals, x.right, source, visitor, diagnostics)?;
                    let right_label = visitor.mark_branch_point();
                    check_condition_type(&left, x.left, diagnostics)?;
                    check_condition_type(&right, x.right, diagnostics)?;
                    let it = visitor.visit_binary_logical_expression(
                        op,
                        (left, left_label),
                        (right, right_label),
                        node.byte_range(),
                    );
                    Some(Intermediate::Item(it))
                }
            }
        }
        Expression::As(x) => {
            let value = walk_rvalue(ctx, locals, x.value, source, visitor, diagnostics)?;
            let ty = process_type_annotation(ctx, &x.ty, source, diagnostics)?;
            diagnostics
                .consume_expr_err(
                    node,
                    visitor.visit_as_expression(value, ty, node.byte_range()),
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
            check_condition_type(&condition, x.condition, diagnostics)?;
            match visitor.visit_ternary_expression(
                (condition, condition_label),
                (consequence, consequence_label),
                (alternative, alternative_label),
                node.byte_range(),
            ) {
                Ok(it) => Some(Intermediate::Item(it)),
                Err(e) => {
                    let mut diag = Diagnostic::error(node.byte_range(), e.to_string());
                    if let ExpressionError::OperationOnIncompatibleTypes(_, l, r) = &e {
                        typeutil::diagnose_incompatible_types(
                            &mut diag,
                            x.consequence.byte_range(),
                            l,
                            x.alternative.byte_range(),
                            r,
                        );
                    };
                    diagnostics.push(diag);
                    None
                }
            }
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
                .map(|item| Intermediate::BoundProperty(item, p, ReceiverKind::Object)),
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
    item_kind: ExprKind,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<Intermediate<'a, T, L>>
where
    T: DescribeType<'a>,
{
    let name = id.to_str(source);
    let not_found = || {
        Diagnostic::error(
            id.node().byte_range(),
            format!(
                "property/method named '{name}' not found in type '{type_name}'",
                type_name = item.type_desc().qualified_name(),
            ),
        )
    };
    let ty = match typeutil::to_concrete_type(item.type_desc()) {
        Ok(ty) => ty,
        Err(e) => {
            diagnostics.push(Diagnostic::error(id.node().byte_range(), e.to_string()));
            return None;
        }
    };
    let is_pointer = ty.is_pointer();
    if let Some(cls) = ty.into_class() {
        let k = if is_pointer {
            ReceiverKind::Object
        } else {
            ReceiverKind::Gadget(item_kind)
        };
        if let Some(r) = cls.get_property(name) {
            match r {
                Ok(p) => Some(Intermediate::BoundProperty(item, p, k)),
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
    } else {
        diagnostics.push(not_found());
        None
    }
}

fn process_type_annotation<'a, C>(
    ctx: &C,
    id: &NestedIdentifier,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<TypeKind<'a>>
where
    C: TypeAnnotationSpace<'a>,
{
    // TODO: look up qualified name without joining
    let scoped_name = id.components().iter().map(|n| n.to_str(source)).join("::");
    match ctx.get_annotated_type_scoped(&scoped_name) {
        Some(Ok(ty)) => Some(ty),
        Some(Err(e)) => {
            diagnostics.push(Diagnostic::error(
                id.node().byte_range(),
                format!("type resolution failed: {e}"),
            ));
            None
        }
        None => {
            diagnostics.push(Diagnostic::error(id.node().byte_range(), "undefined type"));
            None
        }
    }
}

fn check_condition_type<'a, T>(
    condition: &T,
    node: ExpressionNode,
    diagnostics: &mut Diagnostics,
) -> Option<()>
where
    T: DescribeType<'a>,
{
    match condition.type_desc() {
        TypeDesc::BOOL => Some(()),
        t => {
            let mut diag = Diagnostic::error(
                node.byte_range(),
                format!(
                    "condition must be of bool type, but got: {}",
                    t.qualified_name()
                ),
            );
            typeutil::diagnose_bool_conversion(
                &mut diag,
                node.byte_range(),
                &t,
                true, // truthy
            );
            diagnostics.push(diag);
            None
        }
    }
}

// TODO: maybe rewrite this extension methods with a plain function?
impl Diagnostics {
    fn consume_expr_err<T, E>(&mut self, node: ExpressionNode, result: Result<T, E>) -> Option<T>
    where
        E: ToString,
    {
        self.consume_err(result.map_err(|e| Diagnostic::error(node.byte_range(), e.to_string())))
    }

    fn consume_node_err<T, E>(&mut self, node: Node, result: Result<T, E>) -> Option<T>
    where
        E: ToString,
    {
        self.consume_err(result.map_err(|e| Diagnostic::error(node.byte_range(), e.to_string())))
    }
}
