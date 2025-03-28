//! Code storage for UI object.

use super::context::ObjectContext;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectNode;
use crate::qmlast::{Node, UiBindingMap, UiBindingValue};
use crate::qtname;
use crate::tir::interpret::EvaluatedValue;
use crate::tir::{self, CodeBody};
use crate::typemap::{
    Class, Method, MethodKind, MethodMatches, NamedType, Property, TypeKind, TypeSpace as _,
};
use crate::typeutil;
use once_cell::sync::OnceCell;
use std::collections::{hash_map, HashMap};
use std::iter::FusedIterator;

/// Stores codes found in UI object definition.
#[derive(Debug)]
pub(super) struct ObjectCodeMap<'a, 't, 's> {
    properties: HashMap<&'s str, PropertyCode<'a, 't, 's>>,
    callbacks: Vec<CallbackCode<'a, 't>>,
    attached_properties:
        HashMap<Class<'a>, (Class<'a>, HashMap<&'s str, PropertyCode<'a, 't, 's>>)>,
}

impl<'a, 't, 's> ObjectCodeMap<'a, 't, 's> {
    pub fn build(
        ctx: &ObjectContext<'a, 't, 's>,
        obj_node: ObjectNode<'a, 't, '_>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let binding_map = diagnostics
            .consume_err(obj_node.obj().build_binding_map(ctx.source))
            .unwrap_or_default();
        let (properties, callbacks) =
            build_properties_callbacks(ctx, obj_node.class(), &binding_map, diagnostics);

        let attached_type_map = diagnostics
            .consume_err(obj_node.obj().build_attached_type_map(ctx.source))
            .unwrap_or_default();
        let attached_properties = attached_type_map
            .iter()
            .filter_map(|(names, (id, binding_map))| {
                // TODO: look up qualified name without joining
                let type_name = names.join("::");
                let (cls, acls) = resolve_attached_class(ctx, id.node(), &type_name, diagnostics)?;
                let props = build_properties_map(ctx, &acls, binding_map, diagnostics);
                Some((cls, (acls, props)))
            })
            .collect();

        ObjectCodeMap {
            properties,
            callbacks,
            attached_properties,
        }
    }

    /// Map of property name to binding code.
    pub fn properties(&self) -> &HashMap<&'s str, PropertyCode<'a, 't, 's>> {
        &self.properties
    }

    /// List of signal callback codes.
    pub fn callbacks(&self) -> &[CallbackCode<'a, 't>] {
        &self.callbacks
    }

    /// Map of attaching class to attached properties map.
    pub fn all_attached_properties(
        &self,
    ) -> &HashMap<Class<'a>, (Class<'a>, HashMap<&'s str, PropertyCode<'a, 't, 's>>)> {
        &self.attached_properties
    }

    /// Looks up attached class and properties by the attaching class.
    pub fn attached_properties(
        &self,
        cls: &Class<'a>,
    ) -> Option<&(Class<'a>, HashMap<&'s str, PropertyCode<'a, 't, 's>>)> {
        self.attached_properties.get(cls)
    }

    /// Iterates `CodeBody` stored in this map.
    pub fn code_bodies<'m>(&'m self) -> impl Iterator<Item = &'m CodeBody<'a>> {
        let a = PropertyCodeBodies::new(&self.properties);
        let b = self.callbacks.iter().map(|c| &c.code);
        let c = self
            .attached_properties
            .values()
            .flat_map(|(_, m)| PropertyCodeBodies::new(m));
        a.chain(b).chain(c)
    }
}

fn build_properties_callbacks<'a, 't, 's>(
    ctx: &ObjectContext<'a, 't, '_>,
    cls: &Class<'a>,
    binding_map: &UiBindingMap<'t, 's>,
    diagnostics: &mut Diagnostics,
) -> (
    HashMap<&'s str, PropertyCode<'a, 't, 's>>,
    Vec<CallbackCode<'a, 't>>,
) {
    let mut properties = HashMap::new();
    let mut callbacks = Vec::new();
    for (&name, value) in binding_map {
        let cb_name = qtname::callback_to_signal_name(name);
        if let Some(r) = cls.get_property(name) {
            match r {
                Ok(desc) => {
                    if let Some(p) = PropertyCode::build(ctx, desc, value, diagnostics) {
                        properties.insert(name, p);
                    }
                }
                Err(e) => {
                    diagnostics.push(Diagnostic::error(
                        value.binding_node().byte_range(),
                        format!("property resolution failed: {e}"),
                    ));
                }
            }
        } else if let Some(r) = cb_name.as_ref().and_then(|n| cls.get_public_method(n)) {
            match r.map(uniquify_methods) {
                Ok(Some(desc)) if desc.kind() == MethodKind::Signal => {
                    if let Some(c) = CallbackCode::build(ctx, desc, value, diagnostics) {
                        callbacks.push(c);
                    }
                }
                Ok(Some(_)) => {
                    diagnostics.push(Diagnostic::error(
                        value.binding_node().byte_range(),
                        "not a signal",
                    ));
                }
                Ok(None) => {
                    diagnostics.push(Diagnostic::error(
                        value.binding_node().byte_range(),
                        "cannot bind to overloaded signal",
                    ));
                }
                Err(e) => {
                    diagnostics.push(Diagnostic::error(
                        value.binding_node().byte_range(),
                        format!("signal resolution failed: {e}"),
                    ));
                }
            }
        } else {
            let class_name = cls.qualified_cxx_name();
            let msg = if let Some(n) = cb_name.as_ref() {
                format!("unknown signal of class '{}': {}", class_name, n)
            } else {
                format!("unknown property of class '{}': {}", class_name, name)
            };
            diagnostics.push(Diagnostic::error(value.binding_node().byte_range(), msg));
        }
    }
    (properties, callbacks)
}

fn build_properties_map<'a, 't, 's>(
    ctx: &ObjectContext<'a, 't, '_>,
    cls: &Class<'a>,
    binding_map: &UiBindingMap<'t, 's>,
    diagnostics: &mut Diagnostics,
) -> HashMap<&'s str, PropertyCode<'a, 't, 's>> {
    let (properties, callbacks) = build_properties_callbacks(ctx, cls, binding_map, diagnostics);
    for c in &callbacks {
        diagnostics.push(Diagnostic::error(
            c.binding_node().byte_range(),
            "attached/nested/gadget callback is not supported",
        ));
    }
    properties
}

fn resolve_attached_class<'a>(
    ctx: &ObjectContext<'a, '_, '_>,
    node: Node,
    type_name: &str,
    diagnostics: &mut Diagnostics,
) -> Option<(Class<'a>, Class<'a>)> {
    let cls = match ctx.type_space.get_type_scoped(type_name) {
        Some(Ok(ty)) => {
            if let Some(cls) = ty.into_class() {
                cls
            } else {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!("invalid attaching type: {type_name}"),
                ));
                return None;
            }
        }
        Some(Err(e)) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!("attaching type resolution failed: {e}"),
            ));
            return None;
        }
        None => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!("unknown attaching type: {type_name}"),
            ));
            return None;
        }
    };
    let acls = match cls.attached_class() {
        Some(Ok(cls)) => cls,
        Some(Err(e)) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!("attached type resolution failed: {e}"),
            ));
            return None;
        }
        None => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!("no attached type for type: {type_name}"),
            ));
            return None;
        }
    };
    Some((cls, acls))
}

fn uniquify_methods(methods: MethodMatches) -> Option<Method> {
    match methods {
        MethodMatches::Unique(m) => Some(m),
        MethodMatches::Overloaded(mut meths) => {
            // If a function has default parameters, more than one metatype entries are
            // generated: e.g. clicked(bool = false) -> [clicked(), clicked(bool)].
            // There's no way to discriminate them from true overloaded functions, and
            // default parameters are common in button/action signals. Therefore, we assume
            // that two functions are identical if their leading arguments are the same.
            meths.sort_by_key(|m| -(m.arguments_len() as isize));
            let mut known = meths.pop().expect("method matches should not be empty");
            while let Some(m) = meths.pop() {
                if known.kind() == m.kind()
                    && known.return_type() == m.return_type()
                    && m.argument_types().starts_with(known.argument_types())
                {
                    known = m;
                } else {
                    return None;
                }
            }
            Some(known)
        }
    }
}

/// Property binding code or map with its description.
///
/// This keeps track of constant evaluation state, which shouldn't be updated after
/// cloning the `PropertyCode` storage. Therefore, it does not implement `Clone`.
#[derive(Debug)]
pub(super) struct PropertyCode<'a, 't, 's> {
    desc: Property<'a>,
    node: Node<'t>,
    kind: PropertyCodeKind<'a, 't, 's>,
    evaluated_value: OnceCell<Option<EvaluatedValue>>,
}

impl<'a, 't, 's> PropertyCode<'a, 't, 's> {
    pub fn build(
        ctx: &ObjectContext<'a, 't, '_>,
        desc: Property<'a>,
        value: &UiBindingValue<'t, 's>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let kind = PropertyCodeKind::build(ctx, desc.value_type().clone(), value, diagnostics)?;
        Some(PropertyCode {
            desc,
            node: value.node(),
            kind,
            evaluated_value: OnceCell::new(),
        })
    }

    pub fn desc(&self) -> &Property<'a> {
        &self.desc
    }

    pub fn node(&self) -> Node<'t> {
        self.node
    }

    pub fn binding_node(&self) -> Node<'t> {
        // see UiBindingValue::binding_node()
        self.node
            .parent()
            .expect("binding value node should have parent")
    }

    pub fn kind(&self) -> &PropertyCodeKind<'a, 't, 's> {
        &self.kind
    }

    /// Whether or not this has been successfully evaluated to a constant value.
    ///
    /// If this is a map, returns true only if all descendant properties are constants.
    pub fn is_evaluated_constant(&self) -> bool {
        match &self.kind {
            PropertyCodeKind::Expr(..) => self
                .evaluated_value
                .get()
                .map(|v| v.is_some())
                .unwrap_or(false),
            PropertyCodeKind::GadgetMap(_, map) | PropertyCodeKind::ObjectMap(_, map) => {
                map.values().all(|p| p.is_evaluated_constant())
            }
        }
    }

    /// Evaluates the code once as constant expression, caches the evaluated value.
    ///
    /// If this is a map, returns None.
    pub fn evaluate(&self) -> Option<EvaluatedValue> {
        self.evaluated_value
            .get_or_init(|| self.evaluate_uncached())
            .clone()
    }

    fn evaluate_uncached(&self) -> Option<EvaluatedValue> {
        match &self.kind {
            PropertyCodeKind::Expr(_, code) => tir::evaluate_code(code),
            PropertyCodeKind::GadgetMap(..) | PropertyCodeKind::ObjectMap(..) => None,
        }
    }
}

/// Variant for property binding codes.
#[derive(Debug)]
pub(super) enum PropertyCodeKind<'a, 't, 's> {
    /// Value expression.
    ///
    /// The return type of the code is not verified.
    Expr(TypeKind<'a>, CodeBody<'a>),
    /// Map of gadget (or value object) properties.
    GadgetMap(Class<'a>, HashMap<&'s str, PropertyCode<'a, 't, 's>>),
    /// Map of object properties.
    ObjectMap(Class<'a>, HashMap<&'s str, PropertyCode<'a, 't, 's>>),
}

impl<'a, 't, 's> PropertyCodeKind<'a, 't, 's> {
    pub fn build(
        ctx: &ObjectContext<'a, 't, '_>,
        ty: TypeKind<'a>,
        value: &UiBindingValue<'t, 's>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match value {
            UiBindingValue::Node(n) => {
                // TODO: wanna verify return type, but can't because static uigen has
                // various implicit conversion rules (e.g. string -> color.)
                let mut code = tir::build(ctx, *n, ctx.source, diagnostics)?;
                tir::analyze_code_property_dependency(&mut code, diagnostics);
                Some(PropertyCodeKind::Expr(ty, code))
            }
            UiBindingValue::Map(n, m) => match ty {
                TypeKind::Just(NamedType::Class(cls)) => {
                    let map = build_properties_map(ctx, &cls, m, diagnostics);
                    Some(PropertyCodeKind::GadgetMap(cls, map))
                }
                TypeKind::Pointer(NamedType::Class(cls)) => {
                    // TODO: handle callback on nested object
                    let map = build_properties_map(ctx, &cls, m, diagnostics);
                    Some(PropertyCodeKind::ObjectMap(cls, map))
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        n.byte_range(),
                        format!(
                            "binding map cannot be parsed as non-class type '{}'",
                            ty.qualified_cxx_name()
                        ),
                    ));
                    None
                }
            },
        }
    }
}

/// Recursive iterator over `CodeBody` of properties stored in `ObjectCodeMap`.
#[derive(Clone, Debug)]
pub(super) struct PropertyCodeBodies<'a, 't, 's, 'm> {
    stack: Vec<hash_map::Values<'m, &'s str, PropertyCode<'a, 't, 's>>>,
}

impl<'a, 't, 's, 'm> PropertyCodeBodies<'a, 't, 's, 'm> {
    fn new(map: &'m HashMap<&'s str, PropertyCode<'a, 't, 's>>) -> Self {
        PropertyCodeBodies {
            stack: vec![map.values()],
        }
    }
}

impl<'a, 'm> Iterator for PropertyCodeBodies<'a, '_, '_, 'm> {
    type Item = &'m CodeBody<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(iter) = self.stack.last_mut() {
            if let Some(p) = iter.next() {
                match &p.kind {
                    PropertyCodeKind::Expr(_, code) => return Some(code),
                    PropertyCodeKind::GadgetMap(_, map) | PropertyCodeKind::ObjectMap(_, map) => {
                        self.stack.push(map.values())
                    }
                }
            } else {
                self.stack.pop();
            }
        }
        None
    }
}

impl FusedIterator for PropertyCodeBodies<'_, '_, '_, '_> {}

/// Signal callback code with its description.
#[derive(Clone, Debug)]
pub(super) struct CallbackCode<'a, 't> {
    desc: Method<'a>,
    node: Node<'t>,
    code: CodeBody<'a>,
}

impl<'a, 't> CallbackCode<'a, 't> {
    pub fn build(
        ctx: &ObjectContext<'a, 't, '_>,
        desc: Method<'a>,
        value: &UiBindingValue<'t, '_>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let code = match value {
            UiBindingValue::Node(n) => tir::build_callback(ctx, *n, ctx.source, diagnostics)?,
            UiBindingValue::Map(n, _) => {
                diagnostics.push(Diagnostic::error(
                    n.byte_range(),
                    "signal callback cannot be a map",
                ));
                return None;
            }
        };
        if !verify_callback_parameter_type(&desc, &code, diagnostics) {
            return None;
        }
        Some(CallbackCode {
            desc,
            node: value.node(),
            code,
        })
    }

    pub fn desc(&self) -> &Method<'a> {
        &self.desc
    }

    /*
    pub fn node(&self) -> Node<'t> {
        self.node
    }
    */

    pub fn binding_node(&self) -> Node<'t> {
        // see UiBindingValue::binding_node()
        self.node
            .parent()
            .expect("binding value node should have parent")
    }

    pub fn code(&self) -> &CodeBody<'a> {
        &self.code
    }
}

#[must_use]
fn verify_callback_parameter_type(
    desc: &Method,
    code: &CodeBody,
    diagnostics: &mut Diagnostics,
) -> bool {
    if code.parameter_count > desc.arguments_len() {
        let s = code.locals[desc.arguments_len()].byte_range.start;
        let e = code.locals[code.parameter_count - 1].byte_range.end;
        diagnostics.push(Diagnostic::error(
            s..e,
            format!(
                "too many callback arguments (expected: 0..{}, actual: {})",
                desc.arguments_len(),
                code.parameter_count,
            ),
        ));
        return false;
    }

    let incompatible_args: Vec<_> = desc
        .argument_types()
        .iter()
        .zip(&code.locals[..code.parameter_count])
        .filter(|(ty, a)| !typeutil::is_concrete_assignable(&a.ty, ty).unwrap_or(false))
        .collect();
    for (ty, a) in &incompatible_args {
        let msg = format!(
            "incompatible callback arguments (expected: {}, actual: {})",
            ty.qualified_cxx_name(),
            a.ty.qualified_cxx_name(),
        );
        diagnostics.push(Diagnostic::error(a.byte_range.clone(), msg));
    }
    incompatible_args.is_empty()
}
