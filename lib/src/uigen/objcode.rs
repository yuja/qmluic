//! Code storage for UI object.

use super::context::ObjectContext;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectNode;
use crate::qmlast::{Node, UiBindingMap, UiBindingValue};
use crate::tir::{self, CodeBody};
use crate::typemap::{Class, NamedType, Property, TypeKind, TypeSpace as _};
use std::collections::HashMap;

/// Stores codes found in UI object definition.
#[derive(Clone, Debug)]
pub(super) struct ObjectCodeMap<'a, 't, 's> {
    properties: HashMap<&'s str, PropertyCode<'a, 't, 's>>,
    // TODO: map of onSignal callbacks
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
        let properties = build_properties_map(ctx, obj_node.class(), &binding_map, diagnostics);
        ObjectCodeMap { properties }
    }

    /// Map of property name to binding code.
    pub fn properties(&self) -> &HashMap<&'s str, PropertyCode<'a, 't, 's>> {
        &self.properties
    }
}

fn build_properties_map<'a, 't, 's>(
    ctx: &ObjectContext<'a, 't, '_>,
    cls: &Class<'a>,
    binding_map: &UiBindingMap<'t, 's>,
    diagnostics: &mut Diagnostics,
) -> HashMap<&'s str, PropertyCode<'a, 't, 's>> {
    binding_map
        .iter()
        .filter_map(|(&name, value)| {
            match cls.get_property(name) {
                Some(Ok(desc)) => PropertyCode::build(ctx, desc, value, diagnostics),
                Some(Err(e)) => {
                    diagnostics.push(Diagnostic::error(
                        value.binding_node().byte_range(),
                        format!("property resolution failed: {e}"),
                    ));
                    None
                }
                None => {
                    diagnostics.push(Diagnostic::error(
                        value.binding_node().byte_range(),
                        format!(
                            "unknown property of class '{}': {}",
                            cls.qualified_cxx_name(),
                            name
                        ),
                    ));
                    None
                }
            }
            .map(|code| (name, code))
        })
        .collect()
}

/// Property binding code or map with its description.
#[derive(Clone, Debug)]
pub(super) struct PropertyCode<'a, 't, 's> {
    desc: Property<'a>,
    node: Node<'t>,
    kind: PropertyCodeKind<'a, 't, 's>,
}

impl<'a, 't, 's> PropertyCode<'a, 't, 's> {
    pub fn build(
        ctx: &ObjectContext<'a, 't, '_>,
        desc: Property<'a>,
        value: &UiBindingValue<'t, 's>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let node = value.node();
        let kind = match desc.value_type() {
            Ok(ty) => PropertyCodeKind::build(ctx, ty, value, diagnostics)?,
            Err(e) => {
                diagnostics.push(Diagnostic::error(
                    value.binding_node().byte_range(),
                    format!("unresolved property type: {}", e),
                ));
                return None;
            }
        };
        Some(PropertyCode { desc, node, kind })
    }

    pub fn desc(&self) -> &Property<'a> {
        &self.desc
    }

    pub fn node(&self) -> Node<'t> {
        self.node
    }

    pub fn kind(&self) -> &PropertyCodeKind<'a, 't, 's> {
        &self.kind
    }
}

/// Variant for property binding codes.
#[derive(Clone, Debug)]
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
                let code = tir::build(ctx, *n, ctx.source, diagnostics)?;
                Some(PropertyCodeKind::Expr(ty, code))
            }
            UiBindingValue::Map(n, m) => match ty {
                TypeKind::Just(NamedType::Class(cls)) => {
                    let map = build_properties_map(ctx, &cls, m, diagnostics);
                    Some(PropertyCodeKind::GadgetMap(cls, map))
                }
                TypeKind::Pointer(NamedType::Class(cls)) => {
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
