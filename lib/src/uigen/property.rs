use super::expr::Value;
use super::{BuildDocContext, XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Node, UiBindingMap, UiBindingValue};
use crate::typemap::{Class, TypeSpace};
use itertools::Itertools as _;
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::fmt::Debug;
use std::io;
use std::ops::Range;
use thiserror::Error;

/// Parsed property value with its AST node.
#[derive(Clone, Copy, Debug)]
pub(super) struct WithNode<'t, V> {
    node: Node<'t>,
    value: V,
}

impl<'t, V> WithNode<'t, V> {
    fn new(binding_value: &UiBindingValue<'t, '_>, value: V) -> Self {
        WithNode {
            node: binding_value.node(),
            value,
        }
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

    pub fn value(&self) -> &V {
        &self.value
    }

    pub fn into_value(self) -> V {
        self.value
    }

    pub fn map_value<U, F>(self, f: F) -> WithNode<'t, U>
    where
        F: FnOnce(V) -> U,
    {
        WithNode {
            node: self.node,
            value: f(self.value),
        }
    }

    fn rewrap<U>(&self, value: U) -> WithNode<'t, U> {
        WithNode {
            node: self.node,
            value,
        }
    }

    fn make_type_error(&self) -> ValueTypeError<'t> {
        ValueTypeError { node: self.node }
    }
}

impl<'t> WithNode<'t, Value> {
    pub fn to_enum(&self) -> Result<&str, ValueTypeError<'t>> {
        self.value.as_enum().ok_or_else(|| self.make_type_error())
    }

    pub fn to_enum_with_node(&self) -> Result<WithNode<'t, &str>, ValueTypeError<'t>> {
        Ok(self.rewrap(self.to_enum()?))
    }

    pub fn to_i32(&self) -> Result<i32, ValueTypeError<'t>> {
        self.value
            .as_number()
            .map(|d| d as i32)
            .ok_or_else(|| self.make_type_error())
    }

    pub fn to_i32_with_node(&self) -> Result<WithNode<'t, i32>, ValueTypeError<'t>> {
        Ok(self.rewrap(self.to_i32()?))
    }
}

#[derive(Clone, Debug, Error)]
#[error("unexpected type of value")]
pub(super) struct ValueTypeError<'t> {
    node: Node<'t>,
}

impl<'t> ValueTypeError<'t> {
    pub fn byte_range(&self) -> Range<usize> {
        self.node.byte_range()
    }
}

impl From<&ValueTypeError<'_>> for Diagnostic {
    fn from(error: &ValueTypeError) -> Self {
        Self::error(error.byte_range(), error.to_string())
    }
}

impl From<ValueTypeError<'_>> for Diagnostic {
    fn from(error: ValueTypeError) -> Self {
        Self::from(&error)
    }
}

/// Parses the given `binding_map` into a map of constant expressions.
///
/// `exclude_names` is a list of property names which have to be processed in a special
/// manner by the caller. The list should be small.
///
/// Unparsable properties are excluded from the resulting map so as many diagnostic messages
/// will be generated as possible.
///
/// Use `collect_properties_with_node()` if you need to inspect resulting values further.
pub(super) fn collect_properties(
    ctx: &BuildDocContext,
    cls: &Class,
    binding_map: &UiBindingMap,
    exclude_names: &[&str],
    diagnostics: &mut Diagnostics,
) -> HashMap<String, Value> {
    resolve_properties(ctx, cls, binding_map, exclude_names, diagnostics, |_, x| x)
}

pub(super) fn collect_properties_with_node<'t>(
    ctx: &BuildDocContext,
    cls: &Class,
    binding_map: &UiBindingMap<'t, '_>,
    exclude_names: &[&str],
    diagnostics: &mut Diagnostics,
) -> HashMap<String, WithNode<'t, Value>> {
    resolve_properties(
        ctx,
        cls,
        binding_map,
        exclude_names,
        diagnostics,
        WithNode::new,
    )
}

fn resolve_properties<'t, 's, B, F>(
    ctx: &BuildDocContext,
    cls: &Class,
    binding_map: &UiBindingMap<'t, 's>,
    exclude_names: &[&str],
    diagnostics: &mut Diagnostics,
    mut make_value: F,
) -> HashMap<String, B>
where
    F: FnMut(&UiBindingValue<'t, 's>, Value) -> B,
{
    binding_map
        .iter()
        .filter(|(name, _)| !exclude_names.contains(name))
        .filter_map(|(&name, value)| {
            if let Some(ty) = cls.get_property_type(name) {
                Value::from_binding_value(ctx, &ty, value, diagnostics)
            } else {
                diagnostics.push(Diagnostic::error(
                    value.binding_node().byte_range(),
                    format!(
                        "unknown property of class '{}': {}",
                        cls.qualified_name(),
                        name
                    ),
                ));
                None
            }
            .map(|x| (name.to_owned(), make_value(value, x)))
        })
        .collect()
}

pub(super) fn serialize_properties_to_xml<W, T>(
    writer: &mut XmlWriter<W>,
    tag_name: T,
    properties: &HashMap<String, Value>,
) -> XmlResult<()>
where
    W: io::Write,
    T: AsRef<[u8]>,
{
    let tag_name = tag_name.as_ref();
    for (k, v) in properties.iter().sorted_by_key(|&(k, _)| k) {
        let tag = BytesStart::borrowed_name(tag_name).with_attributes([("name", k.as_ref())]);
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        v.serialize_to_xml(writer)?;
        writer.write_event(Event::End(tag.to_end()))?;
    }
    Ok(())
}
