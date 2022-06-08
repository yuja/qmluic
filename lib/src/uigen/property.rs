use super::expr::{PropertyValue, Value};
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

impl<'t> WithNode<'t, PropertyValue> {
    pub fn to_enum(&self) -> Result<&str, ValueTypeError<'t>> {
        self.as_serializable()
            .and_then(|v| v.as_enum())
            .ok_or_else(|| self.make_type_error())
    }

    pub fn to_enum_with_node(&self) -> Result<WithNode<'t, &str>, ValueTypeError<'t>> {
        Ok(self.rewrap(self.to_enum()?))
    }

    pub fn to_i32(&self) -> Result<i32, ValueTypeError<'t>> {
        self.as_serializable()
            .and_then(|v| v.as_number())
            .map(|d| d as i32)
            .ok_or_else(|| self.make_type_error())
    }

    pub fn to_i32_with_node(&self) -> Result<WithNode<'t, i32>, ValueTypeError<'t>> {
        Ok(self.rewrap(self.to_i32()?))
    }

    pub fn as_serializable(&self) -> Option<&Value> {
        match &self.value {
            PropertyValue::Serializable(v) => Some(v),
        }
    }

    pub fn into_serializable(self) -> Result<Value, ValueTypeError<'t>> {
        match self.value {
            PropertyValue::Serializable(v) => Ok(v),
        }
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
/// Unparsable properties are excluded from the resulting map so as many diagnostic messages
/// will be generated as possible.
///
/// Use `collect_properties_with_node()` if you need to inspect resulting values further.
pub(super) fn collect_properties(
    ctx: &BuildDocContext,
    cls: &Class,
    binding_map: &UiBindingMap,
    diagnostics: &mut Diagnostics,
) -> HashMap<String, Value> {
    resolve_properties(ctx, cls, binding_map, diagnostics, |v, diagnostics| {
        diagnostics.consume_err(v.into_serializable())
    })
}

pub(super) fn collect_properties_with_node<'t>(
    ctx: &BuildDocContext,
    cls: &Class,
    binding_map: &UiBindingMap<'t, '_>,
    diagnostics: &mut Diagnostics,
) -> HashMap<String, WithNode<'t, PropertyValue>> {
    resolve_properties(ctx, cls, binding_map, diagnostics, |v, _| Option::Some(v))
}

fn resolve_properties<'t, 's, B, F>(
    ctx: &BuildDocContext,
    cls: &Class,
    binding_map: &UiBindingMap<'t, 's>,
    diagnostics: &mut Diagnostics,
    mut make_value: F,
) -> HashMap<String, B>
where
    F: FnMut(WithNode<'t, PropertyValue>, &mut Diagnostics) -> Option<B>,
{
    binding_map
        .iter()
        .filter_map(|(&name, value)| {
            if let Some(ty) = cls.get_property_type(name) {
                Value::from_binding_value(ctx, &ty, value, diagnostics)
                    .map(PropertyValue::Serializable)
            } else {
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
            .and_then(|x| {
                make_value(WithNode::new(value, x), diagnostics).map(|v| (name.to_owned(), v))
            })
        })
        .collect()
}

pub(super) fn make_serializable_properties(
    properties_map: HashMap<String, WithNode<'_, PropertyValue>>,
    diagnostics: &mut Diagnostics,
) -> HashMap<String, Value> {
    properties_map
        .into_iter()
        .filter_map(|(k, v)| {
            diagnostics
                .consume_err(v.into_serializable())
                .map(|v| (k, v))
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
