use super::context::ObjectContext;
use super::expr::{PropertyValue, SimpleValue, Value};
use super::objcode::PropertyCode;
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Node, UiBindingMap, UiBindingValue};
use crate::typemap::{Class, Property, TypeSpace};
use itertools::Itertools as _;
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::fmt::Debug;
use std::io;
use std::ops::Range;
use thiserror::Error;

/// Property value with its description.
#[derive(Clone, Debug)]
pub(super) struct PropertyDescValue<'a> {
    pub desc: Property<'a>,
    pub value: PropertyValue,
}

impl<'a> PropertyDescValue<'a> {
    pub fn new(desc: Property<'a>, value: PropertyValue) -> Self {
        PropertyDescValue { desc, value }
    }
}

/// Type of the property setter to be used by `uic`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PropertySetter {
    /// `QObject::setProperty(<name>, <value>)`
    Var,
    /// `set<Name>(<value>)`
    StdSet,
}

/// Parsed property value with its AST node.
#[derive(Clone, Copy, Debug)]
pub(super) struct WithNode<'t, V> {
    node: Node<'t>,
    pub data: V,
}

impl<'t, V> WithNode<'t, V> {
    fn new(binding_value: &UiBindingValue<'t, '_>, data: V) -> Self {
        WithNode {
            node: binding_value.node(),
            data,
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

    pub fn data(&self) -> &V {
        &self.data
    }

    pub fn into_data(self) -> V {
        self.data
    }

    pub fn map_data<U, F>(self, f: F) -> WithNode<'t, U>
    where
        F: FnOnce(V) -> U,
    {
        WithNode {
            node: self.node,
            data: f(self.data),
        }
    }

    fn rewrap<U>(&self, data: U) -> WithNode<'t, U> {
        WithNode {
            node: self.node,
            data,
        }
    }

    fn make_type_error(&self) -> ValueTypeError<'t> {
        ValueTypeError { node: self.node }
    }
}

impl<'t> WithNode<'t, PropertyDescValue<'_>> {
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
        match &self.data.value {
            PropertyValue::Serializable(v) => Some(v),
            _ => None,
        }
    }

    pub fn into_serializable(self) -> Result<Value, ValueTypeError<'t>> {
        match self.data.value {
            PropertyValue::Serializable(v) => Ok(v),
            _ => Err(self.make_type_error()),
        }
    }

    pub fn into_serializable_setter(self) -> Result<(Value, PropertySetter), ValueTypeError<'t>> {
        let s = if self.data.desc.is_std_set() {
            PropertySetter::StdSet
        } else {
            PropertySetter::Var
        };
        self.into_serializable().map(|v| (v, s))
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

/// Hash map of property values that may or may not be serialized to UI XML.
pub(super) type PropertiesMap<'a, 't> = HashMap<String, WithNode<'t, PropertyDescValue<'a>>>;

/// Parses the given `binding_map` into a map of constant expressions.
///
/// Unparsable properties are excluded from the resulting map so as many diagnostic messages
/// will be generated as possible.
///
/// Use `collect_properties_with_node()` if you need to inspect resulting values further.
pub(super) fn collect_properties(
    ctx: &ObjectContext,
    cls: &Class,
    binding_map: &UiBindingMap,
    diagnostics: &mut Diagnostics,
) -> HashMap<String, Value> {
    resolve_properties(ctx, cls, binding_map, diagnostics, |v, diagnostics| {
        diagnostics.consume_err(v.into_serializable())
    })
}

pub(super) fn collect_properties_with_node<'a, 't>(
    ctx: &ObjectContext<'a, 't, '_>,
    cls: &Class<'a>,
    binding_map: &UiBindingMap<'t, '_>,
    diagnostics: &mut Diagnostics,
) -> PropertiesMap<'a, 't> {
    resolve_properties(ctx, cls, binding_map, diagnostics, |v, _| Option::Some(v))
}

// TODO: migrate attached property handling to ObjectCodeMap and remove collect_properties*()
fn resolve_properties<'a, 't, 's, B, F>(
    ctx: &ObjectContext<'a, 't, '_>,
    cls: &Class<'a>,
    binding_map: &UiBindingMap<'t, 's>,
    diagnostics: &mut Diagnostics,
    mut make_value: F,
) -> HashMap<String, B>
where
    F: FnMut(WithNode<'t, PropertyDescValue<'a>>, &mut Diagnostics) -> Option<B>,
{
    binding_map
        .iter()
        .filter_map(|(&name, value)| {
            match cls.get_property(name) {
                Some(Ok(desc)) => {
                    let property_code = PropertyCode::build(ctx, desc, value, diagnostics)?;
                    let value_opt = PropertyValue::build(ctx, &property_code, diagnostics);
                    if property_code.is_evaluated_constant() {
                        value_opt.map(|v| PropertyDescValue::new(property_code.desc().clone(), v))
                    } else {
                        diagnostics.push(Diagnostic::error(
                            property_code.node().byte_range(),
                            "dynamic binding to attached property is not supported",
                        ));
                        None
                    }
                }
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
            .and_then(|x| {
                make_value(WithNode::new(value, x), diagnostics).map(|v| (name.to_owned(), v))
            })
        })
        .collect()
}

// TODO: remove after migrating to ObjectCodeMap
pub(super) fn make_properties_from_code_map<'a, 't>(
    ctx: &ObjectContext<'a, '_, '_>,
    properties_code_map: &HashMap<&str, PropertyCode<'a, 't, '_>>,
    diagnostics: &mut Diagnostics,
) -> PropertiesMap<'a, 't> {
    properties_code_map
        .iter()
        .filter_map(|(&name, property_code)| {
            let node = property_code.node();
            let value = PropertyValue::build(ctx, property_code, diagnostics)?;
            let data = PropertyDescValue::new(property_code.desc().clone(), value);
            Some((name.to_owned(), WithNode { node, data }))
        })
        .collect()
}

/// Creates a map of serializable properties from the given code map.
///
/// `excludes` is a list of property names which should have been processed in a special
/// manner by the caller. The list should be small in practice.
pub(super) fn make_serializable_map(
    ctx: &ObjectContext,
    properties_code_map: &HashMap<&str, PropertyCode>,
    excludes: &[&str],
    diagnostics: &mut Diagnostics,
) -> HashMap<String, (Value, PropertySetter)> {
    properties_code_map
        .iter()
        .filter(|(name, _)| !excludes.contains(name))
        .filter_map(|(&name, property_code)| {
            // evaluate once to sort out constant/dynamic nature
            let v = Value::build(ctx, property_code, diagnostics)?;
            if property_code.desc().is_writable() {
                let s = if property_code.desc().is_std_set() {
                    PropertySetter::StdSet
                } else {
                    PropertySetter::Var
                };
                Some((name.to_owned(), (v, s)))
            } else {
                diagnostics.push(Diagnostic::error(
                    property_code.binding_node().byte_range(),
                    "not a writable property",
                ));
                None
            }
        })
        .collect()
}

/// Creates a map of gadget properties from the given code map.
///
/// Unlike `make_serializable_map()`, this does not check for the property's setter
/// availability since the gadget properties will be passed as constructor arguments.
pub(super) fn make_value_map(
    ctx: &ObjectContext,
    properties_code_map: &HashMap<&str, PropertyCode>,
    excludes: &[&str],
    diagnostics: &mut Diagnostics,
) -> HashMap<String, Value> {
    properties_code_map
        .iter()
        .filter(|(name, _)| !excludes.contains(name))
        .filter_map(|(&name, property_code)| {
            let v = Value::build(ctx, property_code, diagnostics)?;
            Some((name.to_owned(), v))
        })
        .collect()
}

pub(super) fn get_simple_value<'a, 't, 's, 'm>(
    ctx: &ObjectContext,
    properties_code_map: &'m HashMap<&str, PropertyCode<'a, 't, 's>>,
    name: impl AsRef<str>,
    diagnostics: &mut Diagnostics,
) -> Option<(&'m PropertyCode<'a, 't, 's>, SimpleValue)> {
    let p = properties_code_map.get(name.as_ref())?;
    match Value::build(ctx, p, diagnostics) {
        Some(Value::Simple(s)) => Some((p, s)),
        Some(_) => {
            diagnostics.push(Diagnostic::error(
                p.node().byte_range(),
                "unexpected value type",
            ));
            None
        }
        None => None,
    }
}

pub(super) fn get_enum<'a, 't, 's, 'm>(
    ctx: &ObjectContext,
    properties_code_map: &'m HashMap<&str, PropertyCode<'a, 't, 's>>,
    name: impl AsRef<str>,
    diagnostics: &mut Diagnostics,
) -> Option<(&'m PropertyCode<'a, 't, 's>, String)> {
    let (p, v) = get_simple_value(ctx, properties_code_map, name, diagnostics)?;
    if let Some(s) = v.into_enum() {
        Some((p, s))
    } else {
        diagnostics.push(Diagnostic::error(
            p.node().byte_range(),
            "unexpected value type",
        ));
        None
    }
}

pub(super) fn get_i32<'a, 't, 's, 'm>(
    ctx: &ObjectContext,
    properties_code_map: &'m HashMap<&str, PropertyCode<'a, 't, 's>>,
    name: impl AsRef<str>,
    diagnostics: &mut Diagnostics,
) -> Option<(&'m PropertyCode<'a, 't, 's>, i32)> {
    let (p, v) = get_simple_value(ctx, properties_code_map, name, diagnostics)?;
    if let Some(d) = v.as_number() {
        Some((p, d as i32))
    } else {
        diagnostics.push(Diagnostic::error(
            p.node().byte_range(),
            "unexpected value type",
        ));
        None
    }
}

/// Makes sure all properties are writable, removes if not.
///
/// This should be called at the very last but before `make_serializable_properties()`
/// where all pseudo properties would have been transformed.
pub(super) fn reject_unwritable_properties(
    properties_map: &mut PropertiesMap,
    diagnostics: &mut Diagnostics,
) {
    properties_map.retain(|_, v| {
        let writable = v.data().desc.is_writable();
        if !writable {
            diagnostics.push(Diagnostic::error(
                v.binding_node().byte_range(),
                "not a writable property",
            ));
        }
        writable
    });
}

pub(super) fn make_serializable_properties(
    properties_map: PropertiesMap,
    diagnostics: &mut Diagnostics,
) -> HashMap<String, (Value, PropertySetter)> {
    properties_map
        .into_iter()
        .filter_map(|(k, v)| {
            diagnostics
                .consume_err(v.into_serializable_setter())
                .map(|x| (k, x))
        })
        .collect()
}

pub(super) fn serialize_properties_to_xml<W, T>(
    writer: &mut XmlWriter<W>,
    tag_name: T,
    properties: &HashMap<String, (Value, PropertySetter)>,
) -> XmlResult<()>
where
    W: io::Write,
    T: AsRef<[u8]>,
{
    let tag_name = tag_name.as_ref();
    for (k, (v, s)) in properties.iter().sorted_by_key(|&(k, _)| k) {
        let mut tag = BytesStart::borrowed_name(tag_name).with_attributes([("name", k.as_ref())]);
        if *s != PropertySetter::StdSet {
            tag.push_attribute(("stdset", "0"));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        v.serialize_to_xml(writer)?;
        writer.write_event(Event::End(tag.to_end()))?;
    }
    Ok(())
}
