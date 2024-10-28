use super::context::ObjectContext;
use super::expr::{SerializableValue, SimpleValue};
use super::objcode::PropertyCode;
use super::XmlWriter;
use crate::diagnostic::{Diagnostic, Diagnostics};
use itertools::Itertools as _;
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::fmt::Debug;
use std::io;

/// Type of the property setter to be used by `uic`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PropertySetter {
    /// `QObject::setProperty(<name>, <value>)`
    Var,
    /// `set<Name>(<value>)`
    StdSet,
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
) -> HashMap<String, (SerializableValue, PropertySetter)> {
    properties_code_map
        .iter()
        .filter(|(name, _)| !excludes.contains(name))
        .filter_map(|(&name, property_code)| {
            // evaluate once to sort out constant/dynamic nature
            let v = SerializableValue::build(ctx, property_code, diagnostics)?;
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
) -> HashMap<String, SerializableValue> {
    properties_code_map
        .iter()
        .filter(|(name, _)| !excludes.contains(name))
        .filter_map(|(&name, property_code)| {
            let v = SerializableValue::build(ctx, property_code, diagnostics)?;
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
    match SerializableValue::build(ctx, p, diagnostics) {
        Some(SerializableValue::Simple(s)) => Some((p, s)),
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

pub(super) fn get_bool<'a, 't, 's, 'm>(
    ctx: &ObjectContext,
    properties_code_map: &'m HashMap<&str, PropertyCode<'a, 't, 's>>,
    name: impl AsRef<str>,
    diagnostics: &mut Diagnostics,
) -> Option<(&'m PropertyCode<'a, 't, 's>, bool)> {
    let (p, v) = get_simple_value(ctx, properties_code_map, name, diagnostics)?;
    if let Some(b) = v.as_bool() {
        Some((p, b))
    } else {
        diagnostics.push(Diagnostic::error(
            p.node().byte_range(),
            "unexpected value type",
        ));
        None
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

pub(super) fn serialize_properties_to_xml<W, T>(
    writer: &mut XmlWriter<W>,
    tag_name: T,
    properties: &HashMap<String, (SerializableValue, PropertySetter)>,
) -> io::Result<()>
where
    W: io::Write,
    T: AsRef<str>,
{
    let tag_name = tag_name.as_ref();
    for (k, (v, s)) in properties.iter().sorted_by_key(|&(k, _)| k) {
        let mut tag = BytesStart::new(tag_name).with_attributes([("name", k.as_ref())]);
        if *s != PropertySetter::StdSet {
            tag.push_attribute(("stdset", "0"));
        }
        writer.write_event(Event::Start(tag.borrow()))?;
        v.serialize_to_xml(writer)?;
        writer.write_event(Event::End(tag.to_end()))?;
    }
    Ok(())
}
