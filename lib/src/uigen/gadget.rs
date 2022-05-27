use super::expr::{ConstantExpression, ConstantValue};
use super::property;
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Node, UiBindingMap};
use crate::typemap::{Class, TypeSpace};
use itertools::Itertools as _;
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::io;

/// Constant map-like object which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Gadget {
    pub name: String,
    pub properties: HashMap<String, ConstantExpression>,
    pub no_enum_prefix: bool,
}

impl Gadget {
    /// Generates gadget of `cls` type from the given `binding_map`.
    pub fn from_binding_map(
        cls: &Class,
        node: Node,
        binding_map: &UiBindingMap,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let properties = property::collect_properties(cls, binding_map, &[], source, diagnostics);
        match cls.name() {
            "QFont" => Some(Gadget {
                name: "font".to_owned(),
                properties,
                no_enum_prefix: true,
            }),
            "QMargins" => Some(Gadget {
                name: "margins".to_owned(), // not supported by uic
                properties,
                no_enum_prefix: false,
            }),
            "QRect" => Some(Gadget {
                name: "rect".to_owned(),
                properties,
                no_enum_prefix: false,
            }),
            "QSize" => Some(Gadget {
                name: "size".to_owned(),
                properties,
                no_enum_prefix: false,
            }),
            _ => {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!("unsupported gadget type: {}", cls.qualified_name()),
                ));
                None
            }
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        self.serialize_to_xml_as(writer, &self.name)
    }

    pub(super) fn serialize_to_xml_as<W, T>(
        &self,
        writer: &mut XmlWriter<W>,
        tag_name: T,
    ) -> XmlResult<()>
    where
        W: io::Write,
        T: AsRef<[u8]>,
    {
        let tag = BytesStart::borrowed_name(tag_name.as_ref());
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        for (k, v) in self.properties.iter().sorted_by_key(|&(k, _)| k) {
            let t = k.to_ascii_lowercase(); // apparently tag name of .ui is lowercase
            match v {
                ConstantExpression::Value(ConstantValue::Enum(s)) if self.no_enum_prefix => {
                    xmlutil::write_tagged_str(writer, &t, strip_enum_prefix(s))?;
                }
                _ => v.serialize_to_xml_as(writer, &t)?,
            }
        }
        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Constant size policy which can be serialized to UI XML.
///
/// `QSizePolicy` is special in that
/// - it is registered as gadget type but have no useful property,
/// - UI XML attribute names are diverged from the method/property names.
#[derive(Clone, Debug, Default)]
pub struct SizePolicy {
    pub policy: Option<(String, String)>,
    pub horizontal_stretch: Option<i32>,
    pub vertical_stretch: Option<i32>,
}

impl SizePolicy {
    /// Generates size policy from the given `binding_map`.
    ///
    /// The `cls` is supposed to be of `QSizePolicy` type.
    pub fn from_binding_map(
        cls: &Class,
        binding_map: &UiBindingMap,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let properties_map =
            property::collect_properties_with_node(cls, binding_map, &[], source, diagnostics);
        let get_enum_property = |name, diagnostics: &mut Diagnostics| {
            properties_map
                .get(name)
                .and_then(|v| diagnostics.consume_err(v.to_enum_with_node()))
        };
        let get_i32_property = |name, diagnostics: &mut Diagnostics| {
            properties_map
                .get(name)
                .and_then(|v| diagnostics.consume_err(v.to_i32()))
        };
        let policy = match (
            get_enum_property("horizontalPolicy", diagnostics),
            get_enum_property("verticalPolicy", diagnostics),
        ) {
            (Some(h), Some(v)) => Some((h.into_value().to_owned(), v.into_value().to_owned())),
            (Some(x), _) | (_, Some(x)) => {
                diagnostics.push(Diagnostic::error(
                    x.binding_node().byte_range(),
                    "both horizontal and vertical policies must be specified",
                ));
                None
            }
            (None, None) => None,
        };
        SizePolicy {
            // should be kept sync with QSizePolicy definition in metatype_tweak.rs
            policy,
            horizontal_stretch: get_i32_property("horizontalStretch", diagnostics),
            vertical_stretch: get_i32_property("verticalStretch", diagnostics),
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        self.serialize_to_xml_as(writer, "sizepolicy")
    }

    pub(super) fn serialize_to_xml_as<W, T>(
        &self,
        writer: &mut XmlWriter<W>,
        tag_name: T,
    ) -> XmlResult<()>
    where
        W: io::Write,
        T: AsRef<[u8]>,
    {
        let mut tag = BytesStart::borrowed_name(tag_name.as_ref());
        if let Some((h, v)) = &self.policy {
            tag.push_attribute(("hsizetype", strip_enum_prefix(h)));
            tag.push_attribute(("vsizetype", strip_enum_prefix(v)));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        if let Some(d) = self.horizontal_stretch {
            xmlutil::write_tagged_str(writer, "horstretch", d.to_string())?;
        }
        if let Some(d) = self.vertical_stretch {
            xmlutil::write_tagged_str(writer, "verstretch", d.to_string())?;
        }

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

fn strip_enum_prefix(s: &str) -> &str {
    s.split_once("::").map(|(_, t)| t).unwrap_or(s)
}
