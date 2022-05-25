use super::expr::ConstantValue;
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
    pub properties: HashMap<String, ConstantValue>,
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
        let properties = collect_constant_properties(cls, binding_map, source, diagnostics);
        match cls.name() {
            "QRect" => Some(Gadget {
                name: "rect".to_owned(),
                properties,
            }),
            "QSize" => Some(Gadget {
                name: "size".to_owned(),
                properties,
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
        let tag = BytesStart::borrowed_name(self.name.as_ref());
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        for (k, v) in self.properties.iter().sorted_by_key(|&(k, _)| k) {
            xmlutil::write_tagged_str(writer, k, v.to_string())?;
        }
        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Parses the given `binding_map` into a map of constant values.
///
/// Unparsable properties are excluded from the resulting map so as many diagnostic messages
/// will be generated as possible.
fn collect_constant_properties(
    cls: &Class,
    binding_map: &UiBindingMap,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> HashMap<String, ConstantValue> {
    binding_map
        .iter()
        .filter_map(|(&name, value)| {
            if let Some(ty) = cls.get_property_type(name) {
                ConstantValue::from_binding_value(cls, &ty, value, source, diagnostics)
            } else {
                diagnostics.push(Diagnostic::error(
                    value.node().byte_range(),
                    format!(
                        "unknown property of class '{}': {}",
                        cls.qualified_name(),
                        name
                    ),
                ));
                None
            }
            .map(|v| (name.to_owned(), v))
        })
        .collect()
}

/// Struct representing `QMargins`.
///
/// This is primarily designed for the `QLayout.contentsMargins` property, which needs to
/// be serialized to UI XML in a special manner.
#[derive(Clone, Debug, Default)]
pub struct Margins {
    pub left: i32,
    pub top: i32,
    pub right: i32,
    pub bottom: i32,
}

impl Margins {
    /// Generates margins from the given `binding_map`.
    ///
    /// The `cls` is supposed to be of `QMargins` type.
    pub(super) fn from_binding_map(
        cls: &Class,
        binding_map: &UiBindingMap,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        // TODO: better to create a properties map by caller, and extract it per gadget type?
        let properties_map =
            property::collect_properties_with_node(cls, binding_map, &[], source, diagnostics);
        let get_i32_property = |name, diagnostics: &mut Diagnostics| {
            properties_map
                .get(name)
                .and_then(|v| diagnostics.consume_err(v.to_i32()))
                .unwrap_or(0)
        };
        Margins {
            // should be kept sync with QMargins definition in metatype_tweak.rs
            left: get_i32_property("left", diagnostics),
            top: get_i32_property("top", diagnostics),
            right: get_i32_property("right", diagnostics),
            bottom: get_i32_property("bottom", diagnostics),
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let tag = BytesStart::borrowed_name(b"margins");
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        xmlutil::write_tagged_str(writer, "left", self.left.to_string())?;
        xmlutil::write_tagged_str(writer, "top", self.top.to_string())?;
        xmlutil::write_tagged_str(writer, "right", self.right.to_string())?;
        xmlutil::write_tagged_str(writer, "bottom", self.bottom.to_string())?;
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
    pub horizontal_policy: Option<String>,
    pub vertical_policy: Option<String>,
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
                .and_then(|v| diagnostics.consume_err(v.to_enum()))
                .map(|s| s.to_owned())
        };
        let get_i32_property = |name, diagnostics: &mut Diagnostics| {
            properties_map
                .get(name)
                .and_then(|v| diagnostics.consume_err(v.to_i32()))
        };
        SizePolicy {
            // should be kept sync with QSizePolicy definition in metatype_tweak.rs
            horizontal_policy: get_enum_property("horizontalPolicy", diagnostics),
            vertical_policy: get_enum_property("verticalPolicy", diagnostics),
            horizontal_stretch: get_i32_property("horizontalStretch", diagnostics),
            vertical_stretch: get_i32_property("verticalStretch", diagnostics),
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"sizepolicy");
        if let Some(s) = &self.horizontal_policy {
            tag.push_attribute(("hsizetype", s.strip_prefix("QSizePolicy::").unwrap_or(s)));
        }
        if let Some(s) = &self.vertical_policy {
            tag.push_attribute(("vsizetype", s.strip_prefix("QSizePolicy::").unwrap_or(s)));
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
