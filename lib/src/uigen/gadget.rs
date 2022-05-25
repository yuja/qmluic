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
        let properties = property::collect_gadget_properties(cls, binding_map, source, diagnostics);
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
        let tag = BytesStart::borrowed_name(self.name.as_ref());
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        for (k, v) in self.properties.iter().sorted_by_key(|&(k, _)| k) {
            let s = if self.no_enum_prefix {
                v.as_enum()
                    .map(|s| strip_enum_prefix(s).to_owned())
                    .unwrap_or_else(|| v.to_string())
            } else {
                v.to_string()
            };
            // apparently tag name of .ui is lowercase
            xmlutil::write_tagged_str(writer, k.to_ascii_lowercase(), s)?;
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
            tag.push_attribute(("hsizetype", strip_enum_prefix(s)));
        }
        if let Some(s) = &self.vertical_policy {
            tag.push_attribute(("vsizetype", strip_enum_prefix(s)));
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
