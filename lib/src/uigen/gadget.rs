use super::expr::{self, ConstantValue};
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Node, UiBindingMap, UiBindingValue};
use crate::typemap::{Class, TypeSpace};
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::io;

/// Constant map-like object which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct ConstantGadget {
    pub name: String,
    pub properties: HashMap<String, ConstantValue>,
}

impl ConstantGadget {
    fn new(name: &str, properties: HashMap<String, ConstantValue>) -> Self {
        ConstantGadget {
            name: name.to_owned(),
            properties,
        }
    }

    /// Generates gadget of `cls` type from the given `binding_map`.
    pub fn from_binding_map(
        cls: &Class,
        node: Node,
        binding_map: &UiBindingMap,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match cls.name() {
            "QRect" => Some(Self::new(
                "rect",
                collect_constant_properties(cls, binding_map, source, diagnostics),
            )),
            "QSize" => Some(Self::new(
                "size",
                collect_constant_properties(cls, binding_map, source, diagnostics),
            )),
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
        let mut pairs: Vec<_> = self.properties.iter().collect();
        pairs.sort_by_key(|&(k, _)| k);
        for (k, v) in pairs {
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
                match value {
                    UiBindingValue::Node(n) => {
                        ConstantValue::from_expression(cls, &ty, *n, source, diagnostics)
                    }
                    UiBindingValue::Map(n, _) => {
                        diagnostics.push(Diagnostic::error(
                            n.byte_range(),
                            "binding map cannot be gadget property",
                        ));
                        None
                    }
                }
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

/// Constant size policy which can be serialized to UI XML.
///
/// `QSizePolicy` is special in that
/// - it is registered as gadget type but have no useful property,
/// - UI XML attribute names are diverged from the method/property names.
#[derive(Clone, Debug, Default)]
pub struct ConstantSizePolicy {
    pub horizontal_policy: Option<String>,
    pub vertical_policy: Option<String>,
    pub horizontal_stretch: Option<f64>,
    pub vertical_stretch: Option<f64>,
}

impl ConstantSizePolicy {
    /// Generates size policy from the given `binding_map`.
    ///
    /// The `cls` is supposed to be of `QSizePolicy` type.
    pub fn from_binding_map(
        cls: &Class,
        _node: Node,
        binding_map: &UiBindingMap,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let mut policy = ConstantSizePolicy::default();
        let mut ok = true;
        for (&name, value) in binding_map {
            match name {
                "horizontalPolicy" => {
                    policy.horizontal_policy = extract_size_policy(cls, value, source, diagnostics);
                }
                "verticalPolicy" => {
                    policy.vertical_policy = extract_size_policy(cls, value, source, diagnostics);
                }
                "horizontalStretch" => {
                    policy.horizontal_stretch =
                        expr::evaluate_number(cls, value, source, diagnostics);
                }
                "verticalStretch" => {
                    policy.vertical_stretch =
                        expr::evaluate_number(cls, value, source, diagnostics);
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        value.node().byte_range(),
                        format!(
                            "unknown property of class '{}': {}",
                            cls.qualified_name(),
                            name
                        ),
                    ));
                    ok = false;
                }
            }
        }

        if ok {
            Some(policy)
        } else {
            None
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"sizepolicy");
        if let Some(s) = &self.horizontal_policy {
            tag.push_attribute(("hsizetype", s.as_ref()));
        }
        if let Some(s) = &self.vertical_policy {
            tag.push_attribute(("vsizetype", s.as_ref()));
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

fn extract_size_policy(
    cls: &Class,
    value: &UiBindingValue,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<String> {
    if let Some(ty) = cls.resolve_type_scoped("QSizePolicy::Policy") {
        expr::format_enum_expression(cls, &ty, value, source, diagnostics)
            .and_then(|s| s.strip_prefix("QSizePolicy::").map(|t| t.to_owned()))
    } else {
        diagnostics.push(Diagnostic::error(
            value.node().byte_range(),
            "QSizePolicy::Policy cannot be resolved from type map",
        ));
        None
    }
}
