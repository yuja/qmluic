use super::expr::ConstantValue;
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::qmlast::{Node, ParseError, ParseErrorKind, UiBindingMap, UiBindingValue};
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
    pub fn from_binding_map<'tree>(
        cls: &Class,
        node: Node<'tree>,
        binding_map: &UiBindingMap<'tree, '_>,
        source: &str,
        diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
    ) -> Option<Self> {
        match cls.name() {
            "QRect" => collect_constant_properties(cls, binding_map, source, diagnostics)
                .map(|ps| Self::new("rect", ps)),
            "QSize" => collect_constant_properties(cls, binding_map, source, diagnostics)
                .map(|ps| Self::new("size", ps)),
            _ => {
                diagnostics.push(unexpected_node(node));
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

fn collect_constant_properties<'tree>(
    cls: &Class,
    binding_map: &UiBindingMap<'tree, '_>,
    source: &str,
    diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
) -> Option<HashMap<String, ConstantValue>> {
    binding_map
        .iter()
        .map(|(&name, value)| {
            if let Some(ty) = cls.get_property_type(name) {
                match value {
                    UiBindingValue::Node(n) => {
                        ConstantValue::from_expression(&ty, *n, source, diagnostics)
                    }
                    UiBindingValue::Map(n, _) => {
                        diagnostics.push(unexpected_node(*n));
                        None
                    }
                }
            } else {
                diagnostics.push(unexpected_node(value.node())); // TODO: unknown property/type
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
    /// The `cls` must be of `QSizePolicy` type.
    pub fn from_binding_map<'tree>(
        cls: &Class,
        node: Node<'tree>,
        binding_map: &UiBindingMap<'tree, '_>,
        source: &str,
        diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
    ) -> Option<Self> {
        if cls.name() != "QSizePolicy" {
            diagnostics.push(unexpected_node(node)); // TODO
            return None;
        }

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
                    policy.horizontal_stretch = extract_int(cls, value, source, diagnostics);
                }
                "verticalStretch" => {
                    policy.vertical_stretch = extract_int(cls, value, source, diagnostics);
                }
                _ => {
                    diagnostics.push(unexpected_node(value.node())); // TODO
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

fn extract_size_policy<'tree>(
    cls: &Class,
    value: &UiBindingValue<'tree, '_>,
    source: &str,
    diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
) -> Option<String> {
    extract_value_of_type_name(cls, "QSizePolicy::Policy", value, source, diagnostics).and_then(
        |v| {
            match v {
                ConstantValue::Enum(s) => {
                    if let Some(t) = s.strip_prefix("QSizePolicy::") {
                        Some(t.to_owned())
                    } else {
                        diagnostics.push(unexpected_node(value.node())); // TODO
                        None
                    }
                }
                _ => {
                    diagnostics.push(unexpected_node(value.node())); // TODO
                    None
                }
            }
        },
    )
}

fn extract_int<'tree>(
    cls: &Class,
    value: &UiBindingValue<'tree, '_>,
    source: &str,
    diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
) -> Option<f64> {
    extract_value_of_type_name(cls, "int", value, source, diagnostics).and_then(|v| {
        match v {
            ConstantValue::Number(d) => Some(d),
            _ => {
                diagnostics.push(unexpected_node(value.node())); // TODO
                None
            }
        }
    })
}

fn extract_value_of_type_name<'tree>(
    cls: &Class,
    type_name: &str,
    value: &UiBindingValue<'tree, '_>,
    source: &str,
    diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
) -> Option<ConstantValue> {
    if let Some(ty) = cls.resolve_type_scoped(type_name) {
        match value {
            UiBindingValue::Node(n) => ConstantValue::from_expression(&ty, *n, source, diagnostics),
            UiBindingValue::Map(n, _) => {
                diagnostics.push(unexpected_node(*n));
                None
            }
        }
    } else {
        diagnostics.push(unexpected_node(value.node())); // TODO
        None
    }
}

fn unexpected_node(node: Node) -> ParseError {
    ParseError::new(node, ParseErrorKind::UnexpectedNodeKind)
}
