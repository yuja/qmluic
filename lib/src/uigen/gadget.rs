use super::expr::ConstantValue;
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::qmlast::{Node, ParseError, ParseErrorKind, UiBindingMap, UiBindingValue};
use crate::typemap::Class;
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

fn unexpected_node(node: Node) -> ParseError {
    ParseError::new(node, ParseErrorKind::UnexpectedNodeKind)
}
