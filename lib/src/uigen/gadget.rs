use super::expr::{self, ConstantValue};
use super::object;
use super::xmlutil;
use super::{BuildContext, XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Node, UiBindingMap, UiBindingValue};
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
    fn new(name: &str, properties: HashMap<String, ConstantValue>) -> Self {
        Gadget {
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
    pub(super) fn from_binding_value<'a, P>(
        ctx: &BuildContext,
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        binding_value: &UiBindingValue,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self>
    where
        P: TypeSpace<'a>,
    {
        match binding_value {
            UiBindingValue::Node(n) => {
                diagnostics.push(Diagnostic::error(
                    n.byte_range(),
                    "expression cannot be parsed as QMargins",
                ));
                None
            }
            UiBindingValue::Map(_, m) => {
                Some(Self::from_binding_map(ctx, parent_space, m, diagnostics))
            }
        }
    }

    pub(super) fn from_binding_map<'a, P>(
        ctx: &BuildContext,
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        binding_map: &UiBindingMap,
        diagnostics: &mut Diagnostics,
    ) -> Self
    where
        P: TypeSpace<'a>,
    {
        // TODO: maybe add a proc macro for this kind of gadget types?
        let mut margins = Margins::default();
        for (&name, value) in binding_map {
            match name {
                "left" => {
                    margins.left = expr::evaluate_i32(parent_space, value, ctx.source, diagnostics)
                        .unwrap_or(0);
                }
                "top" => {
                    margins.top = expr::evaluate_i32(parent_space, value, ctx.source, diagnostics)
                        .unwrap_or(0);
                }
                "right" => {
                    margins.right =
                        expr::evaluate_i32(parent_space, value, ctx.source, diagnostics)
                            .unwrap_or(0);
                }
                "bottom" => {
                    margins.bottom =
                        expr::evaluate_i32(parent_space, value, ctx.source, diagnostics)
                            .unwrap_or(0);
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        value.binding_node().byte_range(),
                        "unknown property of class 'QMargins'",
                    ));
                }
            }
        }
        margins
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
    pub horizontal_stretch: Option<f64>,
    pub vertical_stretch: Option<f64>,
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
            object::collect_properties(cls, binding_map, &[], source, diagnostics);
        let expect_size_policy_property = |name| {
            properties_map.get(name).map(|v| {
                let s = v
                    .as_enum()
                    .expect("internal QSizePolicy property should be typed as enum");
                s.strip_prefix("QSizePolicy::").unwrap_or(s).to_owned()
            })
        };
        let expect_f64_property = |name| {
            properties_map.get(name).map(|v| {
                v.as_number()
                    .expect("internal QSizePolicy property should be typed as number")
            })
        };
        SizePolicy {
            // should be kept sync with QSizePolicy definition in metatype_tweak.rs
            horizontal_policy: expect_size_policy_property("horizontalPolicy"),
            vertical_policy: expect_size_policy_property("verticalPolicy"),
            horizontal_stretch: expect_f64_property("horizontalStretch"),
            vertical_stretch: expect_f64_property("verticalStretch"),
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
