use super::expr::{self, SimpleValue, Value};
use super::property::{self, PropertiesMap};
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::color::Color;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::typemap::{Class, TypeSpace};
use itertools::Itertools as _;
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::io;

/// Constant map-like object which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Gadget {
    pub kind: GadgetKind,
    pub attributes: HashMap<String, SimpleValue>,
    pub properties: HashMap<String, Value>,
}

impl Gadget {
    pub(super) fn new(
        kind: GadgetKind,
        properties_map: PropertiesMap,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let (attributes, properties) = match kind {
            GadgetKind::Brush => make_brush_properties(properties_map, diagnostics),
            GadgetKind::Icon => make_icon_properties(properties_map, diagnostics),
            GadgetKind::Palette => make_palette_properties(properties_map, diagnostics),
            GadgetKind::SizePolicy => make_size_policy_properties(properties_map, diagnostics),
            _ => (
                HashMap::new(),
                property::make_serializable_properties(properties_map, diagnostics),
            ),
        };
        Gadget {
            kind,
            attributes,
            properties,
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        self.serialize_to_xml_as(writer, self.kind.as_tag_name())
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
        for (k, v) in self.attributes.iter().sorted_by_key(|&(k, _)| k) {
            match v {
                SimpleValue::Enum(s) if self.kind.no_enum_prefix() => {
                    tag.push_attribute((k.as_str(), expr::strip_enum_prefix(s)))
                }
                _ => tag.push_attribute((k.as_str(), v.to_string().as_str())),
            }
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        for (k, v) in self.properties.iter().sorted_by_key(|&(k, _)| k) {
            let t = k.to_ascii_lowercase(); // apparently tag name of .ui is lowercase
            match v {
                Value::Simple(SimpleValue::Enum(s)) if self.kind.no_enum_prefix() => {
                    xmlutil::write_tagged_str(writer, &t, expr::strip_enum_prefix(s))?;
                }
                _ => v.serialize_to_xml_as(writer, &t)?,
            }
        }

        writer.write_event(Event::End(tag.to_end()))
    }
}

impl From<Color> for Gadget {
    fn from(color: Color) -> Self {
        let attr = |n: &str, v: u8| (n.to_owned(), SimpleValue::Number(v.into()));
        let prop = |n: &str, v: u8| (n.to_owned(), Value::Simple(SimpleValue::Number(v.into())));
        let (attributes, properties) = match color {
            Color::Rgb8(c) => (
                // forcibly set alpha since QUiLoader can't handle color without alpha component
                HashMap::from([attr("alpha", 0xff)]),
                HashMap::from([
                    prop("red", c.red),
                    prop("green", c.green),
                    prop("blue", c.blue),
                ]),
            ),
            Color::Rgba8(c) => (
                // "alpha" is attribute for unknown reason
                HashMap::from([attr("alpha", c.alpha)]),
                HashMap::from([
                    prop("red", c.red),
                    prop("green", c.green),
                    prop("blue", c.blue),
                ]),
            ),
        };
        Gadget {
            kind: GadgetKind::Color,
            attributes,
            properties,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GadgetKind {
    Brush,
    Color,
    Font,
    Icon,
    Margins,
    Palette,
    Rect,
    Size,
    SizePolicy,
}

impl GadgetKind {
    pub(super) fn from_class(cls: &Class) -> Option<GadgetKind> {
        match cls.name() {
            "QBrush" => Some(GadgetKind::Brush),
            // incompatible property names: "QColor" => Some(GadgetKind::Color),
            "QFont" => Some(GadgetKind::Font),
            "QIcon" => Some(GadgetKind::Icon),
            "QMargins" => Some(GadgetKind::Margins),
            "QPalette" => Some(GadgetKind::Palette),
            "QRect" => Some(GadgetKind::Rect),
            "QSize" => Some(GadgetKind::Size),
            "QSizePolicy" => Some(GadgetKind::SizePolicy),
            _ => None,
        }
    }

    pub fn as_tag_name(&self) -> &'static str {
        match self {
            GadgetKind::Brush => "brush",
            GadgetKind::Color => "color",
            GadgetKind::Font => "font",
            GadgetKind::Icon => "iconset",
            GadgetKind::Margins => "margins", // not supported by uic
            GadgetKind::Palette => "palette",
            GadgetKind::Rect => "rect",
            GadgetKind::Size => "size",
            GadgetKind::SizePolicy => "sizepolicy",
        }
    }

    fn no_enum_prefix(&self) -> bool {
        match self {
            GadgetKind::Brush => true,
            GadgetKind::Color => false,
            GadgetKind::Font => true,
            GadgetKind::Icon => false,
            GadgetKind::Margins => false,
            GadgetKind::Palette => false,
            GadgetKind::Rect => false,
            GadgetKind::Size => false,
            GadgetKind::SizePolicy => true,
        }
    }
}

fn make_brush_properties(
    mut properties_map: PropertiesMap,
    diagnostics: &mut Diagnostics,
) -> (HashMap<String, SimpleValue>, HashMap<String, Value>) {
    let mut attributes = HashMap::new();
    if let Some(s) = properties_map
        .remove("style")
        .and_then(|v| diagnostics.consume_err(v.into_simple()))
    {
        attributes.insert("brushstyle".to_owned(), s);
    }

    let properties = property::make_serializable_properties(properties_map, diagnostics);
    (attributes, properties)
}

fn make_icon_properties(
    mut properties_map: PropertiesMap,
    diagnostics: &mut Diagnostics,
) -> (HashMap<String, SimpleValue>, HashMap<String, Value>) {
    let mut attributes = HashMap::new();
    if let Some(s) = properties_map
        .remove("name")
        .and_then(|v| diagnostics.consume_err(v.into_simple()))
    {
        attributes.insert("theme".to_owned(), s);
    }

    let properties = property::make_serializable_properties(properties_map, diagnostics);
    (attributes, properties)
}

fn make_palette_properties(
    properties_map: PropertiesMap,
    diagnostics: &mut Diagnostics,
) -> (HashMap<String, SimpleValue>, HashMap<String, Value>) {
    let mut color_groups = HashMap::from([
        ("active".to_owned(), PaletteColorGroup::default()),
        ("inactive".to_owned(), PaletteColorGroup::default()),
        ("disabled".to_owned(), PaletteColorGroup::default()),
    ]);
    let mut default_roles = Vec::new();
    for (k, v) in properties_map {
        match diagnostics.consume_err(v.into_serializable()) {
            Some(Value::PaletteColorGroup(g)) => {
                color_groups.insert(k, g);
            }
            Some(x) => {
                default_roles.push((uppercase_first_char(&k), x));
            }
            None => {}
        }
    }

    for g in color_groups.values_mut() {
        g.merge_default_roles(&default_roles);
    }

    let attributes = HashMap::new();
    let properties = color_groups
        .into_iter()
        .map(|(k, g)| (k, Value::PaletteColorGroup(g)))
        .collect();
    (attributes, properties)
}

fn make_size_policy_properties(
    mut properties_map: PropertiesMap,
    diagnostics: &mut Diagnostics,
) -> (HashMap<String, SimpleValue>, HashMap<String, Value>) {
    let mut attributes = HashMap::new();
    match (
        properties_map.remove("horizontalPolicy"),
        properties_map.remove("verticalPolicy"),
    ) {
        (Some(h), Some(v)) => {
            if let Some(s) = diagnostics.consume_err(h.into_simple()) {
                attributes.insert("hsizetype".to_owned(), s);
            }
            if let Some(s) = diagnostics.consume_err(v.into_simple()) {
                attributes.insert("vsizetype".to_owned(), s);
            }
        }
        (Some(x), None) | (None, Some(x)) => {
            diagnostics.push(Diagnostic::error(
                x.binding_node().byte_range(),
                "both horizontal and vertical policies must be specified",
            ));
        }
        (None, None) => {}
    }

    let mut properties = HashMap::new();
    for (k0, k1) in [
        ("horizontalStretch", "horstretch"),
        ("verticalStretch", "verstretch"),
    ] {
        if let Some(s) = properties_map
            .remove(k0)
            .and_then(|v| diagnostics.consume_err(v.into_serializable()))
        {
            properties.insert(k1.to_owned(), s);
        }
    }

    for x in properties_map.values() {
        diagnostics.push(Diagnostic::error(
            x.binding_node().byte_range(),
            "unknown property of size policy",
        ));
    }

    (attributes, properties)
}

/// Map of palette color role to brush.
#[derive(Clone, Debug, Default)]
pub struct PaletteColorGroup {
    pub roles: HashMap<String, Value>,
}

impl PaletteColorGroup {
    pub(super) fn new(properties_map: PropertiesMap, diagnostics: &mut Diagnostics) -> Self {
        let roles = properties_map
            .into_iter()
            .filter_map(|(k, v)| {
                diagnostics
                    .consume_err(v.into_serializable())
                    .map(|v| (uppercase_first_char(&k), v))
            })
            .collect();
        PaletteColorGroup { roles }
    }

    pub(super) fn merge_default_roles(&mut self, default_roles: &[(String, Value)]) {
        for (k, v) in default_roles {
            self.roles.entry(k.to_owned()).or_insert_with(|| v.clone());
        }
    }

    pub(super) fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        self.serialize_to_xml_as(writer, "colorgroup") // not supported by uic
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
        let group_tag = BytesStart::borrowed_name(tag_name.as_ref());
        writer.write_event(Event::Start(group_tag.to_borrowed()))?;

        for (k, v) in self.roles.iter().sorted_by_key(|&(k, _)| k) {
            let tag =
                BytesStart::borrowed_name(b"colorrole").with_attributes([("role", k.as_ref())]);
            writer.write_event(Event::Start(tag.to_borrowed()))?;
            v.serialize_to_xml(writer)?;
            writer.write_event(Event::End(tag.to_end()))?;
        }

        writer.write_event(Event::End(group_tag.to_end()))
    }
}

fn uppercase_first_char(name: &str) -> String {
    let mut chars = name.chars();
    let mut upper_name = String::new();
    if let Some(c) = chars.next() {
        upper_name.push(c.to_ascii_uppercase());
        upper_name.extend(chars);
    }
    upper_name
}
