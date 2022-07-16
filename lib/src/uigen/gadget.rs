use super::context::{KnownClasses, ObjectContext};
use super::expr::{self, SimpleValue, StringKind, Value};
use super::objcode::PropertyCode;
use super::property;
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::color::Color;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qtname;
use crate::typemap::Class;
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
        ctx: &ObjectContext,
        kind: GadgetKind,
        map: &HashMap<&str, PropertyCode>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let (attributes, properties) = match kind {
            GadgetKind::Brush => make_brush_properties(ctx, map, diagnostics),
            GadgetKind::Icon => make_icon_properties(ctx, map, diagnostics),
            GadgetKind::Palette => make_palette_properties(ctx, map, diagnostics),
            GadgetKind::SizePolicy => make_size_policy_properties(ctx, map, diagnostics),
            _ => (
                HashMap::new(),
                property::make_value_map(ctx, map, &[], diagnostics),
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
    pub(super) fn from_class(cls: &Class, classes: &KnownClasses) -> Option<GadgetKind> {
        match cls {
            _ if cls == &classes.brush => Some(GadgetKind::Brush),
            // incompatible property names: color => Some(GadgetKind::Color),
            _ if cls == &classes.font => Some(GadgetKind::Font),
            _ if cls == &classes.icon => Some(GadgetKind::Icon),
            _ if cls == &classes.margins => Some(GadgetKind::Margins),
            _ if cls == &classes.palette => Some(GadgetKind::Palette),
            _ if cls == &classes.rect => Some(GadgetKind::Rect),
            _ if cls == &classes.size => Some(GadgetKind::Size),
            _ if cls == &classes.size_policy => Some(GadgetKind::SizePolicy),
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
    ctx: &ObjectContext,
    map: &HashMap<&str, PropertyCode>,
    diagnostics: &mut Diagnostics,
) -> (HashMap<String, SimpleValue>, HashMap<String, Value>) {
    let mut attributes = HashMap::new();
    if let Some((_, s)) = property::get_simple_value(ctx, map, "style", diagnostics) {
        attributes.insert("brushstyle".to_owned(), s);
    }

    let properties = property::make_value_map(ctx, map, &["style"], diagnostics);
    (attributes, properties)
}

fn make_icon_properties(
    ctx: &ObjectContext,
    map: &HashMap<&str, PropertyCode>,
    diagnostics: &mut Diagnostics,
) -> (HashMap<String, SimpleValue>, HashMap<String, Value>) {
    let mut attributes = HashMap::new();
    if let Some((_, s)) = property::get_simple_value(ctx, map, "name", diagnostics) {
        attributes.insert("theme".to_owned(), s);
    }

    let properties = property::make_value_map(ctx, map, &["name"], diagnostics);
    (attributes, properties)
}

fn make_palette_properties(
    ctx: &ObjectContext,
    map: &HashMap<&str, PropertyCode>,
    diagnostics: &mut Diagnostics,
) -> (HashMap<String, SimpleValue>, HashMap<String, Value>) {
    let mut color_groups = HashMap::from([
        ("active", PaletteColorGroup::default()),
        ("inactive", PaletteColorGroup::default()),
        ("disabled", PaletteColorGroup::default()),
    ]);
    let mut default_roles = Vec::new();
    for (k, p) in map {
        match Value::build(ctx, p, diagnostics) {
            Some(Value::PaletteColorGroup(g)) => {
                color_groups.insert(k, g);
            }
            Some(x) => {
                default_roles.push((qtname::to_ascii_capitalized(k), x));
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
        .map(|(k, g)| (k.to_owned(), Value::PaletteColorGroup(g)))
        .collect();
    (attributes, properties)
}

fn make_size_policy_properties(
    ctx: &ObjectContext,
    map: &HashMap<&str, PropertyCode>,
    diagnostics: &mut Diagnostics,
) -> (HashMap<String, SimpleValue>, HashMap<String, Value>) {
    let mut attributes = HashMap::new();
    match (
        property::get_simple_value(ctx, map, "horizontalPolicy", diagnostics),
        property::get_simple_value(ctx, map, "verticalPolicy", diagnostics),
    ) {
        (Some((_, h)), Some((_, v))) => {
            attributes.insert("hsizetype".to_owned(), h);
            attributes.insert("vsizetype".to_owned(), v);
        }
        (Some((p, _)), None) | (None, Some((p, _))) => {
            diagnostics.push(Diagnostic::error(
                p.binding_node().byte_range(),
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
        if let Some((p, v)) = map
            .get(k0)
            .and_then(|p| Value::build(ctx, p, diagnostics).map(|v| (p, v)))
        {
            if attributes.is_empty() {
                // uic would otherwise generate invalid code, which might look correct but is
                // actually parsed as a function declaration:
                //     QSizePolicy sizePolicy();
                diagnostics.push(Diagnostic::error(
                    p.binding_node().byte_range(),
                    "cannot specify stretch without horizontal and vertical policies",
                ));
            } else {
                properties.insert(k1.to_owned(), v);
            }
        }
    }

    let known_property_names = [
        "horizontalPolicy",
        "verticalPolicy",
        "horizontalStretch",
        "verticalStretch",
    ];
    for (_, p) in map
        .iter()
        .filter(|(n, _)| !known_property_names.contains(n))
    {
        diagnostics.push(Diagnostic::error(
            p.binding_node().byte_range(),
            "unknown property of size policy",
        ));
    }

    (attributes, properties)
}

/// QComboBox/QAbstractItemView item.
#[derive(Clone, Debug)]
pub struct ModelItem {
    pub properties: HashMap<String, Value>,
}

impl ModelItem {
    pub(super) fn with_text(s: String, k: StringKind) -> Self {
        let properties =
            HashMap::from([("text".to_owned(), Value::Simple(SimpleValue::String(s, k)))]);
        ModelItem { properties }
    }

    pub(super) fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let tag = BytesStart::borrowed_name(b"item");
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        serialize_item_properties_to_xml(writer, &self.properties)?;
        writer.write_event(Event::End(tag.to_end()))
    }
}

fn serialize_item_properties_to_xml<W>(
    writer: &mut XmlWriter<W>,
    properties: &HashMap<String, Value>,
) -> XmlResult<()>
where
    W: io::Write,
{
    for (k, v) in properties.iter().sorted_by_key(|&(k, _)| k) {
        let tag = BytesStart::borrowed_name(b"property").with_attributes([("name", k.as_ref())]);
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        v.serialize_to_xml(writer)?;
        writer.write_event(Event::End(tag.to_end()))?;
    }
    Ok(())
}

/// Map of palette color role to brush.
#[derive(Clone, Debug, Default)]
pub struct PaletteColorGroup {
    pub roles: HashMap<String, Value>,
}

impl PaletteColorGroup {
    pub(super) fn new(
        ctx: &ObjectContext,
        map: &HashMap<&str, PropertyCode>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let roles = map
            .iter()
            .filter_map(|(&k, p)| {
                let v = Value::build(ctx, p, diagnostics)?;
                Some((qtname::to_ascii_capitalized(k), v))
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
