use super::expr::{self, ConstantExpression};
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Expression, Node, UiBindingMap, UiBindingValue, UiObjectDefinition};
use crate::typemap::{Class, TypeSpace};
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::io;

/// Top-level object wrapper to be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct UiForm {
    pub class: Option<String>,
    pub root_object: UiObject,
}

impl UiForm {
    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let tag = BytesStart::borrowed_name(b"ui").with_attributes([("version", "4.0")]);
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        if let Some(name) = &self.class {
            xmlutil::write_tagged_str(writer, "class", name)?;
        }
        self.root_object.serialize_to_xml(writer)?;
        writer.write_event(Event::End(tag.to_end()))?;
        writer.write(b"\n")
    }
}

/// Variant for the object definitions which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub enum UiObject {
    Action(Action),
    Layout(Layout),
    Widget(Widget),
}

impl UiObject {
    /// Generates object of `cls` type from the given `obj` definition.
    ///
    /// Child objects are NOT generated recursively.
    pub fn from_object_definition(
        cls: &Class,
        obj: &UiObjectDefinition,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        // TODO: leverage type map to dispatch
        if cls.name() == "QAction" {
            Action::from_object_definition(cls, obj, source, diagnostics).map(UiObject::Action)
        } else if cls.name().ends_with("Layout") {
            Layout::from_object_definition(cls, obj, source, diagnostics).map(UiObject::Layout)
        } else {
            Widget::from_object_definition(cls, obj, source, diagnostics).map(UiObject::Widget)
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use UiObject::*;
        match self {
            Action(x) => x.serialize_to_xml(writer),
            Layout(x) => x.serialize_to_xml(writer),
            Widget(x) => x.serialize_to_xml(writer),
        }
    }
}

/// Action definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Action {
    pub name: Option<String>,
    pub properties: HashMap<String, ConstantExpression>,
}

impl Action {
    /// Generates action of `cls` type from the given `obj` definition.
    ///
    /// The given `cls` is supposed to be of `QAction` type.
    pub fn from_object_definition(
        cls: &Class,
        obj: &UiObjectDefinition,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let binding_map = diagnostics.consume_err(obj.build_binding_map(source))?;
        Some(Action {
            name: obj.object_id().map(|n| n.to_str(source).to_owned()),
            properties: collect_properties(cls, &binding_map, source, diagnostics),
        })
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"action");
        if let Some(n) = &self.name {
            tag.push_attribute(("name", n.as_ref()));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        serialize_properties_to_xml(writer, &self.properties)?;

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Widget definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Widget {
    pub class: String,
    pub name: Option<String>,
    pub properties: HashMap<String, ConstantExpression>,
    pub actions: Vec<String>,
    pub children: Vec<UiObject>,
}

impl Widget {
    /// Generates widget of `cls` type from the given `obj` definition.
    ///
    /// Child objects are NOT constructed recursively.
    pub fn from_object_definition(
        cls: &Class,
        obj: &UiObjectDefinition,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let binding_map = diagnostics.consume_err(obj.build_binding_map(source))?;
        Some(Widget {
            class: cls.name().to_owned(),
            name: obj.object_id().map(|n| n.to_str(source).to_owned()),
            properties: collect_properties(cls, &binding_map, source, diagnostics),
            actions: binding_map
                .get("actions")
                .map(|v| collect_identifiers(v, source, diagnostics))
                .unwrap_or(vec![]),
            children: vec![],
        })
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"widget");
        tag.push_attribute(("class", self.class.as_ref()));
        if let Some(n) = &self.name {
            tag.push_attribute(("name", n.as_ref()));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        serialize_properties_to_xml(writer, &self.properties)?;

        for n in &self.actions {
            writer.write_event(Event::Empty(
                BytesStart::borrowed_name(b"addaction").with_attributes([("name", n.as_ref())]),
            ))?;
        }

        for c in &self.children {
            c.serialize_to_xml(writer)?;
        }

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Layout item definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct LayoutItem {
    pub properties: LayoutItemProperties,
    pub content: LayoutItemContent,
}

impl LayoutItem {
    /// Generates layout item of `cls` type from the given `obj` definition.
    pub fn from_object_definition(
        cls: &Class,
        obj: &UiObjectDefinition,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let attached_type_map = diagnostics.consume_err(obj.build_attached_type_map(source))?;
        Some(LayoutItem {
            properties: attached_type_map
                .get(["QLayoutItem"].as_ref()) // TODO: resolve against imported types
                .map(|m| LayoutItemProperties::from_binding_map(cls, m, source, diagnostics))
                .unwrap_or_default(),
            content: LayoutItemContent::from_object_definition(cls, obj, source, diagnostics)?,
        })
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"item");
        self.properties.push_attributes_to_item_tag(&mut tag);
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        self.content.serialize_to_xml(writer)?;

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Layout properties of [`LayoutItem`].
#[derive(Clone, Debug, Default)]
pub struct LayoutItemProperties {
    pub column: Option<i32>,
    pub row: Option<i32>,
}

impl LayoutItemProperties {
    fn from_binding_map<'a, P>(
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        binding_map: &UiBindingMap,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Self
    where
        P: TypeSpace<'a>,
    {
        let mut properties = LayoutItemProperties::default();
        for (&name, value) in binding_map {
            match name {
                "column" => {
                    properties.column =
                        expr::evaluate_i32(parent_space, value, source, diagnostics);
                }
                "row" => {
                    properties.row = expr::evaluate_i32(parent_space, value, source, diagnostics);
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        value.node().byte_range(),
                        format!("unknown property of QLayoutItem: {}", name),
                    ));
                }
            }
        }
        properties
    }

    fn push_attributes_to_item_tag(&self, tag: &mut BytesStart) {
        self.column
            .map(|v| tag.push_attribute(("column", v.to_string().as_ref())));
        self.row
            .map(|v| tag.push_attribute(("row", v.to_string().as_ref())));
    }
}

/// Variant for the object managed by the layout item.
#[derive(Clone, Debug)]
pub enum LayoutItemContent {
    Layout(Layout),
    SpacerItem(SpacerItem),
    Widget(Widget),
}

impl LayoutItemContent {
    fn from_object_definition(
        cls: &Class,
        obj: &UiObjectDefinition,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        // TODO: leverage type map to dispatch
        if cls.name().ends_with("Layout") {
            Layout::from_object_definition(cls, obj, source, diagnostics)
                .map(LayoutItemContent::Layout)
        } else if cls.name() == "QSpacerItem" {
            SpacerItem::from_object_definition(cls, obj, source, diagnostics)
                .map(LayoutItemContent::SpacerItem)
        } else {
            Widget::from_object_definition(cls, obj, source, diagnostics)
                .map(LayoutItemContent::Widget)
        }
    }

    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use LayoutItemContent::*;
        match self {
            Layout(x) => x.serialize_to_xml(writer),
            SpacerItem(x) => x.serialize_to_xml(writer),
            Widget(x) => x.serialize_to_xml(writer),
        }
    }
}

/// Spacer item definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct SpacerItem {
    pub name: Option<String>,
    pub properties: HashMap<String, ConstantExpression>,
}

impl SpacerItem {
    /// Generates spacer item of `cls` type from the given `obj` definition.
    ///
    /// The given `cls` is supposed to be of `QSpacerItem` type.
    pub fn from_object_definition(
        cls: &Class,
        obj: &UiObjectDefinition,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let binding_map = diagnostics.consume_err(obj.build_binding_map(source))?;
        Some(SpacerItem {
            name: obj.object_id().map(|n| n.to_str(source).to_owned()),
            properties: collect_properties(cls, &binding_map, source, diagnostics),
        })
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"spacer");
        if let Some(n) = &self.name {
            tag.push_attribute(("name", n.as_ref()));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        serialize_properties_to_xml(writer, &self.properties)?;

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Layout definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Layout {
    pub class: String,
    pub name: Option<String>,
    pub properties: HashMap<String, ConstantExpression>,
    pub children: Vec<LayoutItem>,
}

impl Layout {
    /// Generates layout of `cls` type from the given `obj` definition.
    ///
    /// Child items are NOT constructed recursively.
    pub fn from_object_definition(
        cls: &Class,
        obj: &UiObjectDefinition,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let binding_map = diagnostics.consume_err(obj.build_binding_map(source))?;
        Some(Layout {
            class: cls.name().to_owned(),
            name: obj.object_id().map(|n| n.to_str(source).to_owned()),
            properties: collect_properties(cls, &binding_map, source, diagnostics),
            children: vec![],
        })
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"layout");
        tag.push_attribute(("class", self.class.as_ref()));
        if let Some(n) = &self.name {
            tag.push_attribute(("name", n.as_ref()));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        serialize_properties_to_xml(writer, &self.properties)?;

        for c in &self.children {
            c.serialize_to_xml(writer)?;
        }

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Parses the given `binding_map` into a map of constant expressions.
///
/// Unparsable properties are excluded from the resulting map so as many diagnostic messages
/// will be generated as possible.
fn collect_properties(
    cls: &Class,
    binding_map: &UiBindingMap,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> HashMap<String, ConstantExpression> {
    binding_map
        .iter()
        .filter(|(&name, _)| name != "actions") // TODO: only for QWidget subclasses
        .filter_map(|(&name, value)| {
            if let Some(ty) = cls.get_property_type(name) {
                ConstantExpression::from_binding_value(cls, &ty, value, source, diagnostics)
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

fn collect_identifiers(
    value: &UiBindingValue,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Vec<String> {
    match value {
        UiBindingValue::Node(n) => {
            parse_as_identifier_array(*n, source, diagnostics).unwrap_or(vec![])
        }
        UiBindingValue::Map(n, _) => {
            diagnostics.push(Diagnostic::error(
                n.byte_range(),
                "binding map cannot be parsed as array of identifiers",
            ));
            vec![]
        }
    }
}

fn parse_as_identifier_string(
    node: Node,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<String> {
    match diagnostics.consume_err(Expression::from_node(node, source))? {
        Expression::Identifier(n) => Some(n.to_str(source).to_owned()),
        _ => {
            diagnostics.push(Diagnostic::error(node.byte_range(), "not an identifier"));
            None
        }
    }
}

fn parse_as_identifier_array(
    node: Node,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<Vec<String>> {
    match diagnostics.consume_err(Expression::from_node(node, source))? {
        Expression::Array(ns) => ns
            .iter()
            .map(|&n| parse_as_identifier_string(n, source, diagnostics))
            .collect(),
        _ => {
            diagnostics.push(Diagnostic::error(node.byte_range(), "not an array"));
            None
        }
    }
}

fn serialize_properties_to_xml<W>(
    writer: &mut XmlWriter<W>,
    properties: &HashMap<String, ConstantExpression>,
) -> XmlResult<()>
where
    W: io::Write,
{
    let mut pairs: Vec<_> = properties.iter().collect();
    pairs.sort_by_key(|&(k, _)| k);
    for (k, v) in pairs {
        let tag = BytesStart::borrowed_name(b"property").with_attributes([("name", k.as_ref())]);
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        v.serialize_to_xml(writer)?;
        writer.write_event(Event::End(tag.to_end()))?;
    }
    Ok(())
}
