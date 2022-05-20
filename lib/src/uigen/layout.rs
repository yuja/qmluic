use super::expr::{self, ConstantExpression};
use super::object::{self, Widget};
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{UiBindingMap, UiObjectDefinition};
use crate::typemap::{Class, TypeSpace};
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::io;

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
            properties: object::collect_properties(cls, &binding_map, source, diagnostics),
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

        object::serialize_properties_to_xml(writer, &self.properties)?;

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
        content: LayoutItemContent,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let attached_type_map = diagnostics.consume_err(obj.build_attached_type_map(source))?;
        Some(LayoutItem {
            properties: attached_type_map
                .get(["QLayoutItem"].as_ref()) // TODO: resolve against imported types
                .map(|m| LayoutItemProperties::from_binding_map(cls, m, source, diagnostics))
                .unwrap_or_default(),
            content,
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
    pub alignment: Option<String>,
    pub column: Option<i32>,
    pub column_span: Option<i32>,
    pub row: Option<i32>,
    pub row_span: Option<i32>,
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
                "alignment" => {
                    // type name is resolved within C++ metatype space, which is correct
                    properties.alignment = expr::resolve_type_scoped_for_node(
                        parent_space,
                        "Qt::Alignment",
                        value.node(),
                        diagnostics,
                    )
                    .and_then(|ty| {
                        expr::format_enum_expression(parent_space, &ty, value, source, diagnostics)
                    });
                }
                "column" => {
                    properties.column =
                        expr::evaluate_i32(parent_space, value, source, diagnostics);
                }
                "columnSpan" => {
                    properties.column_span =
                        expr::evaluate_i32(parent_space, value, source, diagnostics);
                }
                "row" => {
                    properties.row = expr::evaluate_i32(parent_space, value, source, diagnostics);
                }
                "rowSpan" => {
                    properties.row_span =
                        expr::evaluate_i32(parent_space, value, source, diagnostics);
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
        if let Some(v) = &self.alignment {
            tag.push_attribute(("alignment", v.as_ref()));
        }
        if let Some(v) = self.column {
            tag.push_attribute(("column", v.to_string().as_ref()));
        }
        if let Some(v) = self.column_span {
            tag.push_attribute(("columnspan", v.to_string().as_ref()));
        }
        if let Some(v) = self.row {
            tag.push_attribute(("row", v.to_string().as_ref()));
        }
        if let Some(v) = self.row_span {
            tag.push_attribute(("rowspan", v.to_string().as_ref()));
        }
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
            properties: object::collect_properties(cls, &binding_map, source, diagnostics),
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

        object::serialize_properties_to_xml(writer, &self.properties)?;

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}
