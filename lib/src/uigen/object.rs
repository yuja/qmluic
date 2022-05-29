use super::expr::Value;
use super::layout::Layout;
use super::property;
use super::xmlutil;
use super::{BuildContext, XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Expression, Node, UiBindingValue, UiObjectDefinition};
use crate::typemap::{Class, Type, TypeSpace};
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

/// Type of the object parent.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum ContainerKind {
    Any,
    TabWidget,
}

/// Variant for the object definitions which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub enum UiObject {
    Action(Action),
    Layout(Layout),
    Widget(Widget),
}

impl UiObject {
    /// Generates object and its children recursively from the given `obj` definition.
    pub(super) fn from_object_definition(
        ctx: &BuildContext,
        cls: &Class,
        obj: &UiObjectDefinition,
        container_kind: ContainerKind,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        if cls.is_derived_from(&ctx.action_class) {
            Action::from_object_definition(ctx, cls, obj, diagnostics).map(UiObject::Action)
        } else if cls.is_derived_from(&ctx.layout_class) {
            Layout::from_object_definition(ctx, cls, obj, diagnostics).map(UiObject::Layout)
        } else if cls.is_derived_from(&ctx.widget_class) {
            Widget::from_object_definition(ctx, cls, obj, container_kind, diagnostics)
                .map(UiObject::Widget)
        } else {
            diagnostics.push(Diagnostic::error(
                obj.node().byte_range(),
                format!(
                    "class '{}' is not a QAction, QLayout, nor QWidget",
                    cls.qualified_name()
                ),
            ));
            None
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
    pub properties: HashMap<String, Value>,
}

impl Action {
    /// Generates action of `cls` type from the given `obj` definition.
    ///
    /// The given `cls` is supposed to be of `QAction` type.
    fn from_object_definition(
        ctx: &BuildContext,
        cls: &Class,
        obj: &UiObjectDefinition,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let binding_map = diagnostics.consume_err(obj.build_binding_map(ctx.source))?;
        confine_children(cls, obj, diagnostics);
        Some(Action {
            name: obj.object_id().map(|n| n.to_str(ctx.source).to_owned()),
            properties: property::collect_properties(ctx, cls, &binding_map, &[], diagnostics),
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

        property::serialize_properties_to_xml(writer, "property", &self.properties)?;

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Widget definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Widget {
    pub class: String,
    pub name: Option<String>,
    pub attributes: HashMap<String, Value>,
    pub properties: HashMap<String, Value>,
    pub actions: Vec<String>,
    pub children: Vec<UiObject>,
}

impl Widget {
    /// Generates widget of `cls` type and its children recursively from the given `obj`
    /// definition.
    pub(super) fn from_object_definition(
        ctx: &BuildContext,
        cls: &Class,
        obj: &UiObjectDefinition,
        container_kind: ContainerKind,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let attached_type_map = diagnostics.consume_err(obj.build_attached_type_map(ctx.source))?;
        let attributes = match container_kind {
            ContainerKind::Any => HashMap::new(),
            ContainerKind::TabWidget => {
                // TODO: resolve against imported types,
                attached_type_map
                    .get(["QTabWidget"].as_ref())
                    .map(|m| {
                        property::collect_properties(
                            ctx,
                            &ctx.tab_widget_attached_class,
                            m,
                            &[],
                            diagnostics,
                        )
                    })
                    .unwrap_or_default()
            }
        };

        let binding_map = diagnostics.consume_err(obj.build_binding_map(ctx.source))?;
        let mut properties =
            property::collect_properties(ctx, cls, &binding_map, &["actions"], diagnostics);
        if cls.is_derived_from(&ctx.push_button_class) {
            // see metatype_tweak.rs, "default" is a reserved word
            if let Some((mut k, v)) = properties.remove_entry("default_") {
                k.pop();
                properties.insert(k, v);
            }
        }
        let actions = binding_map
            .get("actions")
            .map(|v| collect_identifiers(v, ctx.source, diagnostics))
            .unwrap_or_default();

        let child_container_kind = if cls.is_derived_from(&ctx.tab_widget_class) {
            ContainerKind::TabWidget
        } else {
            ContainerKind::Any
        };
        let children = obj
            .child_object_nodes()
            .iter()
            .filter_map(|&n| {
                let (obj, cls) = resolve_object_definition(ctx, n, diagnostics)?;
                UiObject::from_object_definition(ctx, &cls, &obj, child_container_kind, diagnostics)
            })
            .collect();

        Some(Widget {
            class: cls.name().to_owned(),
            name: obj.object_id().map(|n| n.to_str(ctx.source).to_owned()),
            attributes,
            properties,
            actions,
            children,
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

        property::serialize_properties_to_xml(writer, "attribute", &self.attributes)?;
        property::serialize_properties_to_xml(writer, "property", &self.properties)?;

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

pub(super) fn resolve_object_definition<'a, 't>(
    ctx: &BuildContext<'a, '_>,
    node: Node<'t>,
    diagnostics: &mut Diagnostics,
) -> Option<(UiObjectDefinition<'t>, Class<'a>)> {
    let obj = diagnostics.consume_err(UiObjectDefinition::from_node(node, ctx.source))?;
    // TODO: resolve against imported types: Qml.Type -> Cxx::Type -> type object
    let type_name = obj.type_name().to_string(ctx.source);
    if let Some(Type::Class(cls)) = ctx.module.get_type(&type_name) {
        Some((obj, cls))
    } else {
        diagnostics.push(Diagnostic::error(
            obj.type_name().node().byte_range(),
            format!("unknown object type: {type_name}"),
        ));
        None
    }
}

pub(super) fn confine_children(
    cls: &Class,
    obj: &UiObjectDefinition,
    diagnostics: &mut Diagnostics,
) {
    if let Some(n) = obj.child_object_nodes().first() {
        // TODO: error on obj.node(), and add hint to child nodes
        diagnostics.push(Diagnostic::error(
            n.byte_range(),
            format!("'{}' should have no children", cls.qualified_name()),
        ));
    }
}

fn collect_identifiers(
    value: &UiBindingValue,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Vec<String> {
    match value {
        UiBindingValue::Node(n) => {
            parse_as_identifier_array(*n, source, diagnostics).unwrap_or_default()
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
