use super::expr::{PropertyValue, Value};
use super::layout::Layout;
use super::property::{self, WithNode};
use super::{BuildDocContext, XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectNode;
use crate::typemap::TypeSpace;
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::io;

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
    /// Generates object and its children recursively from the given `obj_node`.
    pub(super) fn from_object_node(
        ctx: &BuildDocContext,
        obj_node: ObjectNode,
        container_kind: ContainerKind,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let cls = obj_node.class();
        if cls.is_derived_from(&ctx.action_class) {
            // TODO: hack for menu separator: <addaction name="separator"/> is reserved by uic
            if obj_node
                .obj()
                .object_id()
                .map(|n| n.to_str(ctx.source) == "separator")
                .unwrap_or(false)
            {
                None
            } else {
                Action::from_object_node(ctx, obj_node, diagnostics).map(UiObject::Action)
            }
        } else if cls.is_derived_from(&ctx.layout_class) {
            Layout::from_object_node(ctx, obj_node, diagnostics).map(UiObject::Layout)
        } else if cls.is_derived_from(&ctx.widget_class) {
            Widget::from_object_node(ctx, obj_node, container_kind, diagnostics)
                .map(UiObject::Widget)
        } else {
            diagnostics.push(Diagnostic::error(
                obj_node.obj().node().byte_range(),
                format!(
                    "class '{}' is not a QAction, QLayout, nor QWidget",
                    cls.qualified_cxx_name()
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
    /// Generates action from the given `obj_node`.
    ///
    /// The given `obj_node` is supposed to be of `QAction` type.
    fn from_object_node(
        ctx: &BuildDocContext,
        obj_node: ObjectNode,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let binding_map = diagnostics.consume_err(obj_node.obj().build_binding_map(ctx.source))?;
        confine_children(obj_node, diagnostics);
        Some(Action {
            name: obj_node
                .obj()
                .object_id()
                .map(|n| n.to_str(ctx.source).to_owned()),
            properties: property::collect_properties(
                ctx,
                obj_node.class(),
                &binding_map,
                diagnostics,
            ),
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
    /// Generates widget and its children recursively from the given `obj_node`.
    pub(super) fn from_object_node(
        ctx: &BuildDocContext,
        obj_node: ObjectNode,
        container_kind: ContainerKind,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let attached_type_map =
            diagnostics.consume_err(obj_node.obj().build_attached_type_map(ctx.source))?;
        let mut attributes = match container_kind {
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
                            diagnostics,
                        )
                    })
                    .unwrap_or_default()
            }
        };

        let binding_map = diagnostics.consume_err(obj_node.obj().build_binding_map(ctx.source))?;
        let mut properties_map = property::collect_properties_with_node(
            ctx,
            obj_node.class(),
            &binding_map,
            diagnostics,
        );
        let actions = match properties_map.remove("actions") {
            Some(WithNode {
                value: PropertyValue::ObjectRefList(refs),
                ..
            }) => refs,
            Some(x) => {
                diagnostics.push(Diagnostic::error(
                    x.node().byte_range(),
                    "not an actions list",
                ));
                vec![]
            }
            None => vec![],
        };
        if obj_node.class().is_derived_from(&ctx.table_view_class) {
            flatten_object_properties_into_attributes(
                &mut attributes,
                &mut properties_map,
                "horizontalHeader",
                diagnostics,
            );
            flatten_object_properties_into_attributes(
                &mut attributes,
                &mut properties_map,
                "verticalHeader",
                diagnostics,
            );
        }
        if obj_node.class().is_derived_from(&ctx.tree_view_class) {
            flatten_object_properties_into_attributes(
                &mut attributes,
                &mut properties_map,
                "header",
                diagnostics,
            );
        }
        let mut properties = property::make_serializable_properties(properties_map, diagnostics);
        if obj_node.class().is_derived_from(&ctx.push_button_class) {
            // see metatype_tweak.rs, "default" is a reserved word
            if let Some((mut k, v)) = properties.remove_entry("default_") {
                k.pop();
                properties.insert(k, v);
            }
        }

        let child_container_kind = if obj_node.class().is_derived_from(&ctx.tab_widget_class) {
            ContainerKind::TabWidget
        } else {
            ContainerKind::Any
        };
        let children = obj_node
            .children()
            .filter_map(|n| UiObject::from_object_node(ctx, n, child_container_kind, diagnostics))
            .collect();

        Some(Widget {
            class: obj_node.class().qualified_cxx_name().into_owned(),
            name: obj_node
                .obj()
                .object_id()
                .map(|n| n.to_str(ctx.source).to_owned()),
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

fn flatten_object_properties_into_attributes(
    attributes: &mut HashMap<String, Value>,
    properties_map: &mut HashMap<String, WithNode<'_, PropertyValue>>,
    name: &str,
    diagnostics: &mut Diagnostics,
) {
    match properties_map.remove(name) {
        Some(WithNode {
            value: PropertyValue::ObjectProperties(props),
            ..
        }) => {
            attributes.extend(props.into_iter().filter_map(|(k, v)| {
                diagnostics
                    .consume_err(v.into_serializable())
                    .map(|v| (concat_camel_case_names(name, &k), v))
            }));
        }
        Some(x) => {
            diagnostics.push(Diagnostic::error(
                x.node().byte_range(),
                "not a properties map",
            ));
        }
        None => {}
    }
}

fn concat_camel_case_names(head: &str, tail: &str) -> String {
    let mut name = head.to_owned();
    let mut chars = tail.chars();
    if let Some(c) = chars.next() {
        name.push(c.to_ascii_uppercase());
        name.extend(chars);
    }
    name
}

pub(super) fn confine_children(obj_node: ObjectNode, diagnostics: &mut Diagnostics) {
    if let Some(n) = obj_node.children().next() {
        // TODO: error on obj.node(), and add hint to child nodes
        diagnostics.push(Diagnostic::error(
            n.obj().node().byte_range(),
            format!(
                "'{}' should have no children",
                obj_node.class().qualified_cxx_name()
            ),
        ));
    }
}
