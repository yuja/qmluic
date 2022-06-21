use super::context::BuildDocContext;
use super::expr::{PropertyValue, Value};
use super::gadget::ModelItem;
use super::layout::Layout;
use super::property::{self, PropertiesMap, PropertyDescValue, PropertySetter, WithNode};
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectNode;
use crate::typemap::{Class, TypeSpace};
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::io;

/// Reserved name for <addaction name="separator"/>.
const ACTION_SEPARATOR_NAME: &str = "separator";

/// Variant for the object definitions which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub enum UiObject {
    Action(Action),
    ActionSeparator,
    Layout(Layout),
    Menu(Widget),
    Widget(Widget),
}

impl UiObject {
    /// Creates a serializable tree by visiting the given node and its children recursively.
    pub(super) fn build(
        ctx: &BuildDocContext,
        obj_node: ObjectNode,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let cls = obj_node.class();
        let binding_map = diagnostics
            .consume_err(obj_node.obj().build_binding_map(ctx.source))
            .unwrap_or_default();
        let properties_map =
            property::collect_properties_with_node(ctx, cls, &binding_map, diagnostics);

        if cls.is_derived_from(&ctx.classes.action) {
            confine_children(obj_node, diagnostics);
            UiObject::Action(Action::new(
                cls,
                obj_node
                    .obj()
                    .object_id()
                    .map(|n| n.to_str(ctx.source).to_owned()),
                properties_map,
                diagnostics,
            ))
        } else if cls.is_derived_from(&ctx.classes.action_separator) {
            confine_children(obj_node, diagnostics);
            UiObject::ActionSeparator
        } else if cls.is_derived_from(&ctx.classes.layout) {
            UiObject::Layout(Layout::build(ctx, obj_node, properties_map, diagnostics))
        } else if cls.is_derived_from(&ctx.classes.menu) {
            UiObject::Menu(Widget::build(ctx, obj_node, properties_map, diagnostics))
        } else if cls.is_derived_from(&ctx.classes.widget) {
            UiObject::Widget(Widget::build(ctx, obj_node, properties_map, diagnostics))
        } else {
            diagnostics.push(Diagnostic::error(
                obj_node.obj().node().byte_range(),
                format!(
                    "class '{}' is not a QAction, QLayout, nor QWidget",
                    cls.qualified_cxx_name()
                ),
            ));
            // but process as widget to report as many errors as possible
            UiObject::Widget(Widget::build(ctx, obj_node, properties_map, diagnostics))
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
            ActionSeparator => Ok(()),
            Layout(x) => x.serialize_to_xml(writer),
            Menu(x) | Widget(x) => x.serialize_to_xml(writer),
        }
    }
}

/// Action definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Action {
    pub name: Option<String>,
    pub properties: HashMap<String, (Value, PropertySetter)>,
}

impl Action {
    pub(super) fn new(
        cls: &Class,
        name: Option<String>,
        properties_map: PropertiesMap,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let properties = property::make_serializable_properties(cls, properties_map, diagnostics);
        Action { name, properties }
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
    pub attributes: HashMap<String, (Value, PropertySetter)>,
    pub properties: HashMap<String, (Value, PropertySetter)>,
    pub actions: Vec<String>,
    pub items: Vec<ModelItem>,
    pub children: Vec<UiObject>,
}

impl Widget {
    /// Creates a serializable tree by visiting the children recursively.
    pub(super) fn build(
        ctx: &BuildDocContext,
        obj_node: ObjectNode,
        properties_map: PropertiesMap,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let children = if obj_node.class().is_derived_from(&ctx.classes.tab_widget) {
            process_tab_widget_children(ctx, obj_node, diagnostics)
        } else {
            process_widget_children(ctx, obj_node, diagnostics)
        };

        Self::new(
            ctx,
            obj_node.class(),
            obj_node
                .obj()
                .object_id()
                .map(|n| n.to_str(ctx.source).to_owned()),
            properties_map,
            children,
            diagnostics,
        )
    }

    pub(super) fn new(
        ctx: &BuildDocContext,
        class: &Class,
        name: Option<String>,
        mut properties_map: PropertiesMap,
        mut children: Vec<UiObject>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let actions = match properties_map.remove("actions") {
            Some(WithNode {
                data:
                    PropertyDescValue {
                        value: PropertyValue::ObjectRefList(refs),
                        ..
                    },
                ..
            }) => refs
                .into_iter()
                .map(|id| {
                    if ctx
                        .object_tree
                        .get_by_id(&id)
                        .expect("object ref must be valid")
                        .class()
                        .is_derived_from(&ctx.classes.action_separator)
                    {
                        ACTION_SEPARATOR_NAME.to_owned()
                    } else {
                        id
                    }
                })
                .collect(),
            Some(x) => {
                diagnostics.push(Diagnostic::error(
                    x.node().byte_range(),
                    "not an actions list",
                ));
                vec![]
            }
            None => collect_action_like_children(ctx, &mut children),
        };

        let items = if class.is_derived_from(&ctx.classes.combo_box)
            || class.is_derived_from(&ctx.classes.list_widget)
        {
            match properties_map.remove("model") {
                Some(WithNode {
                    data:
                        PropertyDescValue {
                            value: PropertyValue::ItemModel(items),
                            ..
                        },
                    ..
                }) => items,
                Some(x) => {
                    diagnostics.push(Diagnostic::error(
                        x.node().byte_range(),
                        "not an item model",
                    ));
                    vec![]
                }
                None => vec![],
            }
        } else {
            vec![]
        };

        let mut attributes = HashMap::new();
        if class.is_derived_from(&ctx.classes.table_view) {
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
        if class.is_derived_from(&ctx.classes.tree_view) {
            flatten_object_properties_into_attributes(
                &mut attributes,
                &mut properties_map,
                "header",
                diagnostics,
            );
        }
        let mut properties =
            property::make_serializable_properties(class, properties_map, diagnostics);
        if class.is_derived_from(&ctx.classes.push_button) {
            // see metatype_tweak.rs, "default" is a reserved word
            if let Some((mut k, (v, _))) = properties.remove_entry("default_") {
                k.pop();
                properties.insert(k, (v, PropertySetter::StdSet));
            }
        }

        Widget {
            class: class.qualified_cxx_name().into_owned(),
            name,
            attributes,
            properties,
            actions,
            items,
            children,
        }
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

        for e in &self.items {
            e.serialize_to_xml(writer)?;
        }

        for c in &self.children {
            c.serialize_to_xml(writer)?;
        }

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

fn process_tab_widget_children(
    ctx: &BuildDocContext,
    obj_node: ObjectNode,
    diagnostics: &mut Diagnostics,
) -> Vec<UiObject> {
    obj_node
        .children()
        .map(|n| {
            let mut o = UiObject::build(ctx, n, diagnostics);
            match &mut o {
                UiObject::Menu(w) | UiObject::Widget(w) => {
                    let attached_type_map = diagnostics
                        .consume_err(n.obj().build_attached_type_map(ctx.source))
                        .unwrap_or_default();
                    // TODO: resolve against imported types,
                    if let Some(m) = attached_type_map.get(["QTabWidget"].as_ref()) {
                        w.attributes.extend(
                            property::collect_properties(
                                ctx,
                                &ctx.classes.tab_widget_attached,
                                m,
                                diagnostics,
                            )
                            .into_iter()
                            // don't care the setter since uic will handle them specially
                            .map(|(k, v)| (k, (v, PropertySetter::StdSet))),
                        );
                    }
                }
                UiObject::Action(_) | UiObject::ActionSeparator | UiObject::Layout(_) => {}
            }
            o
        })
        .collect()
}

fn process_widget_children(
    ctx: &BuildDocContext,
    obj_node: ObjectNode,
    diagnostics: &mut Diagnostics,
) -> Vec<UiObject> {
    obj_node
        .children()
        .map(|n| UiObject::build(ctx, n, diagnostics))
        .collect()
}

fn collect_action_like_children(ctx: &BuildDocContext, children: &mut [UiObject]) -> Vec<String> {
    children
        .iter_mut()
        .filter_map(|child| match child {
            UiObject::Action(a) => Some(
                a.name
                    .get_or_insert_with(|| ctx.generate_object_id("action"))
                    .to_owned(),
            ),
            UiObject::ActionSeparator => Some(ACTION_SEPARATOR_NAME.to_owned()),
            UiObject::Menu(w) => Some(
                w.name
                    .get_or_insert_with(|| ctx.generate_object_id("menu"))
                    .to_owned(),
            ),
            UiObject::Layout(_) | UiObject::Widget(_) => None,
        })
        .collect()
}

fn flatten_object_properties_into_attributes(
    attributes: &mut HashMap<String, (Value, PropertySetter)>,
    properties_map: &mut PropertiesMap,
    name: &str,
    diagnostics: &mut Diagnostics,
) {
    match properties_map.remove(name) {
        Some(WithNode {
            data:
                PropertyDescValue {
                    value: PropertyValue::ObjectProperties(_, props),
                    ..
                },
            ..
        }) => {
            attributes.extend(props.into_iter().filter_map(|(k, v)| {
                diagnostics
                    .consume_err(v.into_serializable_setter())
                    .map(|x| (concat_camel_case_names(name, &k), x))
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
