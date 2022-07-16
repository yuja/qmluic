use super::context::BuildDocContext;
use super::expr::{PropertyValue, Value};
use super::gadget::ModelItem;
use super::layout::Layout;
use super::objcode::PropertyCode;
use super::property::{self, PropertySetter};
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
        if cls.is_derived_from(&ctx.classes.action) {
            confine_children(obj_node, diagnostics);
            UiObject::Action(Action::new(
                ctx,
                obj_node.name(),
                ctx.code_map_for_object(obj_node).properties(),
                diagnostics,
            ))
        } else if cls.is_derived_from(&ctx.classes.action_separator) {
            confine_children(obj_node, diagnostics);
            UiObject::ActionSeparator
        } else if cls.is_derived_from(&ctx.classes.layout) {
            let properties_map = property::make_properties_from_code_map(
                &ctx.make_object_context(),
                ctx.code_map_for_object(obj_node).properties(),
                diagnostics,
            );
            UiObject::Layout(Layout::build(ctx, obj_node, properties_map, diagnostics))
        } else if cls.is_derived_from(&ctx.classes.menu) {
            UiObject::Menu(Widget::build(ctx, obj_node, diagnostics))
        } else if cls.is_derived_from(&ctx.classes.widget) {
            UiObject::Widget(Widget::build(ctx, obj_node, diagnostics))
        } else {
            diagnostics.push(Diagnostic::error(
                obj_node.obj().node().byte_range(),
                format!(
                    "class '{}' is not a QAction, QLayout, nor QWidget",
                    cls.qualified_cxx_name()
                ),
            ));
            // but process as widget to report as many errors as possible
            UiObject::Widget(Widget::build(ctx, obj_node, diagnostics))
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
    pub name: String,
    pub properties: HashMap<String, (Value, PropertySetter)>,
}

impl Action {
    pub(super) fn new(
        ctx: &BuildDocContext,
        name: impl Into<String>,
        properties_code_map: &HashMap<&str, PropertyCode>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let properties = property::make_serializable_map(
            &ctx.make_object_context(),
            properties_code_map,
            &[],
            diagnostics,
        );
        Action {
            name: name.into(),
            properties,
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let tag =
            BytesStart::borrowed_name(b"action").with_attributes([("name", self.name.as_ref())]);
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
    pub name: String,
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
            obj_node.name(),
            ctx.code_map_for_object(obj_node).properties(),
            children,
            diagnostics,
        )
    }

    pub(super) fn new(
        ctx: &BuildDocContext,
        class: &Class,
        name: impl Into<String>,
        properties_code_map: &HashMap<&str, PropertyCode>,
        children: Vec<UiObject>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let mut pseudo_property_names = vec!["actions", "model"];

        let actions = if let Some(p) = properties_code_map.get("actions") {
            match PropertyValue::build(&ctx.make_object_context(), p, diagnostics) {
                Some(PropertyValue::ObjectRefList(refs)) => refs
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
                Some(_) => {
                    diagnostics.push(Diagnostic::error(
                        p.node().byte_range(),
                        "not an actions list",
                    ));
                    vec![]
                }
                None => vec![],
            }
        } else {
            collect_action_like_children(&children)
        };

        let items = if class.is_derived_from(&ctx.classes.combo_box)
            || class.is_derived_from(&ctx.classes.list_widget)
        {
            if let Some(p) = properties_code_map.get("model") {
                match PropertyValue::build(&ctx.make_object_context(), p, diagnostics) {
                    Some(PropertyValue::ItemModel(items)) => items,
                    Some(_) => {
                        diagnostics.push(Diagnostic::error(
                            p.node().byte_range(),
                            "not an item model",
                        ));
                        vec![]
                    }
                    None => vec![],
                }
            } else {
                vec![]
            }
        } else {
            vec![]
        };

        let mut attributes = HashMap::new();
        if class.is_derived_from(&ctx.classes.table_view) {
            pseudo_property_names.extend(["horizontalHeader", "verticalHeader"]);
            flatten_object_properties_into_attributes(
                ctx,
                &mut attributes,
                properties_code_map,
                "horizontalHeader",
                diagnostics,
            );
            flatten_object_properties_into_attributes(
                ctx,
                &mut attributes,
                properties_code_map,
                "verticalHeader",
                diagnostics,
            );
        }
        if class.is_derived_from(&ctx.classes.tree_view) {
            pseudo_property_names.extend(["header"]);
            flatten_object_properties_into_attributes(
                ctx,
                &mut attributes,
                properties_code_map,
                "header",
                diagnostics,
            );
        }

        let mut properties = property::make_serializable_map(
            &ctx.make_object_context(),
            properties_code_map,
            &pseudo_property_names,
            diagnostics,
        );
        if class.is_derived_from(&ctx.classes.push_button) {
            // see metatype_tweak.rs, "default" is a reserved word
            if let Some((mut k, (v, _))) = properties.remove_entry("default_") {
                k.pop();
                properties.insert(k, (v, PropertySetter::StdSet));
            }
        }

        Widget {
            class: class.qualified_cxx_name().into_owned(),
            name: name.into(),
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
        let tag = BytesStart::borrowed_name(b"widget")
            .with_attributes([("class", self.class.as_ref()), ("name", self.name.as_ref())]);
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
                                &ctx.make_object_context(),
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

fn collect_action_like_children(children: &[UiObject]) -> Vec<String> {
    children
        .iter()
        .filter_map(|child| match child {
            UiObject::Action(a) => Some(a.name.clone()),
            UiObject::ActionSeparator => Some(ACTION_SEPARATOR_NAME.to_owned()),
            UiObject::Menu(w) => Some(w.name.clone()),
            UiObject::Layout(_) | UiObject::Widget(_) => None,
        })
        .collect()
}

fn flatten_object_properties_into_attributes(
    ctx: &BuildDocContext,
    attributes: &mut HashMap<String, (Value, PropertySetter)>,
    properties_code_map: &HashMap<&str, PropertyCode>,
    name: &str,
    diagnostics: &mut Diagnostics,
) {
    if let Some(p) = properties_code_map.get(name) {
        match PropertyValue::build(&ctx.make_object_context(), p, diagnostics) {
            Some(PropertyValue::ObjectProperties(props)) => {
                attributes.extend(props.into_iter().filter_map(|(k, v)| {
                    diagnostics
                        .consume_err(v.into_serializable_setter())
                        .map(|x| (concat_camel_case_names(name, &k), x))
                }));
            }
            Some(_) => {
                diagnostics.push(Diagnostic::error(
                    p.node().byte_range(),
                    "not a properties map",
                ));
            }
            None => {}
        }
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
