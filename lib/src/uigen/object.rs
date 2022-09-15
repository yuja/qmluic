use super::context::{BuildDocContext, ObjectContext};
use super::expr::{self, SerializableValue};
use super::gadget::ModelItem;
use super::layout::Layout;
use super::objcode::{PropertyCode, PropertyCodeKind};
use super::property::{self, PropertySetter};
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectNode;
use crate::qtname;
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
            if is_action_separator(ctx, obj_node, diagnostics) {
                UiObject::ActionSeparator
            } else {
                UiObject::Action(Action::new(
                    &ctx.make_object_context(obj_node),
                    obj_node.name(),
                    ctx.code_map_for_object(obj_node).properties(),
                    diagnostics,
                ))
            }
        } else if cls.is_derived_from(&ctx.classes.layout) {
            UiObject::Layout(Layout::build(ctx, obj_node, diagnostics))
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

fn is_action_separator(
    ctx: &BuildDocContext,
    obj_node: ObjectNode,
    diagnostics: &mut Diagnostics,
) -> bool {
    // Only QAction { separator: true } can be translated to <addaction name="separator"/>.
    // Others will be mapped to a QAction having dynamic "separator" property since
    // "separator" isn't actually a Q_PROPERTY.
    // TODO: can't be a static separator if object id is referenced from any other expression
    let code_map = ctx.code_map_for_object(obj_node);
    let properties_code_map = code_map.properties();
    if obj_node.class().is_derived_from(&ctx.classes.action)
        && code_map.callbacks().is_empty()
        && properties_code_map.len() == 1
        && properties_code_map.contains_key("separator")
    {
        let obj_ctx = ctx.make_object_context(obj_node);
        property::get_bool(&obj_ctx, properties_code_map, "separator", diagnostics)
            .map(|(_, b)| b)
            .unwrap_or(false)
    } else {
        false
    }
}

/// Action definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Action {
    pub name: String,
    pub properties: HashMap<String, (SerializableValue, PropertySetter)>,
}

impl Action {
    fn new(
        ctx: &ObjectContext,
        name: impl Into<String>,
        properties_code_map: &HashMap<&str, PropertyCode>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let properties = property::make_serializable_map(
            ctx,
            properties_code_map,
            &["separator"], // can't be handled by uic
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
        let tag = BytesStart::new("action").with_attributes([("name", self.name.as_ref())]);
        writer.write_event(Event::Start(tag.borrow()))?;

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
    pub attributes: HashMap<String, (SerializableValue, PropertySetter)>,
    pub properties: HashMap<String, (SerializableValue, PropertySetter)>,
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

        let properties_code_map = ctx.code_map_for_object(obj_node).properties();
        let actions = if let Some(p) = properties_code_map.get("actions") {
            if let Some(refs) = expr::build_object_ref_list(p, diagnostics) {
                refs.into_iter()
                    .map(|id| {
                        let o = ctx
                            .object_tree
                            .get_by_id(&id)
                            .expect("object ref must be valid");
                        if is_action_separator(ctx, o, diagnostics) {
                            ACTION_SEPARATOR_NAME.to_owned()
                        } else {
                            id
                        }
                    })
                    .collect()
            } else {
                vec![]
            }
        } else {
            collect_action_like_children(&children)
        };

        Self::new(
            &ctx.make_object_context(obj_node),
            obj_node.class(),
            obj_node.name(),
            properties_code_map,
            actions,
            children,
            diagnostics,
        )
    }

    pub(super) fn new(
        ctx: &ObjectContext,
        class: &Class,
        name: impl Into<String>,
        properties_code_map: &HashMap<&str, PropertyCode>,
        actions: Vec<String>,
        children: Vec<UiObject>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let mut pseudo_property_names = vec!["actions", "model"];

        let items = if class.is_derived_from(&ctx.classes.combo_box)
            || class.is_derived_from(&ctx.classes.list_widget)
        {
            if let Some(p) = properties_code_map.get("model") {
                expr::build_item_model(p, diagnostics).unwrap_or_default()
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
            ctx,
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
        let tag = BytesStart::new("widget")
            .with_attributes([("class", self.class.as_ref()), ("name", self.name.as_ref())]);
        writer.write_event(Event::Start(tag.borrow()))?;

        property::serialize_properties_to_xml(writer, "attribute", &self.attributes)?;
        property::serialize_properties_to_xml(writer, "property", &self.properties)?;

        for n in &self.actions {
            writer.write_event(Event::Empty(
                BytesStart::new("addaction").with_attributes([("name", n.as_ref())]),
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
                    let code_map = ctx.code_map_for_object(n);
                    if let Some((_, map)) = code_map.attached_properties(&ctx.classes.tab_widget) {
                        w.attributes.extend(
                            property::make_value_map(
                                &ctx.make_object_context(n),
                                map,
                                &[],
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
    ctx: &ObjectContext,
    attributes: &mut HashMap<String, (SerializableValue, PropertySetter)>,
    properties_code_map: &HashMap<&str, PropertyCode>,
    name: &str,
    diagnostics: &mut Diagnostics,
) {
    if let Some(property_code) = properties_code_map.get(name) {
        match property_code.kind() {
            PropertyCodeKind::ObjectMap(_, map) => {
                attributes.extend(
                    property::make_serializable_map(ctx, map, &[], diagnostics)
                        .into_iter()
                        .map(|(k, v)| (name.to_owned() + &qtname::to_ascii_capitalized(&k), v)),
                );
            }
            _ => {
                diagnostics.push(Diagnostic::error(
                    property_code.node().byte_range(),
                    "not a properties map",
                ));
            }
        }
    }
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
