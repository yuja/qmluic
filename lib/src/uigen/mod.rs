//! Qt user interface XML (.ui) generator.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Node, UiDocument, UiObjectDefinition, UiProgram};
use crate::typemap::{Class, Type, TypeMap, TypeSpace};

mod expr;
mod gadget;
mod object;
mod xmlutil;

pub use self::expr::*; // re-export
pub use self::gadget::*; // re-export
pub use self::object::*; // re-export

pub type XmlError = quick_xml::Error;
pub type XmlResult<T> = quick_xml::Result<T>;
pub type XmlWriter<W> = quick_xml::Writer<W>;

/// Builds `UiForm` from the given `doc`.
pub fn build(
    type_map: &TypeMap,
    doc: &UiDocument,
    diagnostics: &mut Diagnostics,
) -> Option<UiForm> {
    let mut get_class = |name| {
        if let Some(Type::Class(cls)) = type_map.get_type(name) {
            Some(cls)
        } else {
            diagnostics.push(Diagnostic::error(
                0..0, // TODO: or None?
                format!("required class cannot be resolved: {name} (missing metatypes?)"),
            ));
            None
        }
    };
    let ctx = BuildContext {
        type_map,
        source: doc.source(),
        action_class: get_class("QAction")?,
        layout_class: get_class("QLayout")?,
        spacer_item_class: get_class("QSpacerItem")?,
        widget_class: get_class("QWidget")?,
    };
    let program = diagnostics.consume_err(UiProgram::from_node(doc.root_node()))?;
    let root_object = generate_object_rec(&ctx, program.root_object_node(), diagnostics)?;
    Some(UiForm {
        class: doc.type_name().map(|s| s.to_owned()),
        root_object,
    })
}

/// Resources passed around the UI object constructors.
#[derive(Clone, Debug)]
struct BuildContext<'a, 's> {
    type_map: &'a TypeMap,
    source: &'s str,
    action_class: Class<'a>,
    layout_class: Class<'a>,
    spacer_item_class: Class<'a>,
    widget_class: Class<'a>,
}

fn resolve_object_definition<'a, 't>(
    ctx: &BuildContext<'a, '_>,
    node: Node<'t>,
    diagnostics: &mut Diagnostics,
) -> Option<(UiObjectDefinition<'t>, Class<'a>)> {
    let obj = diagnostics.consume_err(UiObjectDefinition::from_node(node, ctx.source))?;
    // TODO: resolve against imported types: Qml.Type -> Cxx::Type -> type object
    let type_name = obj.type_name().to_string(ctx.source);
    if let Some(Type::Class(cls)) = ctx.type_map.get_type(&type_name) {
        Some((obj, cls))
    } else {
        diagnostics.push(Diagnostic::error(
            obj.node().byte_range(), // TODO: on identifier node
            format!("unknown object type: {type_name}"),
        ));
        None
    }
}

fn generate_object_rec(
    ctx: &BuildContext,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<UiObject> {
    let (obj, cls) = resolve_object_definition(ctx, node, diagnostics)?;
    let mut ui_obj = UiObject::from_object_definition(&cls, &obj, ctx.source, diagnostics)?;
    match &mut ui_obj {
        UiObject::Action(_) => confine_children(&cls, &obj, diagnostics),
        UiObject::Layout(layout) => {
            layout.children.extend(
                obj.child_object_nodes()
                    .iter()
                    .filter_map(|&n| generate_layout_item_rec(ctx, n, diagnostics)),
            );
        }
        UiObject::Widget(widget) => {
            widget.children.extend(
                obj.child_object_nodes()
                    .iter()
                    .filter_map(|&n| generate_object_rec(ctx, n, diagnostics)),
            );
        }
    }
    Some(ui_obj)
}

fn generate_layout_item_rec(
    ctx: &BuildContext,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<LayoutItem> {
    let (obj, cls) = resolve_object_definition(ctx, node, diagnostics)?;
    let mut item = LayoutItem::from_object_definition(&cls, &obj, ctx.source, diagnostics)?;
    match &mut item.content {
        LayoutItemContent::Layout(layout) => {
            layout.children.extend(
                obj.child_object_nodes()
                    .iter()
                    .filter_map(|&n| generate_layout_item_rec(ctx, n, diagnostics)),
            );
        }
        LayoutItemContent::SpacerItem(_) => confine_children(&cls, &obj, diagnostics),
        LayoutItemContent::Widget(widget) => {
            widget.children.extend(
                obj.child_object_nodes()
                    .iter()
                    .filter_map(|&n| generate_object_rec(ctx, n, diagnostics)),
            );
        }
    }
    Some(item)
}

fn confine_children(cls: &Class, obj: &UiObjectDefinition, diagnostics: &mut Diagnostics) {
    if let Some(n) = obj.child_object_nodes().first() {
        // TODO: error on obj.node(), and add hint to child nodes
        diagnostics.push(Diagnostic::error(
            n.byte_range(),
            format!("'{}' should have no children", cls.qualified_name()),
        ));
    }
}
