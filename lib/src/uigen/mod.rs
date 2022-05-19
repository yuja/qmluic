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
    let program = diagnostics.consume_err(UiProgram::from_node(doc.root_node()))?;
    let root_object = generate_object_rec(
        type_map,
        program.root_object_node(),
        doc.source(),
        diagnostics,
    )?;
    Some(UiForm {
        class: doc.type_name().map(|s| s.to_owned()),
        root_object,
    })
}

fn resolve_object_definition<'a, 't>(
    type_map: &'a TypeMap,
    node: Node<'t>,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<(UiObjectDefinition<'t>, Class<'a>)> {
    let obj = diagnostics.consume_err(UiObjectDefinition::from_node(node, source))?;
    // TODO: resolve against imported types: Qml.Type -> Cxx::Type -> type object
    let type_name = obj.type_name().to_string(source);
    if let Some(Type::Class(cls)) = type_map.get_type(&type_name) {
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
    type_map: &TypeMap,
    node: Node,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<UiObject> {
    let (obj, cls) = resolve_object_definition(type_map, node, source, diagnostics)?;
    let mut ui_obj = UiObject::from_object_definition(&cls, &obj, source, diagnostics)?;
    match &mut ui_obj {
        UiObject::Action(_) => confine_children(&cls, &obj, diagnostics),
        UiObject::Layout(layout) => {
            layout.children.extend(
                obj.child_object_nodes()
                    .iter()
                    .filter_map(|&n| generate_layout_item_rec(type_map, n, source, diagnostics)),
            );
        }
        UiObject::Widget(widget) => {
            widget.children.extend(
                obj.child_object_nodes()
                    .iter()
                    .filter_map(|&n| generate_object_rec(type_map, n, source, diagnostics)),
            );
        }
    }
    Some(ui_obj)
}

fn generate_layout_item_rec(
    type_map: &TypeMap,
    node: Node,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<LayoutItem> {
    let (obj, cls) = resolve_object_definition(type_map, node, source, diagnostics)?;
    let mut item = LayoutItem::from_object_definition(&cls, &obj, source, diagnostics)?;
    match &mut item.content {
        LayoutItemContent::Layout(layout) => {
            layout.children.extend(
                obj.child_object_nodes()
                    .iter()
                    .filter_map(|&n| generate_layout_item_rec(type_map, n, source, diagnostics)),
            );
        }
        LayoutItemContent::SpacerItem(_) => confine_children(&cls, &obj, diagnostics),
        LayoutItemContent::Widget(widget) => {
            widget.children.extend(
                obj.child_object_nodes()
                    .iter()
                    .filter_map(|&n| generate_object_rec(type_map, n, source, diagnostics)),
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
