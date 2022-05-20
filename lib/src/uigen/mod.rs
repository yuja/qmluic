//! Qt user interface XML (.ui) generator.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{UiDocument, UiProgram};
use crate::typemap::{Class, Type, TypeMap};

mod expr;
mod gadget;
mod layout;
mod object;
mod xmlutil;

pub use self::expr::*; // re-export
pub use self::gadget::*; // re-export
pub use self::layout::*; // re-export
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
    let (obj, cls) =
        object::resolve_object_definition(&ctx, program.root_object_node(), diagnostics)?;
    let root_object = UiObject::from_object_definition(&ctx, &cls, &obj, diagnostics)?;
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
