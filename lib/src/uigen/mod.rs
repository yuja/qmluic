//! Qt user interface XML (.ui) generator.

use crate::diagnostic::Diagnostics;
use crate::qmlast::{UiDocument, UiProgram};
use crate::typemap::{Class, ModuleId, Namespace, Type, TypeMap, TypeSpace};
use thiserror::Error;

mod expr;
mod gadget;
mod layout;
mod object;
mod property;
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
    ctx: &BuildContext,
    doc: &UiDocument,
    diagnostics: &mut Diagnostics,
) -> Option<UiForm> {
    let program = diagnostics.consume_err(UiProgram::from_node(doc.root_node()))?;
    let (obj, cls) =
        object::resolve_object_definition(ctx, program.root_object_node(), diagnostics)?;
    let root_object =
        UiObject::from_object_definition(ctx, &cls, &obj, ContainerKind::Any, diagnostics)?;
    Some(UiForm {
        class: doc.type_name().map(|s| s.to_owned()),
        root_object,
    })
}

/// Resources passed around the UI object constructors.
#[derive(Clone, Debug)]
pub struct BuildContext<'a, 's> {
    // TODO: type_map: &'a TypeMap,
    source: &'s str,
    action_class: Class<'a>,
    form_layout_class: Class<'a>,
    grid_layout_class: Class<'a>,
    hbox_layout_class: Class<'a>,
    layout_class: Class<'a>,
    layout_attached_class: Class<'a>,
    push_button_class: Class<'a>,
    spacer_item_class: Class<'a>,
    tab_widget_class: Class<'a>,
    tab_widget_attached_class: Class<'a>,
    vbox_layout_class: Class<'a>,
    widget_class: Class<'a>,
    module: Namespace<'a>, // TODO: resolve by imported modules instead
}

impl<'a, 's> BuildContext<'a, 's> {
    /// Sets up context for the given `doc`.
    pub fn prepare(type_map: &'a TypeMap, doc: &'s UiDocument) -> Result<Self, BuildContextError> {
        const MODULE_NAME: &str = "qmluic.QtWidgets";
        let module = type_map
            .get_module(&ModuleId::Named(MODULE_NAME.into()))
            .ok_or(BuildContextError::ModuleNotFound(MODULE_NAME))?;
        let get_class = |name| {
            if let Some(Type::Class(cls)) = module.get_type(name) {
                Ok(cls)
            } else {
                Err(BuildContextError::ClassNotFound(name))
            }
        };
        Ok(BuildContext {
            // TODO: type_map,
            source: doc.source(),
            action_class: get_class("QAction")?,
            form_layout_class: get_class("QFormLayout")?,
            grid_layout_class: get_class("QGridLayout")?,
            hbox_layout_class: get_class("QHBoxLayout")?,
            layout_class: get_class("QLayout")?,
            layout_attached_class: get_class("QLayoutAttached")?,
            push_button_class: get_class("QPushButton")?,
            spacer_item_class: get_class("QSpacerItem")?,
            tab_widget_class: get_class("QTabWidget")?,
            tab_widget_attached_class: get_class("QTabWidgetAttached")?,
            vbox_layout_class: get_class("QVBoxLayout")?,
            widget_class: get_class("QWidget")?,
            module,
        })
    }
}

/// Error occurred while setting up [`BuildContext`].
#[derive(Clone, Debug, Error)]
pub enum BuildContextError {
    #[error("required class not found: {0}")]
    ClassNotFound(&'static str),
    #[error("required module not found: {0}")]
    ModuleNotFound(&'static str),
}
