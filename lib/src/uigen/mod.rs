//! Qt user interface XML (.ui) generator.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{UiImportSource, UiProgram};
use crate::qmldir;
use crate::qmldoc::UiDocument;
use crate::typemap::{Class, Module, ModuleData, ModuleId, Type, TypeMap, TypeSpace};
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
    base_ctx: &BuildContext,
    doc: &UiDocument,
    diagnostics: &mut Diagnostics,
) -> Option<UiForm> {
    let program = diagnostics.consume_err(UiProgram::from_node(doc.root_node(), doc.source()))?;
    // TODO: do not build unnamed module space if directory module available
    let module_data = make_doc_module_data(doc, &program, base_ctx.type_map, diagnostics);
    let type_space = get_doc_type(base_ctx.type_map, doc)
        .unwrap_or_else(|| Type::Module(Module::new(&module_data, base_ctx.type_map)));
    let ctx = BuildDocContext::new(doc, type_space, base_ctx);
    let (obj, cls) =
        object::resolve_object_definition(&ctx, program.root_object_node(), diagnostics)?;
    let root_object =
        UiObject::from_object_definition(&ctx, &cls, &obj, ContainerKind::Any, diagnostics)?;
    Some(UiForm {
        class: doc.type_name().map(|s| s.to_owned()),
        root_object,
    })
}

fn get_doc_type<'a>(type_map: &'a TypeMap, doc: &UiDocument) -> Option<Type<'a>> {
    if let Some(doc_path) = doc.path() {
        let base_dir =
            qmldir::normalize_path(doc_path.parent().expect("doc path should point to file"));
        type_map
            .get_module(ModuleId::Directory(base_dir.into()))
            .and_then(|m| {
                m.get_type(
                    doc.type_name()
                        .expect("doc loaded from file should have type"),
                )
            })
    } else {
        None
    }
}

fn make_doc_module_data(
    doc: &UiDocument,
    program: &UiProgram,
    type_map: &TypeMap,
    diagnostics: &mut Diagnostics,
) -> ModuleData {
    let mut module_data = ModuleData::with_builtins();
    for imp in program.imports() {
        if imp.alias().is_some() {
            diagnostics.push(Diagnostic::error(
                imp.node().byte_range(),
                "aliased import is not supported",
            ));
            continue;
        }
        // TODO: warn that version field is ignored
        // TODO: anchor diagnostic message onto imp.source() node
        let id = match imp.source() {
            UiImportSource::Identifier(x) => {
                ModuleId::Named(x.to_string(doc.source()).into_owned().into())
            }
            UiImportSource::String(x) => {
                if let Some(p) = doc.path().and_then(|p| p.parent()) {
                    ModuleId::Directory(qmldir::normalize_path(p.join(&x)).into())
                } else {
                    diagnostics.push(Diagnostic::error(
                        imp.node().byte_range(),
                        "cannot resolve directory path against inline QML document",
                    ));
                    continue;
                }
            }
        };
        if type_map.contains_module(&id) {
            module_data.import_module(id);
        } else {
            diagnostics.push(Diagnostic::error(
                imp.node().byte_range(),
                "module not found",
            ));
        }
    }
    module_data
}

/// Resources passed around the UI object constructors.
#[derive(Clone, Debug)]
pub struct BuildContext<'a> {
    type_map: &'a TypeMap,
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
}

#[derive(Clone, Debug)]
struct BuildDocContext<'a, 's> {
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
    type_space: Type<'a>,
}

impl<'a> BuildContext<'a> {
    /// Sets up context with the given `type_map`.
    pub fn prepare(type_map: &'a TypeMap) -> Result<Self, BuildContextError> {
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
            type_map,
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
        })
    }
}

impl<'a, 's> BuildDocContext<'a, 's> {
    fn new(doc: &'s UiDocument, type_space: Type<'a>, base_ctx: &BuildContext<'a>) -> Self {
        BuildDocContext {
            source: doc.source(),
            action_class: base_ctx.action_class.clone(),
            form_layout_class: base_ctx.form_layout_class.clone(),
            grid_layout_class: base_ctx.grid_layout_class.clone(),
            hbox_layout_class: base_ctx.hbox_layout_class.clone(),
            layout_class: base_ctx.layout_class.clone(),
            layout_attached_class: base_ctx.layout_attached_class.clone(),
            push_button_class: base_ctx.push_button_class.clone(),
            spacer_item_class: base_ctx.spacer_item_class.clone(),
            tab_widget_class: base_ctx.tab_widget_class.clone(),
            tab_widget_attached_class: base_ctx.tab_widget_attached_class.clone(),
            vbox_layout_class: base_ctx.vbox_layout_class.clone(),
            widget_class: base_ctx.widget_class.clone(),
            type_space,
        }
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
