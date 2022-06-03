//! Qt user interface XML (.ui) generator.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{UiImportSource, UiProgram};
use crate::qmldir;
use crate::qmldoc::UiDocument;
use crate::typemap::{
    Class, ImportedModuleSpace, ModuleId, QmlComponent, Type, TypeMap, TypeSpace,
};
use itertools::Itertools as _;
use std::cell::RefCell;
use thiserror::Error;

mod expr;
mod form;
mod gadget;
mod layout;
mod object;
mod property;
mod xmlutil;

pub use self::expr::*; // re-export
pub use self::form::*; // re-export
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
    let type_space = make_doc_module_space(doc, &program, base_ctx.type_map, diagnostics);
    let ctx = BuildDocContext::new(doc, type_space, base_ctx);
    let (obj, cls) =
        object::resolve_object_definition(&ctx, program.root_object_node(), diagnostics)?;
    let root_object =
        UiObject::from_object_definition(&ctx, &cls, &obj, ContainerKind::Any, diagnostics)?;
    // assumes all QmlComponent types are user types
    let custom_widgets = ctx
        .ref_qml_components
        .borrow()
        .iter()
        .unique()
        .filter_map(|ns| CustomWidget::from_qml_component(ns, &base_ctx.file_name_rules))
        .collect();
    Some(UiForm {
        class: doc.type_name().map(|s| s.to_owned()),
        root_object,
        custom_widgets,
    })
}

fn make_doc_module_space<'a>(
    doc: &UiDocument,
    program: &UiProgram,
    type_map: &'a TypeMap,
    diagnostics: &mut Diagnostics,
) -> ImportedModuleSpace<'a> {
    let mut module_space = ImportedModuleSpace::new(type_map);
    if let Some(p) = doc.path().and_then(|p| p.parent()) {
        // QML files in the base directory should be available by default
        let id = ModuleId::Directory(qmldir::normalize_path(p).into());
        if !module_space.import_module(id) {
            diagnostics.push(Diagnostic::error(0..0, "directory module not found"));
        }
    }

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
        if !module_space.import_module(id) {
            diagnostics.push(Diagnostic::error(
                imp.node().byte_range(),
                "module not found",
            ));
        }
    }

    module_space
}

/// Resources passed around the UI object constructors.
#[derive(Clone, Debug)]
pub struct BuildContext<'a> {
    type_map: &'a TypeMap,
    pub file_name_rules: FileNameRules,
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
    type_space: ImportedModuleSpace<'a>,
    /// List of user types resolved from the top-level type space.
    ref_qml_components: RefCell<Vec<QmlComponent<'a>>>,
}

impl<'a> BuildContext<'a> {
    /// Sets up context with the given `type_map`.
    pub fn prepare(
        type_map: &'a TypeMap,
        file_name_rules: FileNameRules,
    ) -> Result<Self, BuildContextError> {
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
            file_name_rules,
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
    fn new(
        doc: &'s UiDocument,
        type_space: ImportedModuleSpace<'a>,
        base_ctx: &BuildContext<'a>,
    ) -> Self {
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
            ref_qml_components: RefCell::new(Vec::new()),
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

/// File naming rules.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FileNameRules {
    pub cxx_header_suffix: String,
    pub lowercase: bool,
}

impl FileNameRules {
    pub fn type_name_to_cxx_header_name<S>(&self, type_name: S) -> String
    where
        S: AsRef<str>,
    {
        self.apply_case_change(format!(
            "{}.{}",
            type_name.as_ref(),
            &self.cxx_header_suffix
        ))
    }

    pub fn type_name_to_ui_name<S>(&self, type_name: S) -> String
    where
        S: AsRef<str>,
    {
        self.apply_case_change(format!("{}.ui", type_name.as_ref()))
    }

    fn apply_case_change(&self, mut file_name: String) -> String {
        if self.lowercase {
            file_name.make_ascii_lowercase();
        }
        file_name
    }
}

impl Default for FileNameRules {
    fn default() -> Self {
        Self {
            cxx_header_suffix: "h".to_owned(),
            lowercase: true,
        }
    }
}
