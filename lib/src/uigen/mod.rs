//! Qt user interface XML (.ui) generator.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectTree;
use crate::qmlast::{UiImportSource, UiProgram};
use crate::qmldir;
use crate::qmldoc::UiDocument;
use crate::typemap::{Class, Enum, ImportedModuleSpace, ModuleId, NamedType, TypeMap, TypeSpace};
use itertools::Itertools as _;
use std::cell::RefCell;
use std::collections::HashMap;
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
    let object_tree = ObjectTree::build(
        program.root_object_node(),
        doc.source(),
        &type_space,
        diagnostics,
    )?;
    let ctx = BuildDocContext::new(doc, type_space, object_tree, base_ctx);
    let root_object = UiObject::from_object_node(
        &ctx,
        ctx.object_tree.root(),
        ContainerKind::Any,
        diagnostics,
    )?;
    let custom_widgets = ctx
        .object_tree
        .flat_iter()
        .filter_map(|n| n.is_custom_type().then(|| n.class().clone()))
        .unique()
        .filter_map(|cls| CustomWidget::from_class(&cls, &base_ctx.file_name_rules))
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
    classes: KnownClasses<'a>,
}

#[derive(Clone, Debug)]
struct BuildDocContext<'a, 't, 's> {
    source: &'s str,
    classes: &'s KnownClasses<'a>,
    type_space: ImportedModuleSpace<'a>,
    object_tree: ObjectTree<'a, 't>,
    object_id_generator: RefCell<ObjectIdGenerator>,
}

/// Classes to be used to switch uigen paths.
#[derive(Clone, Debug)]
struct KnownClasses<'a> {
    action: Class<'a>,
    form_layout: Class<'a>,
    grid_layout: Class<'a>,
    hbox_layout: Class<'a>,
    key_sequence: Class<'a>,
    key_sequence_standard_key: Enum<'a>,
    layout: Class<'a>,
    layout_attached: Class<'a>,
    menu: Class<'a>,
    push_button: Class<'a>,
    spacer_item: Class<'a>,
    tab_widget: Class<'a>,
    tab_widget_attached: Class<'a>,
    table_view: Class<'a>,
    tree_view: Class<'a>,
    vbox_layout: Class<'a>,
    widget: Class<'a>,
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
            if let Some(NamedType::Class(cls)) = module.get_type(name) {
                Ok(cls)
            } else {
                Err(BuildContextError::ClassNotFound(name))
            }
        };
        let get_enum = |scoped_name| {
            if let Some(NamedType::Enum(en)) = module.get_type_scoped(scoped_name) {
                Ok(en)
            } else {
                // not a "class", but who cares
                Err(BuildContextError::ClassNotFound(scoped_name))
            }
        };
        let classes = KnownClasses {
            action: get_class("QAction")?,
            form_layout: get_class("QFormLayout")?,
            grid_layout: get_class("QGridLayout")?,
            hbox_layout: get_class("QHBoxLayout")?,
            key_sequence: get_class("QKeySequence")?,
            key_sequence_standard_key: get_enum("QKeySequence::StandardKey")?,
            layout: get_class("QLayout")?,
            layout_attached: get_class("QLayoutAttached")?,
            menu: get_class("QMenu")?,
            push_button: get_class("QPushButton")?,
            spacer_item: get_class("QSpacerItem")?,
            tab_widget: get_class("QTabWidget")?,
            tab_widget_attached: get_class("QTabWidgetAttached")?,
            table_view: get_class("QTableView")?,
            tree_view: get_class("QTreeView")?,
            vbox_layout: get_class("QVBoxLayout")?,
            widget: get_class("QWidget")?,
        };
        Ok(BuildContext {
            type_map,
            file_name_rules,
            classes,
        })
    }
}

impl<'a, 't, 's> BuildDocContext<'a, 't, 's> {
    fn new(
        doc: &'s UiDocument,
        type_space: ImportedModuleSpace<'a>,
        object_tree: ObjectTree<'a, 't>,
        base_ctx: &'s BuildContext<'a>,
    ) -> Self {
        BuildDocContext {
            source: doc.source(),
            classes: &base_ctx.classes,
            type_space,
            object_tree,
            object_id_generator: RefCell::new(ObjectIdGenerator::default()),
        }
    }

    /// Generates unique object id.
    ///
    /// The generated object id is NOT registered to the object tree since any reference
    /// expression pointing to the generated id should be invalid.
    fn generate_object_id(&self, prefix: impl AsRef<str>) -> String {
        self.object_id_generator
            .borrow_mut()
            .generate(prefix, &self.object_tree)
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

#[derive(Clone, Debug, Default)]
struct ObjectIdGenerator {
    used_prefixes: HashMap<String, usize>, // prefix: next count
}

impl ObjectIdGenerator {
    fn generate(&mut self, prefix: impl AsRef<str>, object_tree: &ObjectTree) -> String {
        let prefix = prefix.as_ref();
        let count = self.used_prefixes.entry(prefix.to_owned()).or_insert(0);
        let (n, id) = (*count..=*count + object_tree.flat_len())
            .find_map(|n| {
                let id = format!("{prefix}_{n}");
                if object_tree.contains_id(&id) {
                    None
                } else {
                    Some((n, id))
                }
            })
            .expect("unused id must be found within N+1 tries");
        *count = n + 1;
        id
    }
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
