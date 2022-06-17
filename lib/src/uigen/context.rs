use crate::objtree::ObjectTree;
use crate::qmldoc::UiDocument;
use crate::typemap::{Class, Enum, ImportedModuleSpace, ModuleId, NamedType, TypeMap, TypeSpace};
use std::cell::RefCell;
use std::collections::HashMap;
use thiserror::Error;

/// Resources passed around the UI object constructors.
#[derive(Clone, Debug)]
pub struct BuildContext<'a> {
    pub(super) type_map: &'a TypeMap,
    pub file_name_rules: FileNameRules,
    pub(super) classes: KnownClasses<'a>,
}

#[derive(Clone, Debug)]
pub(super) struct BuildDocContext<'a, 't, 's> {
    pub source: &'s str,
    pub classes: &'s KnownClasses<'a>,
    pub type_space: ImportedModuleSpace<'a>,
    pub object_tree: ObjectTree<'a, 't>,
    object_id_generator: RefCell<ObjectIdGenerator>,
}

/// Classes to be used to switch uigen paths.
#[derive(Clone, Debug)]
pub(super) struct KnownClasses<'a> {
    pub action: Class<'a>,
    pub action_separator: Class<'a>,
    pub brush: Class<'a>,
    pub color: Class<'a>,
    pub cursor: Class<'a>,
    pub cursor_shape: Enum<'a>,
    pub form_layout: Class<'a>,
    pub grid_layout: Class<'a>,
    pub hbox_layout: Class<'a>,
    pub key_sequence: Class<'a>,
    pub key_sequence_standard_key: Enum<'a>,
    pub layout: Class<'a>,
    pub layout_attached: Class<'a>,
    pub menu: Class<'a>,
    pub pixmap: Class<'a>,
    pub push_button: Class<'a>,
    pub spacer_item: Class<'a>,
    pub tab_widget: Class<'a>,
    pub tab_widget_attached: Class<'a>,
    pub table_view: Class<'a>,
    pub tree_view: Class<'a>,
    pub vbox_layout: Class<'a>,
    pub widget: Class<'a>,
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
            action_separator: get_class("QActionSeparator")?,
            brush: get_class("QBrush")?,
            color: get_class("QColor")?,
            cursor: get_class("QCursor")?,
            cursor_shape: get_enum("Qt::CursorShape")?,
            form_layout: get_class("QFormLayout")?,
            grid_layout: get_class("QGridLayout")?,
            hbox_layout: get_class("QHBoxLayout")?,
            key_sequence: get_class("QKeySequence")?,
            key_sequence_standard_key: get_enum("QKeySequence::StandardKey")?,
            layout: get_class("QLayout")?,
            layout_attached: get_class("QLayoutAttached")?,
            menu: get_class("QMenu")?,
            pixmap: get_class("QPixmap")?,
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
    pub fn new(
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
    pub fn generate_object_id(&self, prefix: impl AsRef<str>) -> String {
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
    pub fn generate(&mut self, prefix: impl AsRef<str>, object_tree: &ObjectTree) -> String {
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
