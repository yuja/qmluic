use crate::objtree::ObjectTree;
use crate::qmldoc::UiDocument;
use crate::qtname::FileNameRules;
use crate::typedexpr::{BuiltinFunctionKind, RefKind, RefSpace};
use crate::typemap::{Class, Enum, ImportedModuleSpace, ModuleId, NamedType, TypeMap, TypeSpace};
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
    pub type_name: Option<&'s str>,
    pub source: &'s str,
    pub classes: &'s KnownClasses<'a>,
    pub type_space: ImportedModuleSpace<'a>,
    pub object_tree: ObjectTree<'a, 't>,
}

/// Context where expression is supposed to be evaluated.
#[derive(Clone, Debug)]
pub(super) struct ObjectContext<'a, 't, 's> {
    pub doc_type_name: Option<&'s str>,
    pub source: &'s str,
    pub classes: &'s KnownClasses<'a>,
    pub type_space: &'s ImportedModuleSpace<'a>,
    pub object_tree: &'s ObjectTree<'a, 't>,
    // TODO: Class<'a> to resolve property/method of the surrounding object?
}

/// Classes to be used to switch uigen paths.
#[derive(Clone, Debug)]
pub(super) struct KnownClasses<'a> {
    pub abstract_item_model: Class<'a>,
    pub action: Class<'a>,
    pub action_separator: Class<'a>,
    pub brush: Class<'a>,
    pub color: Class<'a>,
    pub combo_box: Class<'a>,
    pub cursor: Class<'a>,
    pub cursor_shape: Enum<'a>,
    pub font: Class<'a>,
    pub form_layout: Class<'a>,
    pub grid_layout: Class<'a>,
    pub hbox_layout: Class<'a>,
    pub icon: Class<'a>,
    pub key_sequence: Class<'a>,
    pub key_sequence_standard_key: Enum<'a>,
    pub layout: Class<'a>,
    pub layout_attached: Class<'a>,
    pub list_widget: Class<'a>,
    pub margins: Class<'a>,
    pub menu: Class<'a>,
    pub palette: Class<'a>,
    pub pixmap: Class<'a>,
    pub push_button: Class<'a>,
    pub rect: Class<'a>,
    pub size: Class<'a>,
    pub size_policy: Class<'a>,
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
            abstract_item_model: get_class("QAbstractItemModel")?,
            action: get_class("QAction")?,
            action_separator: get_class("QActionSeparator")?,
            brush: get_class("QBrush")?,
            color: get_class("QColor")?,
            combo_box: get_class("QComboBox")?,
            cursor: get_class("QCursor")?,
            cursor_shape: get_enum("Qt::CursorShape")?,
            font: get_class("QFont")?,
            form_layout: get_class("QFormLayout")?,
            grid_layout: get_class("QGridLayout")?,
            hbox_layout: get_class("QHBoxLayout")?,
            icon: get_class("QIcon")?,
            key_sequence: get_class("QKeySequence")?,
            key_sequence_standard_key: get_enum("QKeySequence::StandardKey")?,
            layout: get_class("QLayout")?,
            layout_attached: get_class("QLayoutAttached")?,
            list_widget: get_class("QListWidget")?,
            margins: get_class("QMargins")?,
            menu: get_class("QMenu")?,
            palette: get_class("QPalette")?,
            pixmap: get_class("QPixmap")?,
            push_button: get_class("QPushButton")?,
            rect: get_class("QRect")?,
            size: get_class("QSize")?,
            size_policy: get_class("QSizePolicy")?,
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
            type_name: doc.type_name(),
            source: doc.source(),
            classes: &base_ctx.classes,
            type_space,
            object_tree,
        }
    }

    pub fn make_object_context(&self) -> ObjectContext<'a, 't, '_> {
        ObjectContext {
            doc_type_name: self.type_name,
            source: self.source,
            classes: self.classes,
            type_space: &self.type_space,
            object_tree: &self.object_tree,
        }
    }
}

impl<'a> RefSpace<'a> for ObjectContext<'a, '_, '_> {
    fn get_ref(&self, name: &str) -> Option<RefKind<'a>> {
        if let Some(obj) = self.object_tree.get_by_id(name) {
            Some(RefKind::Object(obj.class().clone()))
        } else if let Some(ty) = self.type_space.get_type(name) {
            Some(RefKind::Type(ty))
        } else if name == "qsTr" {
            Some(RefKind::BuiltinFunction(BuiltinFunctionKind::Tr))
        } else {
            None
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
