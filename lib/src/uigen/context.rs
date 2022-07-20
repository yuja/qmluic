use super::objcode::ObjectCodeMap;
use crate::objtree::{ObjectNode, ObjectTree};
use crate::qmldoc::UiDocument;
use crate::qtname::FileNameRules;
use crate::typedexpr::{BuiltinFunctionKind, RefKind, RefSpace};
use crate::typemap::{
    Class, Enum, ImportedModuleSpace, ModuleId, NamedType, TypeMap, TypeMapError, TypeSpace,
};
use thiserror::Error;

/// How to process dynamic bindings.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DynamicBindingHandling {
    /// Silently drops dynamic bindings.
    Omit,
    /// Generates C++ code to set up dynamic bindings.
    Generate,
    /// Generates errors for dynamic bindings.
    Reject,
}

/// Resources passed around the UI object constructors.
#[derive(Clone, Debug)]
pub struct BuildContext<'a> {
    pub(super) type_map: &'a TypeMap,
    pub file_name_rules: FileNameRules,
    pub dynamic_binding_handling: DynamicBindingHandling,
    pub(super) classes: KnownClasses<'a>,
}

#[derive(Clone, Debug)]
pub(super) struct BuildDocContext<'a, 't, 's> {
    pub type_name: &'s str,
    pub source: &'s str,
    pub file_name_rules: &'s FileNameRules,
    pub classes: &'s KnownClasses<'a>,
    pub type_space: &'s ImportedModuleSpace<'a>,
    pub object_tree: &'s ObjectTree<'a, 't>,
    object_code_maps: &'s [ObjectCodeMap<'a, 't, 's>],
}

/// Context where expression is supposed to be evaluated.
#[derive(Clone, Debug)]
pub(super) struct ObjectContext<'a, 't, 's> {
    pub source: &'s str,
    pub classes: &'s KnownClasses<'a>,
    pub type_space: &'s ImportedModuleSpace<'a>,
    pub object_tree: &'s ObjectTree<'a, 't>,
    obj_node: ObjectNode<'a, 't, 's>,
}

/// Classes to be used to switch uigen paths.
#[derive(Clone, Debug)]
pub(super) struct KnownClasses<'a> {
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
        dynamic_binding_handling: DynamicBindingHandling,
    ) -> Result<Self, BuildContextError> {
        const MODULE_NAME: &str = "qmluic.QtWidgets";
        let module = type_map
            .get_module(&ModuleId::Named(MODULE_NAME.into()))
            .ok_or(BuildContextError::ModuleNotFound(MODULE_NAME))?;
        let get_class = |name| match module.get_type(name) {
            Some(Ok(NamedType::Class(cls))) => Ok(cls),
            Some(Err(e)) => Err(BuildContextError::TypeResolution(name, e)),
            Some(Ok(_)) | None => Err(BuildContextError::ClassNotFound(name)),
        };
        let get_enum = |scoped_name| {
            match module.get_type_scoped(scoped_name) {
                Some(Ok(NamedType::Enum(en))) => Ok(en),
                Some(Err(e)) => Err(BuildContextError::TypeResolution(scoped_name, e)),
                Some(Ok(_)) | None => {
                    // not a "class", but who cares
                    Err(BuildContextError::ClassNotFound(scoped_name))
                }
            }
        };
        let classes = KnownClasses {
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
            table_view: get_class("QTableView")?,
            tree_view: get_class("QTreeView")?,
            vbox_layout: get_class("QVBoxLayout")?,
            widget: get_class("QWidget")?,
        };
        Ok(BuildContext {
            type_map,
            file_name_rules,
            dynamic_binding_handling,
            classes,
        })
    }
}

impl<'a, 't, 's> BuildDocContext<'a, 't, 's> {
    pub fn new(
        doc: &'s UiDocument,
        type_space: &'s ImportedModuleSpace<'a>,
        object_tree: &'s ObjectTree<'a, 't>,
        object_code_maps: &'s [ObjectCodeMap<'a, 't, 's>],
        base_ctx: &'s BuildContext<'a>,
    ) -> Self {
        BuildDocContext {
            type_name: doc.type_name(),
            source: doc.source(),
            file_name_rules: &base_ctx.file_name_rules,
            classes: &base_ctx.classes,
            type_space,
            object_tree,
            object_code_maps,
        }
    }

    pub fn code_map_for_object(&self, obj_node: ObjectNode) -> &ObjectCodeMap<'a, 't, 's> {
        &self.object_code_maps[obj_node.flat_index()]
    }

    pub fn make_object_context(
        &self,
        obj_node: ObjectNode<'a, 't, 's>,
    ) -> ObjectContext<'a, 't, 's> {
        ObjectContext {
            source: self.source,
            classes: self.classes,
            type_space: self.type_space,
            object_tree: self.object_tree,
            obj_node,
        }
    }
}

impl<'a, 't, 's> ObjectContext<'a, 't, 's> {
    pub fn new(
        doc: &'s UiDocument,
        type_space: &'s ImportedModuleSpace<'a>,
        object_tree: &'s ObjectTree<'a, 't>,
        obj_node: ObjectNode<'a, 't, 's>,
        base_ctx: &'s BuildContext<'a>,
    ) -> Self {
        ObjectContext {
            source: doc.source(),
            classes: &base_ctx.classes,
            type_space,
            object_tree,
            obj_node,
        }
    }
}

impl<'a> RefSpace<'a> for ObjectContext<'a, '_, '_> {
    fn get_ref(&self, name: &str) -> Option<Result<RefKind<'a>, TypeMapError>> {
        // object id lookup is explicit, which should precede any other implicit this lookups.
        let me = &self.obj_node;
        if let Some(obj) = self.object_tree.get_by_id(name) {
            Some(Ok(RefKind::Object(obj.class().clone())))
        } else if let Some(r) = me.class().get_property(name) {
            Some(r.map(|p| RefKind::ObjectProperty(me.class().clone(), me.name().to_owned(), p)))
        } else if let Some(r) = me.class().get_public_method(name) {
            Some(r.map(|m| RefKind::ObjectMethod(me.class().clone(), me.name().to_owned(), m)))
        } else if let Some(r) = self.type_space.get_type(name) {
            Some(r.map(RefKind::Type))
        } else if name == "qsTr" {
            Some(Ok(RefKind::BuiltinFunction(BuiltinFunctionKind::Tr)))
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
    #[error("required type '{0}' not resolved: {1}")]
    TypeResolution(&'static str, #[source] TypeMapError),
}
