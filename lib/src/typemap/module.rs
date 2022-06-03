use super::core::TypeSpace;
use super::enum_::Enum;
use super::namespace::NamespaceData;
use super::QmlComponentData;
use super::{ParentSpace, Type, TypeMap};
use crate::metatype;
use camino::Utf8Path;
use std::borrow::Cow;
use std::ptr;

/// Top-level module identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ModuleId<'s> {
    /// Built-in types.
    Builtins,
    /// QML module which can be imported by (dotted) name.
    Named(Cow<'s, str>),
    /// Directory containing QML files, which can be imported by string.
    ///
    /// The path must be normalized in a certain form. Typically it is an absolute path.
    Directory(Cow<'s, Utf8Path>),
}

impl<'s> AsRef<ModuleId<'s>> for ModuleId<'s> {
    fn as_ref(&self) -> &ModuleId<'s> {
        self
    }
}

/// Represents a top-level namespace with imports list.
#[derive(Clone, Debug)]
pub struct Module<'a> {
    data: &'a ModuleData,
    type_map: &'a TypeMap,
}

/// Stored top-level namespace with imports list.
#[derive(Clone, Debug, Default)]
pub struct ModuleData {
    namespace: NamespaceData,
    imports: Vec<ModuleId<'static>>,
}

impl<'a> Module<'a> {
    pub fn new(data: &'a ModuleData, type_map: &'a TypeMap) -> Self {
        Module { data, type_map }
    }
}

impl<'a> PartialEq for Module<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data) && ptr::eq(self.type_map, other.type_map)
    }
}

impl<'a> Eq for Module<'a> {}

impl<'a> TypeSpace<'a> for Module<'a> {
    fn name(&self) -> &str {
        "" // TODO
    }

    // TODO: separate function to get imported/exported (i.e. private/public) types
    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        self.data
            .namespace
            .get_type_with(name, self.type_map, || ParentSpace::Module(self.clone()))
            .or_else(|| self.data.get_imported_type(name, self.type_map))
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        None
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        self.data
            .namespace
            .get_enum_by_variant_with(name, || ParentSpace::Module(self.clone()))
    }
}

impl ModuleData {
    // TODO: redesign mutation API

    pub(super) fn with_namespace(namespace: NamespaceData) -> Self {
        ModuleData {
            namespace,
            imports: vec![],
        }
    }

    /// Creates new module storage with builtin imports.
    pub fn with_builtins() -> Self {
        ModuleData {
            namespace: NamespaceData::default(),
            imports: vec![ModuleId::Builtins],
        }
    }

    /// Adds the specified module to the import list from which type names will be resolved.
    pub fn import_module<S>(&mut self, id: S)
    where
        S: Into<ModuleId<'static>>,
    {
        self.imports.push(id.into())
    }

    fn get_imported_type<'a>(&'a self, name: &str, type_map: &'a TypeMap) -> Option<Type<'a>> {
        // TODO: detect cycle
        // TODO: module not found error?
        self.imports
            .iter()
            .find_map(|id| type_map.get_module(id).and_then(|ns| ns.get_type(name)))
    }

    pub fn push_qml_component(&mut self, data: QmlComponentData) {
        self.namespace.push_qml_component(data);
    }
}

impl Extend<metatype::Class> for ModuleData {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = metatype::Class>,
    {
        self.namespace.extend_classes(iter)
    }
}

impl Extend<metatype::Enum> for ModuleData {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = metatype::Enum>,
    {
        self.namespace.extend_enums(iter)
    }
}

/// Stack of modules which represents a type space containing imported modules.
#[derive(Clone, Debug)]
pub struct ImportedModuleSpace<'a> {
    data_stack: Vec<&'a ModuleData>,
    type_map: &'a TypeMap,
}

impl<'a> ImportedModuleSpace<'a> {
    pub fn new(type_map: &'a TypeMap) -> Self {
        ImportedModuleSpace {
            data_stack: vec![],
            type_map,
        }
    }

    pub(super) fn from_modules<'s, I>(modules: I, type_map: &'a TypeMap) -> Self
    where
        I: IntoIterator,
        I::Item: AsRef<ModuleId<'s>>,
    {
        // TODO: module not found error?
        let data_stack = modules
            .into_iter()
            .filter_map(|id| type_map.get_module_data(id))
            .collect();
        ImportedModuleSpace {
            data_stack,
            type_map,
        }
    }

    /// Adds the specified module to the stack of the imported modules.
    #[must_use]
    pub fn import_module<'s, S>(&mut self, id: S) -> bool
    where
        S: AsRef<ModuleId<'s>>,
    {
        if let Some(d) = self.type_map.get_module_data(id) {
            self.data_stack.push(d);
            true
        } else {
            false
        }
    }
}

impl<'a> PartialEq for ImportedModuleSpace<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        self.data_stack.len() == other.data_stack.len()
            && self
                .data_stack
                .iter()
                .zip(&other.data_stack)
                .all(|(&a, &b)| ptr::eq(a, b))
            && ptr::eq(self.type_map, other.type_map)
    }
}

impl<'a> Eq for ImportedModuleSpace<'a> {}

// TODO: remove or refactor TypeSpace abstraction
impl<'a> TypeSpace<'a> for ImportedModuleSpace<'a> {
    fn name(&self) -> &str {
        ""
    }

    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        self.data_stack.iter().rev().find_map(|d| {
            d.namespace.get_type_with(name, self.type_map, || {
                ParentSpace::Module(Module::new(d, self.type_map))
            })
        })
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        None
    }

    fn get_enum_by_variant(&self, _name: &str) -> Option<Enum<'a>> {
        None // enum variant shouldn't exist in the top-level imported namespace
    }
}
