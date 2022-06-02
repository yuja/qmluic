use super::core::TypeSpace;
use super::namespace::NamespaceData;
use super::{Enum, QmlComponentData, Type, TypeMap};
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
    pub(super) namespace: NamespaceData,
    pub(super) imports: Vec<ModuleId<'static>>,
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
            .get_type_with(name, self.type_map, || Type::Module(self.clone()))
            .or_else(|| self.data.get_imported_type(name, self.type_map))
    }

    fn lexical_parent(&self) -> Option<&Type<'a>> {
        None
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        self.data
            .namespace
            .get_enum_by_variant_with(name, || Type::Module(self.clone()))
    }
}

impl ModuleData {
    // TODO: redesign mutation API

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
