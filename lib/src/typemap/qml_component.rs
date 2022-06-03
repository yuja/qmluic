use super::class::{Class, ClassData};
use super::core::TypeSpace;
use super::enum_::Enum;
use super::module::{ImportedModuleSpace, ModuleId};
use super::{ParentSpace, Type, TypeMap};
use std::hash::{Hash, Hasher};
use std::ptr;

/// QML component representation.
///
/// This is basically a class inside an anonymous namespace. Since this represents
/// the namespace part, no types defined in the QML component will be returned from
/// this `TypeSpace`. Use [`QmlComponent::to_class`] to get the class part.
// TODO: maybe remove intermediate QmlComponent wrapper?
#[derive(Clone, Debug)]
pub struct QmlComponent<'a> {
    data: &'a QmlComponentData,
    type_map: &'a TypeMap,
}

/// Stored QML component representation.
#[derive(Clone, Debug)]
pub struct QmlComponentData {
    imports: Vec<ModuleId<'static>>,
    class: ClassData,
}

impl<'a> QmlComponent<'a> {
    pub fn new(data: &'a QmlComponentData, type_map: &'a TypeMap) -> Self {
        QmlComponent { data, type_map }
    }

    /// Returns the inner class representation of this QML component.
    pub fn to_class(&self) -> Class<'a> {
        // TODO: inner enum has to be qualified whereas inner class (or inline component) isn't
        let imported_space = ImportedModuleSpace::from_modules(&self.data.imports, self.type_map);
        Class::new(
            &self.data.class,
            self.type_map,
            ParentSpace::ImportedModuleSpace(imported_space),
        )
    }
}

impl<'a> PartialEq for QmlComponent<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data) && ptr::eq(self.type_map, other.type_map)
    }
}

impl<'a> Eq for QmlComponent<'a> {}

// TODO: maybe extract (data, parent, type_map) type and impl PartialEq/Eq/Hash on it.
impl<'a> Hash for QmlComponent<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(self.data, state);
        ptr::hash(self.type_map, state);
    }
}

impl<'a> TypeSpace<'a> for QmlComponent<'a> {
    fn name(&self) -> &str {
        "" // TODO
    }

    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        self.data.get_imported_type(name, self.type_map)
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        None
    }

    fn get_enum_by_variant(&self, _name: &str) -> Option<Enum<'a>> {
        None
    }
}

impl QmlComponentData {
    // TODO: redesign constructor or builder API
    pub fn with_super<S, T>(name: S, super_name: T) -> Self
    where
        S: Into<String>,
        T: Into<String>,
    {
        QmlComponentData {
            imports: vec![ModuleId::Builtins],
            class: ClassData::with_supers(name, [super_name]),
        }
    }

    pub fn imports(&self) -> &[ModuleId] {
        &self.imports
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

    pub(super) fn class_name(&self) -> &str {
        self.class.class_name()
    }
}
