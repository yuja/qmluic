use super::class::{Class, ClassData};
use super::module::{ImportedModuleSpace, ModuleId};
use super::util::{TypeDataRef, TypeMapRef};
use super::ParentSpace;

/// QML component representation.
///
/// This is basically a class inside an anonymous namespace. Use [`QmlComponent::to_class`]
/// to get the class representation.
// TODO: maybe remove intermediate QmlComponent wrapper?
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct QmlComponent<'a> {
    data: TypeDataRef<'a, QmlComponentData>,
    type_map: TypeMapRef<'a>,
}

/// Stored QML component representation.
#[derive(Clone, Debug)]
pub struct QmlComponentData {
    imports: Vec<ModuleId<'static>>,
    class: ClassData,
}

impl<'a> QmlComponent<'a> {
    pub(super) fn new(data: TypeDataRef<'a, QmlComponentData>, type_map: TypeMapRef<'a>) -> Self {
        QmlComponent { data, type_map }
    }

    /// Returns the inner class representation of this QML component.
    pub fn to_class(&self) -> Class<'a> {
        // TODO: inner enum has to be qualified whereas inner class (or inline component) isn't
        let imported_space =
            ImportedModuleSpace::from_modules(&self.data.as_ref().imports, self.type_map);
        Class::new(
            TypeDataRef(&self.data.as_ref().class),
            self.type_map,
            ParentSpace::ImportedModuleSpace(imported_space),
        )
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

    pub(super) fn class_name(&self) -> &str {
        self.class.class_name()
    }
}
