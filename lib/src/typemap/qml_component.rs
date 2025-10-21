use super::class::{Class, ClassData};
use super::module::{ImportedModuleSpace, ModuleId, ModuleIdBuf};
use super::util::{TypeDataRef, TypeMapRef};
use super::ParentSpace;

/// QML component representation.
///
/// A QML component is basically a class inside an anonymous namespace, and this type itself
/// isn't a [`TypeSpace`](super::core::TypeSpace). Use [`QmlComponent::as_class`] or
/// [`QmlComponent::into_class`] to get the class representation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct QmlComponent<'a> {
    class: Class<'a>,
}

/// Stored QML component representation.
#[derive(Clone, Debug)]
pub struct QmlComponentData {
    imports: Vec<ModuleIdBuf>,
    class: ClassData,
}

impl<'a> QmlComponent<'a> {
    pub(super) fn new(data: TypeDataRef<'a, QmlComponentData>, type_map: TypeMapRef<'a>) -> Self {
        // Inner enum will belong to the class since it is public and should be qualified with
        // the component name. OTOH, inline component will be added to the imported namespace.
        let imported_space = ImportedModuleSpace::from_modules(&data.as_ref().imports, type_map);
        let class = Class::new(
            TypeDataRef(&data.as_ref().class),
            ParentSpace::ImportedModuleSpace(imported_space),
        );
        QmlComponent { class }
    }

    /// Returns reference to the class representation of this QML component.
    pub fn as_class(&self) -> &Class<'a> {
        &self.class
    }

    /// Turns this into the underlying class representation.
    pub fn into_class(self) -> Class<'a> {
        self.class
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
            imports: vec![ModuleIdBuf::Builtins],
            class: ClassData::with_supers(name, [super_name]),
        }
    }

    pub fn imports(&self) -> impl ExactSizeIterator<Item = ModuleId<'_>> {
        self.imports.iter().map(|id| id.as_ref())
    }

    /// Adds the specified module to the import list from which type names will be resolved.
    pub fn import_module<S>(&mut self, id: S)
    where
        S: Into<ModuleIdBuf>,
    {
        self.imports.push(id.into())
    }

    pub(super) fn class_name(&self) -> &str {
        self.class.class_name()
    }
}
