use super::core::{TypeMapError, TypeSpace};
use super::enum_::Enum;
use super::namespace::{Namespace, NamespaceData};
use super::util::{TypeDataRef, TypeMapRef};
use super::QmlComponentData;
use super::{NamedType, ParentSpace, TypeMap};
use crate::metatype;
use camino::{Utf8Path, Utf8PathBuf};

/// Top-level module identifier.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ModuleId<'s> {
    /// Built-in types.
    Builtins,
    /// QML module which can be imported by (dotted) name.
    Named(&'s str),
    /// Directory containing QML files, which can be imported by string.
    ///
    /// The path must be normalized in a certain form. Typically it is an absolute path.
    Directory(&'s Utf8Path),
}

/// Owned `ModuleId`.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ModuleIdBuf {
    Builtins,
    Named(String),
    Directory(Utf8PathBuf),
}

impl ModuleIdBuf {
    pub fn as_ref(&self) -> ModuleId {
        match self {
            ModuleIdBuf::Builtins => ModuleId::Builtins,
            ModuleIdBuf::Named(name) => ModuleId::Named(name),
            ModuleIdBuf::Directory(path) => ModuleId::Directory(path),
        }
    }
}

impl From<ModuleId<'_>> for ModuleIdBuf {
    fn from(id: ModuleId<'_>) -> Self {
        match id {
            ModuleId::Builtins => ModuleIdBuf::Builtins,
            ModuleId::Named(name) => ModuleIdBuf::Named(name.to_owned()),
            ModuleId::Directory(path) => ModuleIdBuf::Directory(path.to_owned()),
        }
    }
}

/// Stored top-level namespace with imports list.
#[derive(Clone, Debug, Default)]
pub struct ModuleData {
    namespace: NamespaceData,
    imports: Vec<ModuleIdBuf>,
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
            imports: vec![ModuleIdBuf::Builtins],
        }
    }

    /// Adds the specified module to the import list from which type names will be resolved.
    pub fn import_module<S>(&mut self, id: S)
    where
        S: Into<ModuleIdBuf>,
    {
        self.imports.push(id.into())
    }

    /// Makes type `old_name` in the same module be also found by the given `new_name`.
    pub fn push_alias<S, T>(&mut self, new_name: S, old_name: T) -> Result<(), TypeMapError>
    where
        S: Into<String>,
        T: AsRef<str>,
    {
        self.namespace.push_alias(new_name, old_name)
    }

    pub fn push_qml_component(&mut self, data: QmlComponentData) {
        self.namespace.push_qml_component(data);
    }

    pub(super) fn to_namespace<'a>(
        self: &'a ModuleData,
        type_map: TypeMapRef<'a>,
    ) -> Namespace<'a> {
        let imported_space = ImportedModuleSpace::from_modules(&self.imports, type_map);
        Namespace::new(
            TypeDataRef(&self.namespace),
            type_map,
            ParentSpace::ImportedModuleSpace(imported_space),
        )
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
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ImportedModuleSpace<'a> {
    data_stack: Vec<Result<TypeDataRef<'a, ModuleData>, ModuleIdBuf>>,
    type_map: TypeMapRef<'a>,
}

impl<'a> ImportedModuleSpace<'a> {
    pub fn new(type_map: &'a TypeMap) -> Self {
        ImportedModuleSpace {
            data_stack: vec![],
            type_map: TypeMapRef(type_map),
        }
    }

    pub(super) fn from_modules(modules: &[ModuleIdBuf], type_map: TypeMapRef<'a>) -> Self {
        let data_stack = modules
            .iter()
            .map(|id| {
                type_map
                    .as_ref()
                    .get_module_data(id.as_ref())
                    .ok_or_else(|| id.clone())
                    .map(TypeDataRef)
            })
            .collect();
        ImportedModuleSpace {
            data_stack,
            type_map,
        }
    }

    /// Adds the specified module to the stack of the imported modules.
    #[must_use]
    pub fn import_module(&mut self, id: ModuleId) -> bool {
        if let Some(d) = self.type_map.as_ref().get_module_data(id) {
            self.data_stack.push(Ok(TypeDataRef(d)));
            true
        } else {
            false
        }
    }
}

// TODO: remove or refactor TypeSpace abstraction
impl<'a> TypeSpace<'a> for ImportedModuleSpace<'a> {
    fn name(&self) -> &str {
        ""
    }

    fn get_type(&self, name: &str) -> Option<Result<NamedType<'a>, TypeMapError>> {
        self.data_stack.iter().rev().find_map(|r| match r {
            Ok(d) => d.as_ref().namespace.get_type_with(name, self.type_map, || {
                ParentSpace::Namespace(d.as_ref().to_namespace(self.type_map))
            }),
            Err(id) => Some(Err(TypeMapError::InvalidModuleRef(id.clone()))),
        })
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        None
    }

    fn get_enum_by_variant(&self, _name: &str) -> Option<Result<Enum<'a>, TypeMapError>> {
        None // enum variant shouldn't exist in the top-level imported namespace
    }
}
