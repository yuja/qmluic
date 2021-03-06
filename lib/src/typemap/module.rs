use super::core::{TypeMapError, TypeSpace};
use super::enum_::Enum;
use super::namespace::{Namespace, NamespaceData};
use super::util::{TypeDataRef, TypeMapRef};
use super::QmlComponentData;
use super::{NamedType, ParentSpace, TypeMap};
use crate::metatype;
use camino::Utf8Path;
use std::borrow::Cow;

/// Top-level module identifier.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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

impl<'s> From<&ModuleId<'s>> for ModuleId<'static> {
    fn from(id: &ModuleId<'s>) -> Self {
        match id {
            ModuleId::Builtins => ModuleId::Builtins,
            ModuleId::Named(name) => ModuleId::Named(name.clone().into_owned().into()),
            ModuleId::Directory(path) => ModuleId::Directory(path.clone().into_owned().into()),
        }
    }
}

impl<'s> AsRef<ModuleId<'s>> for ModuleId<'s> {
    fn as_ref(&self) -> &ModuleId<'s> {
        self
    }
}

/// Stored top-level namespace with imports list.
#[derive(Clone, Debug, Default)]
pub struct ModuleData {
    namespace: NamespaceData,
    imports: Vec<ModuleId<'static>>,
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

    /// Make type `scoped_name` in `module_id` also found by the given `new_name` in this
    /// module space.
    pub fn push_alias<S, T, U>(&mut self, new_name: S, module_id: T, scoped_name: U)
    where
        S: Into<String>,
        T: Into<ModuleId<'static>>,
        U: Into<String>,
    {
        self.namespace.push_alias(new_name, module_id, scoped_name);
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
    data_stack: Vec<Result<TypeDataRef<'a, ModuleData>, ModuleId<'static>>>,
    type_map: TypeMapRef<'a>,
}

impl<'a> ImportedModuleSpace<'a> {
    pub fn new(type_map: &'a TypeMap) -> Self {
        ImportedModuleSpace {
            data_stack: vec![],
            type_map: TypeMapRef(type_map),
        }
    }

    pub(super) fn from_modules<'s, I>(modules: I, type_map: TypeMapRef<'a>) -> Self
    where
        I: IntoIterator,
        I::Item: AsRef<ModuleId<'s>>,
    {
        let data_stack = modules
            .into_iter()
            .map(|id| {
                let id = id.as_ref();
                type_map
                    .as_ref()
                    .get_module_data(id)
                    .ok_or_else(|| id.into())
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
    pub fn import_module<'s, S>(&mut self, id: S) -> bool
    where
        S: AsRef<ModuleId<'s>>,
    {
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
