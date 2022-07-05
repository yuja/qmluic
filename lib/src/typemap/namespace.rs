use super::class::{Class, ClassData};
use super::core::TypeSpace;
use super::enum_::{Enum, EnumData};
use super::module::ModuleId;
use super::qml_component::{QmlComponent, QmlComponentData};
use super::util::{TypeDataRef, TypeMapRef};
use super::{NamedType, ParentSpace, PrimitiveType};
use crate::metatype;
use std::collections::HashMap;

/// Represents a type map in tree (or a namespace), which is created temporarily by borrowing
/// type maps.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Namespace<'a> {
    data: TypeDataRef<'a, NamespaceData>,
    type_map: TypeMapRef<'a>,
    parent_space: Box<ParentSpace<'a>>,
}

/// Stored type namespace.
#[derive(Clone, Debug, Default)]
pub struct NamespaceData {
    name_map: HashMap<String, TypeIndex>,
    aliases: Vec<AliasData>,
    classes: Vec<ClassData>,
    enums: Vec<EnumData>,
    enum_variant_map: HashMap<String, usize>,
    qml_components: Vec<QmlComponentData>,
}

#[derive(Clone, Debug)]
struct AliasData {
    module_id: ModuleId<'static>,
    scoped_name: String,
}

/// Index to type variant stored in [`NamespaceData`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TypeIndex {
    Alias(usize),
    Class(usize),
    Enum(usize),
    Primitive(PrimitiveType),
    QmlComponent(usize),
}

impl<'a> Namespace<'a> {
    // TODO: map C++ namespace to this type
    pub(super) fn new(
        data: TypeDataRef<'a, NamespaceData>,
        type_map: TypeMapRef<'a>,
        parent_space: ParentSpace<'a>,
    ) -> Self {
        Namespace {
            data,
            type_map,
            parent_space: Box::new(parent_space),
        }
    }
}

impl<'a> TypeSpace<'a> for Namespace<'a> {
    fn name(&self) -> &str {
        "" // TODO: return name if not root namespace
    }

    fn get_type(&self, name: &str) -> Option<NamedType<'a>> {
        self.data
            .as_ref()
            .get_type_with(name, self.type_map, || ParentSpace::Namespace(self.clone()))
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        Some(&self.parent_space)
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        self.data
            .as_ref()
            .get_enum_by_variant_with(name, || ParentSpace::Namespace(self.clone()))
    }
}

impl NamespaceData {
    // TODO: redesign mutation API

    pub(super) fn with_primitive_types(types: &[PrimitiveType]) -> Self {
        let name_map = HashMap::from_iter(
            types
                .iter()
                .map(|&t| (t.name().to_owned(), TypeIndex::Primitive(t))),
        );
        NamespaceData {
            name_map,
            ..Default::default()
        }
    }

    pub(super) fn extend_classes<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = metatype::Class>,
    {
        let start = self.classes.len();
        for (i, meta) in iter.into_iter().enumerate() {
            // TODO: use qualified type name and insert namespace node accordingly
            let name = meta.class_name.clone();
            self.classes.push(ClassData::from_meta(meta));
            self.name_map.insert(name, TypeIndex::Class(i + start));
        }
    }

    pub(super) fn extend_enums<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = metatype::Enum>,
    {
        let start = self.enums.len();
        for (i, meta) in iter.into_iter().enumerate() {
            let name = meta.name.clone();
            let data = EnumData::from_meta(meta);
            let index = i + start;
            self.name_map.insert(name, TypeIndex::Enum(index));
            if let Some(variants) = data.unscoped_variants() {
                self.enum_variant_map
                    .extend(variants.iter().map(|v| (v.to_owned(), index)));
            }
            self.enums.push(data);
        }
    }

    pub(super) fn push_alias<S, T, U>(&mut self, new_name: S, module_id: T, scoped_name: U)
    where
        S: Into<String>,
        T: Into<ModuleId<'static>>,
        U: Into<String>,
    {
        let start = self.aliases.len();
        self.aliases.push(AliasData {
            module_id: module_id.into(),
            scoped_name: scoped_name.into(),
        });
        self.name_map
            .insert(new_name.into(), TypeIndex::Alias(start));
    }

    pub(super) fn push_qml_component(&mut self, data: QmlComponentData) {
        let start = self.qml_components.len();
        let name = data.class_name().to_owned();
        self.qml_components.push(data);
        self.name_map.insert(name, TypeIndex::QmlComponent(start));
    }

    pub(super) fn get_type_with<'a, F>(
        &'a self,
        name: &str,
        type_map: TypeMapRef<'a>,
        make_parent_space: F,
    ) -> Option<NamedType<'a>>
    where
        F: FnOnce() -> ParentSpace<'a>,
    {
        self.name_map.get(name).and_then(|&index| match index {
            TypeIndex::Alias(i) => {
                // TODO: error if aliased type not found
                let a = &self.aliases[i];
                type_map
                    .as_ref()
                    .get_module(&a.module_id)
                    .and_then(|ns| ns.get_type_scoped(&a.scoped_name))
            }
            TypeIndex::Class(i) => Some(NamedType::Class(Class::new(
                TypeDataRef(&self.classes[i]),
                type_map,
                make_parent_space(),
            ))),
            TypeIndex::Enum(i) => Some(NamedType::Enum(Enum::new(
                TypeDataRef(&self.enums[i]),
                make_parent_space(),
            ))),
            TypeIndex::Primitive(t) => Some(NamedType::Primitive(t)),
            TypeIndex::QmlComponent(i) => Some(NamedType::QmlComponent(QmlComponent::new(
                TypeDataRef(&self.qml_components[i]),
                type_map,
            ))),
        })
    }

    pub(super) fn get_enum_by_variant_with<'a, F>(
        &'a self,
        name: &str,
        make_parent_space: F,
    ) -> Option<Enum<'a>>
    where
        F: FnOnce() -> ParentSpace<'a>,
    {
        self.enum_variant_map
            .get(name)
            .map(|&i| Enum::new(TypeDataRef(&self.enums[i]), make_parent_space()))
    }
}
