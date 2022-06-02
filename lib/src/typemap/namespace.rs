use super::class::{Class, ClassData};
use super::core::TypeSpace;
use super::enum_::{Enum, EnumData};
use super::{PrimitiveType, QmlComponent, QmlComponentData, Type, TypeMap};
use crate::metatype;
use std::collections::HashMap;
use std::ptr;

/// Represents a type map in tree (or a namespace), which is created temporarily by borrowing
/// type maps.
#[derive(Clone, Debug)]
pub struct Namespace<'a> {
    data: &'a NamespaceData,
    type_map: &'a TypeMap,
    // TODO: parent_space
}

/// Stored type namespace.
#[derive(Clone, Debug, Default)]
pub struct NamespaceData {
    name_map: HashMap<String, TypeIndex>,
    classes: Vec<ClassData>,
    enums: Vec<EnumData>,
    enum_variant_map: HashMap<String, usize>,
    qml_components: Vec<QmlComponentData>,
}

/// Index to type variant stored in [`NamespaceData`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TypeIndex {
    Class(usize),
    Enum(usize),
    Primitive(PrimitiveType),
    QmlComponent(usize),
}

impl<'a> Namespace<'a> {
    // TODO: map C++ namespace to this type
    /*
    fn root(data: &'a NamespaceData, type_map: &'a TypeMap) -> Self {
        Namespace { data, type_map }
    }
    */
}

impl<'a> PartialEq for Namespace<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data) && ptr::eq(self.type_map, other.type_map)
        /*&& self.parent_space == other.parent_space*/
    }
}

impl<'a> Eq for Namespace<'a> {}

impl<'a> TypeSpace<'a> for Namespace<'a> {
    fn name(&self) -> &str {
        "" // TODO: return name if not root namespace
    }

    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        self.data
            .get_type_with(name, self.type_map, || Type::Namespace(self.clone()))
    }

    fn lexical_parent(&self) -> Option<&Type<'a>> {
        None // TODO: return some if not root namespace
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        self.data
            .get_enum_by_variant_with(name, || Type::Namespace(self.clone()))
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

    pub(super) fn push_qml_component(&mut self, data: QmlComponentData) {
        let start = self.qml_components.len();
        let name = data.class.class_name().to_owned();
        self.qml_components.push(data);
        self.name_map.insert(name, TypeIndex::QmlComponent(start));
    }

    pub(super) fn get_type_with<'a, F>(
        &'a self,
        name: &str,
        type_map: &'a TypeMap,
        make_parent_space: F,
    ) -> Option<Type<'a>>
    where
        F: FnOnce() -> Type<'a>,
    {
        self.name_map.get(name).map(|&index| match index {
            TypeIndex::Class(i) => {
                Type::Class(Class::new(&self.classes[i], type_map, make_parent_space()))
            }
            TypeIndex::Enum(i) => Type::Enum(Enum::new(&self.enums[i], make_parent_space())),
            TypeIndex::Primitive(t) => Type::Primitive(t),
            TypeIndex::QmlComponent(i) => {
                Type::QmlComponent(QmlComponent::new(&self.qml_components[i], type_map))
            }
        })
    }

    pub(super) fn get_enum_by_variant_with<'a, F>(
        &'a self,
        name: &str,
        make_parent_space: F,
    ) -> Option<Enum<'a>>
    where
        F: FnOnce() -> Type<'a>,
    {
        self.enum_variant_map
            .get(name)
            .map(|&i| Enum::new(&self.enums[i], make_parent_space()))
    }
}
