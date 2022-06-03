use super::core::TypeSpace;
use super::enum_::Enum;
use super::namespace::NamespaceData;
use super::{ParentSpace, Type, TypeMap};
use crate::metatype;
use std::collections::HashMap;
use std::ptr;

/// QObject (or gadget) class representation.
#[derive(Clone, Debug)]
pub struct Class<'a> {
    data: &'a ClassData,
    type_map: &'a TypeMap,
    parent_space: Box<ParentSpace<'a>>,
}

/// Stored class representation.
#[derive(Clone, Debug)]
pub(super) struct ClassData {
    class_name: String,
    public_super_class_names: Vec<String>,
    inner_type_map: NamespaceData,
    property_map: HashMap<String, PropertyData>,
    // TODO
}

impl<'a> Class<'a> {
    pub(super) fn new(
        data: &'a ClassData,
        type_map: &'a TypeMap,
        parent_space: ParentSpace<'a>,
    ) -> Self {
        Class {
            data,
            type_map,
            parent_space: Box::new(parent_space),
        }
    }

    pub fn public_super_classes<'b>(&'b self) -> impl Iterator<Item = Class<'a>> + 'b {
        self.data.public_super_class_names.iter().filter_map(|n| {
            match self.parent_space.resolve_type_scoped(n) {
                Some(Type::Class(x)) => Some(x),
                Some(Type::QmlComponent(ns)) => Some(ns.to_class()),
                Some(Type::Enum(_) | Type::Module(_) | Type::Namespace(_) | Type::Primitive(_))
                | None => None, // TODO: error?
            }
        })
    }

    pub fn is_derived_from(&self, base: &Class) -> bool {
        if self == base {
            true
        } else {
            self.public_super_classes().any(|c| c.is_derived_from(base))
        }
    }

    /// Looks up type of the specified property.
    pub fn get_property_type(&self, name: &str) -> Option<Type<'a>> {
        // TODO: error out if type name can't be resolved?
        self.data
            .property_map
            .get(name)
            .and_then(|p| self.resolve_type_scoped(&p.type_name))
            .or_else(|| {
                self.public_super_classes()
                    .find_map(|cls| cls.get_property_type(name))
            })
    }
}

impl<'a> PartialEq for Class<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data)
            && ptr::eq(self.type_map, other.type_map)
            && self.parent_space == other.parent_space
    }
}

impl<'a> Eq for Class<'a> {}

impl<'a> TypeSpace<'a> for Class<'a> {
    fn name(&self) -> &str {
        &self.data.class_name
    }

    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        // TODO: detect cycle in super-class chain
        self.data
            .inner_type_map
            .get_type_with(name, self.type_map, || ParentSpace::Class(self.clone()))
            .or_else(|| {
                self.public_super_classes()
                    .find_map(|cls| cls.get_type(name))
            })
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        Some(&self.parent_space)
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        // TODO: detect cycle in super-class chain
        self.data
            .inner_type_map
            .get_enum_by_variant_with(name, || ParentSpace::Class(self.clone()))
            .or_else(|| {
                self.public_super_classes()
                    .find_map(|cls| cls.get_enum_by_variant(name))
            })
    }
}

impl ClassData {
    pub(super) fn with_supers<S, I>(name: S, super_names: I) -> Self
    where
        S: Into<String>,
        I: IntoIterator,
        I::Item: Into<String>,
    {
        ClassData {
            class_name: name.into(),
            public_super_class_names: super_names.into_iter().map(Into::into).collect(),
            inner_type_map: NamespaceData::default(),
            property_map: HashMap::new(),
        }
    }

    pub(super) fn from_meta(meta: metatype::Class) -> Self {
        let public_super_class_names = meta
            .super_classes
            .into_iter()
            .filter_map(|s| {
                if s.access == metatype::AccessSpecifier::Public {
                    Some(s.name)
                } else {
                    None
                }
            })
            .collect();

        let mut inner_type_map = NamespaceData::default();
        inner_type_map.extend_enums(meta.enums);

        let property_map = meta
            .properties
            .into_iter()
            .map(PropertyData::pair_from_meta)
            .collect();

        ClassData {
            class_name: meta.class_name,
            public_super_class_names,
            inner_type_map,
            property_map,
        }
    }

    pub(super) fn class_name(&self) -> &str {
        &self.class_name
    }
}

/// Stored property representation.
#[derive(Clone, Debug)]
struct PropertyData {
    type_name: String,
}

impl PropertyData {
    fn pair_from_meta(meta: metatype::Property) -> (String, Self) {
        let data = PropertyData {
            type_name: meta.r#type,
        };
        (meta.name, data)
    }
}
