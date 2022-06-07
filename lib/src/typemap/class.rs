use super::core::TypeSpace;
use super::enum_::Enum;
use super::namespace::NamespaceData;
use super::util::{self, TypeDataRef, TypeMapRef};
use super::{NamedType, ParentSpace, TypeKind};
use crate::metatype;
use std::collections::HashMap;
use std::iter::FusedIterator;
use std::slice;

/// QObject (or gadget) class representation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Class<'a> {
    data: TypeDataRef<'a, ClassData>,
    type_map: TypeMapRef<'a>,
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
        data: TypeDataRef<'a, ClassData>,
        type_map: TypeMapRef<'a>,
        parent_space: ParentSpace<'a>,
    ) -> Self {
        Class {
            data,
            type_map,
            parent_space: Box::new(parent_space),
        }
    }

    pub fn public_super_classes<'b>(&'b self) -> SuperClasses<'a, 'b> {
        SuperClasses::new(
            self.data.as_ref().public_super_class_names.iter(),
            &self.parent_space,
        )
    }

    pub fn is_derived_from(&self, base: &Class) -> bool {
        if self == base {
            true
        } else {
            self.public_super_classes().any(|c| c.is_derived_from(base))
        }
    }

    /// Looks up type of the specified property.
    pub fn get_property_type(&self, name: &str) -> Option<TypeKind<'a>> {
        // TODO: error out if type name can't be resolved?
        self.data
            .as_ref()
            .property_map
            .get(name)
            .and_then(|p| util::decorated_type(&p.type_name, |n| self.resolve_type_scoped(n)))
            .or_else(|| {
                self.public_super_classes()
                    .find_map(|cls| cls.get_property_type(name))
            })
    }
}

impl<'a> TypeSpace<'a> for Class<'a> {
    fn name(&self) -> &str {
        &self.data.as_ref().class_name
    }

    fn get_type(&self, name: &str) -> Option<NamedType<'a>> {
        // TODO: detect cycle in super-class chain
        self.data
            .as_ref()
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
            .as_ref()
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

/// Iterator over the direct super classes.
#[derive(Clone, Debug)]
pub struct SuperClasses<'a, 'b> {
    names_iter: slice::Iter<'a, String>,
    parent_space: &'b ParentSpace<'a>,
}

impl<'a, 'b> SuperClasses<'a, 'b> {
    fn new(names_iter: slice::Iter<'a, String>, parent_space: &'b ParentSpace<'a>) -> Self {
        SuperClasses {
            names_iter,
            parent_space,
        }
    }
}

impl<'a, 'b> Iterator for SuperClasses<'a, 'b> {
    type Item = Class<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        for n in self.names_iter.by_ref() {
            match self.parent_space.resolve_type_scoped(n) {
                Some(NamedType::Class(x)) => return Some(x),
                Some(NamedType::QmlComponent(ns)) => return Some(ns.into_class()),
                Some(NamedType::Enum(_) | NamedType::Namespace(_) | NamedType::Primitive(_))
                | None => (), // TODO: error?
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.names_iter.size_hint()
    }
}

impl<'a, 'b> FusedIterator for SuperClasses<'a, 'b> where slice::Iter<'a, String>: FusedIterator {}
