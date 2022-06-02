use super::core::TypeSpace;
use super::Type;
use crate::metatype;
use std::collections::HashSet;
use std::ptr;

/// Enum representation.
#[derive(Clone, Debug)]
pub struct Enum<'a> {
    data: &'a EnumData,
    parent_space: Box<Type<'a>>, // should be either Class or Namespace
}

/// Stored enum representation.
#[derive(Clone, Debug)]
pub(super) struct EnumData {
    name: String,
    alias: Option<String>,
    pub(super) is_class: bool,
    is_flag: bool,
    pub(super) variants: Vec<String>,
    variant_set: HashSet<String>,
}

impl<'a> Enum<'a> {
    pub(super) fn new(data: &'a EnumData, parent_space: Type<'a>) -> Self {
        Enum {
            data,
            parent_space: Box::new(parent_space),
        }
    }

    pub fn alias_enum(&self) -> Option<Enum<'a>> {
        self.data
            .alias
            .as_ref()
            .and_then(|n| match self.parent_space.resolve_type_scoped(n) {
                Some(Type::Enum(x)) => Some(x),
                _ => None, // TODO: error?
            })
    }

    pub fn is_scoped(&self) -> bool {
        self.data.is_class
    }

    pub fn is_flag(&self) -> bool {
        self.data.is_flag
    }

    pub fn variants(&self) -> impl Iterator<Item = &str> {
        self.data.variants.iter().map(String::as_str)
    }

    pub fn contains_variant(&self, name: &str) -> bool {
        self.data.variant_set.contains(name)
    }

    pub fn qualify_variant_name(&self, name: &str) -> String {
        if self.is_scoped() {
            format!("{}::{}", self.qualified_name(), name)
        } else if let Some(p) = self.lexical_parent() {
            format!("{}::{}", p.qualified_name(), name)
        } else {
            // enum should have a parent space, but if there were none, returning the variant
            // name would make sense.
            name.to_owned()
        }
    }
}

impl<'a> PartialEq for Enum<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data) && self.parent_space == other.parent_space
    }
}

impl<'a> Eq for Enum<'a> {}

impl<'a> TypeSpace<'a> for Enum<'a> {
    fn name(&self) -> &str {
        &self.data.name
    }

    fn get_type(&self, _name: &str) -> Option<Type<'a>> {
        None
    }

    fn lexical_parent(&self) -> Option<&Type<'a>> {
        Some(&self.parent_space)
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        if self.is_scoped() && self.contains_variant(name) {
            Some(self.clone())
        } else {
            None
        }
    }
}

impl EnumData {
    pub(super) fn from_meta(meta: metatype::Enum) -> Self {
        let variant_set = HashSet::from_iter(meta.values.iter().cloned());
        EnumData {
            name: meta.name,
            alias: meta.alias,
            is_class: meta.is_class,
            is_flag: meta.is_flag,
            variants: meta.values,
            variant_set,
        }
    }
}
