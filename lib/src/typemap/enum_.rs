use super::core::{TypeMapError, TypeSpace};
use super::util::TypeDataRef;
use super::{NamedType, ParentSpace};
use crate::metatype;
use std::collections::HashSet;

/// Enum representation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Enum<'a> {
    data: TypeDataRef<'a, EnumData>,
    parent_space: ParentSpace<'a>,
}

/// Stored enum representation.
#[derive(Clone, Debug)]
pub(super) struct EnumData {
    name: String,
    alias: Option<String>,
    is_class: bool,
    is_flag: bool,
    variants: Vec<String>,
    variant_set: HashSet<String>,
}

impl<'a> Enum<'a> {
    pub(super) fn new(data: TypeDataRef<'a, EnumData>, parent_space: ParentSpace<'a>) -> Self {
        Enum { data, parent_space }
    }

    pub fn alias_enum(&self) -> Option<Result<Enum<'a>, TypeMapError>> {
        self.data
            .as_ref()
            .alias
            .as_ref()
            .map(|n| match self.parent_space.resolve_type_scoped(n) {
                Some(Ok(NamedType::Enum(x))) => Ok(x),
                Some(Ok(_)) => Err(TypeMapError::InvalidAliasEnumType(n.to_owned())),
                Some(Err(e)) => Err(e),
                None => Err(TypeMapError::InvalidTypeRef(n.to_owned())),
            })
    }

    pub fn is_scoped(&self) -> bool {
        self.data.as_ref().is_class
    }

    pub fn is_flag(&self) -> bool {
        self.data.as_ref().is_flag
    }

    pub fn variants(&self) -> impl Iterator<Item = &str> {
        self.data.as_ref().variants.iter().map(String::as_str)
    }

    pub fn contains_variant(&self, name: &str) -> bool {
        self.data.as_ref().variant_set.contains(name)
    }

    pub fn qualify_cxx_variant_name(&self, name: &str) -> String {
        if self.is_scoped() {
            format!("{}::{}", self.qualified_cxx_name(), name)
        } else if let Some(p) = self.lexical_parent() {
            format!("{}::{}", p.qualified_cxx_name(), name)
        } else {
            // enum should have a parent space, but if there were none, returning the variant
            // name would make sense.
            name.to_owned()
        }
    }
}

impl<'a> TypeSpace<'a> for Enum<'a> {
    fn name(&self) -> &str {
        &self.data.as_ref().name
    }

    fn get_type(&self, _name: &str) -> Option<Result<NamedType<'a>, TypeMapError>> {
        None
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        Some(&self.parent_space)
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Result<Enum<'a>, TypeMapError>> {
        if self.is_scoped() && self.contains_variant(name) {
            Some(Ok(self.clone()))
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

    pub(super) fn unscoped_variants(&self) -> Option<&[String]> {
        if self.is_class {
            None
        } else {
            Some(&self.variants)
        }
    }
}
