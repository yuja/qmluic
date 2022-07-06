use super::enum_::Enum;
use super::{NamedType, ParentSpace};
use itertools::Itertools as _;
use std::borrow::Cow;
use std::iter::FusedIterator;
use thiserror::Error;

/// Interface to look up type by name.
pub trait TypeSpace<'a> {
    /// Name of this type.
    fn name(&self) -> &str;

    /// Scoped C++ name of this type from the root namespace.
    ///
    /// This is designed for diagnostics, but can be embedded in the generated .ui file.
    fn qualified_cxx_name(&self) -> Cow<'_, str> {
        make_qualified_name(self, "::")
    }

    /// Looks up type by name.
    ///
    /// If this type is a class, the returned type may be inherited from one of the super classes.
    fn get_type(&self, name: &str) -> Option<NamedType<'a>>;

    /// Looks up type by scoped name.
    fn get_type_scoped(&self, scoped_name: &str) -> Option<NamedType<'a>> {
        // no support for the ::<top-level> notation
        let mut parts = scoped_name.split("::");
        let head = self.get_type(parts.next().expect("split should have at least one name"));
        parts.fold(head, |outer, name| outer.and_then(|ty| ty.get_type(name)))
    }

    /// Parent space of this type.
    ///
    /// This is unrelated to the super type of the class inheritance.
    #[doc(hidden)]
    fn lexical_parent(&self) -> Option<&ParentSpace<'a>>;

    /// Looks up type by name from this towards the parent type space.
    fn resolve_type(&self, name: &str) -> Option<NamedType<'a>> {
        self.get_type(name)
            .or_else(|| LexicalAncestorSpaces::new(self).find_map(|ty| ty.get_type(name)))
    }

    /// Looks up type by scoped name from this towards the parent type space.
    fn resolve_type_scoped(&self, scoped_name: &str) -> Option<NamedType<'a>> {
        // no support for the ::<top-level> notation
        let mut parts = scoped_name.split("::");
        let head = self.resolve_type(parts.next().expect("split should have at least one name"));
        // "resolve" only the first node. the remainder should be direct child.
        parts.fold(head, |outer, name| outer.and_then(|ty| ty.get_type(name)))
    }

    /// Looks up enum type by variant name.
    ///
    /// The returned type is not an abstract [`NamedType`] since Qt metatype does not support
    /// an arbitrary static constant.
    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>>;
}

/// Iterator to walk up the type tree.
///
/// This is basically a [`std::iter::successors()`] iterator with slightly different lifetime
/// requirement.
#[derive(Clone, Debug)]
struct LexicalAncestorSpaces<'a, 'b> {
    next_ty: Option<&'b ParentSpace<'a>>,
}

impl<'a, 'b> LexicalAncestorSpaces<'a, 'b> {
    fn new<T>(ty: &'b T) -> Self
    where
        T: TypeSpace<'a> + ?Sized,
    {
        LexicalAncestorSpaces {
            next_ty: ty.lexical_parent(),
        }
    }
}

impl<'a, 'b> Iterator for LexicalAncestorSpaces<'a, 'b> {
    type Item = &'b ParentSpace<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let ty = self.next_ty;
        self.next_ty = ty.and_then(|t| t.lexical_parent());
        ty
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.next_ty.is_some() {
            (1, None)
        } else {
            (0, Some(0))
        }
    }
}

impl<'a, 'b> FusedIterator for LexicalAncestorSpaces<'a, 'b> {}

/// Error denoting inconsistency in the [`TypeMap`](super::TypeMap).
#[derive(Clone, Debug, Error)]
pub enum TypeMapError {
    #[error("invalid type reference '{0}'")]
    InvalidTypeRef(String),
    #[error("invalid type '{0}' as a super class")]
    InvalidSuperClassType(String),
    #[error("unsupported type decoration in '{0}'")]
    UnsupportedDecoration(String),
}

/// Builds a qualified type name by walking up the type tree.
fn make_qualified_name<'a, 'b, T>(ty: &'b T, sep: &str) -> Cow<'b, str>
where
    T: TypeSpace<'a> + ?Sized,
{
    let mut names = collect_ancestor_names(LexicalAncestorSpaces::new(ty));
    if names.is_empty() {
        Cow::Borrowed(ty.name())
    } else {
        names.push(ty.name());
        Cow::Owned(names.join(sep))
    }
}

fn collect_ancestor_names<'b>(ancestors: LexicalAncestorSpaces<'_, 'b>) -> Vec<&'b str> {
    let mut names = Vec::new();
    for (ty, p) in ancestors.tuple_windows() {
        match (ty, p) {
            (ParentSpace::Namespace(_), ParentSpace::ImportedModuleSpace(_)) => {
                break; // root namespace
            }
            _ => {
                names.push(ty.name());
            }
        }
    }
    names.reverse();
    names
}
