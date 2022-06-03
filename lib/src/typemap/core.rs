use super::enum_::Enum;
use super::{ParentSpace, Type};
use std::borrow::Cow;

/// Interface to look up type by name.
pub trait TypeSpace<'a> {
    /// Name of this type.
    fn name(&self) -> &str;

    /// Scoped name of this type from the root.
    ///
    /// This is primarily designed for diagnostics.
    fn qualified_name(&self) -> Cow<'_, str> {
        if let Some(ty) = self.lexical_parent() {
            // ty may be the root namespace
            if ty.name().is_empty() && ty.lexical_parent().is_none() {
                Cow::Borrowed(self.name())
            } else {
                Cow::Owned(format!("{}::{}", ty.qualified_name(), self.name()))
            }
        } else {
            // primitive type can't return the root namespace as parent
            Cow::Borrowed(self.name())
        }
    }

    /// Looks up type by name.
    ///
    /// If this type is a class, the returned type may be inherited from one of the super classes.
    fn get_type(&self, name: &str) -> Option<Type<'a>>;

    /// Looks up type by scoped name.
    fn get_type_scoped(&self, scoped_name: &str) -> Option<Type<'a>> {
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
    fn resolve_type(&self, name: &str) -> Option<Type<'a>> {
        self.get_type(name)
            .or_else(|| self.lexical_parent().and_then(|ty| ty.resolve_type(name)))
    }

    /// Looks up type by scoped name from this towards the parent type space.
    fn resolve_type_scoped(&self, scoped_name: &str) -> Option<Type<'a>> {
        // no support for the ::<top-level> notation
        let mut parts = scoped_name.split("::");
        let head = self.resolve_type(parts.next().expect("split should have at least one name"));
        // "resolve" only the first node. the remainder should be direct child.
        parts.fold(head, |outer, name| outer.and_then(|ty| ty.get_type(name)))
    }

    /// Looks up enum type by variant name.
    ///
    /// The returned type is not an abstract [`Type`] since Qt metatype does not support
    /// an arbitrary static constant.
    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>>;
}
