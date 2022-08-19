use super::core::TypeMapError;
use super::{NamedType, TypeKind, TypeMap};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ptr;

/// Thin wrapper around reference to TypeMap data.
///
/// Two type objects should be considered equal if both borrow the identical data.
///
/// This does not implement Deref nor AsRef since the lifetime of the borrowed data
/// must be propagated in many cases.
#[derive(Clone, Copy)]
pub(super) struct TypeDataRef<'a, T: ?Sized>(pub &'a T);

impl<'a, T: ?Sized> TypeDataRef<'a, T> {
    pub fn as_ref(&self) -> &'a T {
        self.0
    }
}

impl<T: ?Sized> PartialEq for TypeDataRef<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<T: ?Sized> Eq for TypeDataRef<'_, T> {}

impl<T: ?Sized> Hash for TypeDataRef<'_, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(self.0, state);
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for TypeDataRef<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("TypeDataRef")
            .field(&format_args!("@{:p} {:?}", self.0, self.0))
            .finish()
    }
}

/// Thin wrapper around reference to TypeMap.
///
/// This does not implement Deref nor AsRef since the lifetime of the borrowed data
/// must be propagated in many cases.
#[derive(Clone, Copy)]
pub(super) struct TypeMapRef<'a>(pub &'a TypeMap);

impl<'a> TypeMapRef<'a> {
    pub fn as_ref(&self) -> &'a TypeMap {
        self.0
    }
}

impl PartialEq for TypeMapRef<'_> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl Eq for TypeMapRef<'_> {}

impl Hash for TypeMapRef<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(self.0, state);
    }
}

impl fmt::Debug for TypeMapRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // don't print the TypeMap content, which would be quite noisy
        f.debug_tuple("TypeMapRef")
            .field(&format_args!("@{:p}", self.0))
            .finish()
    }
}

/// Strips type decoration from the `name`, looks up the type, and wraps it up.
///
/// The given `name` should be normalized in Qt moc way.
pub(super) fn decorated_type<'a>(
    name: &str,
    lookup: impl FnOnce(&str) -> Option<Result<NamedType<'a>, TypeMapError>>,
) -> Result<TypeKind<'a>, TypeMapError> {
    if let Some(s) = name.strip_suffix('>') {
        if let Some(t) = s.strip_prefix("QList<") {
            Ok(TypeKind::List(Box::new(decorated_type(t, lookup)?)))
        } else if let Some(t) = s.strip_prefix("QVector<") {
            Ok(TypeKind::List(Box::new(decorated_type(t, lookup)?)))
        } else {
            Err(TypeMapError::UnsupportedDecoration(name.to_owned()))
        }
    } else if let Some(s) = name.strip_suffix('*') {
        lookup(s)
            .unwrap_or_else(|| Err(TypeMapError::InvalidTypeRef(s.to_owned())))
            .map(TypeKind::Pointer)
    } else {
        lookup(name)
            .unwrap_or_else(|| Err(TypeMapError::InvalidTypeRef(name.to_owned())))
            .map(TypeKind::Just)
    }
}
