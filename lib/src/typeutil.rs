use crate::typedexpr::TypeDesc;
use crate::typemap::{Enum, NamedType, TypeKind, TypeMapError, TypeSpace as _};
use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum TypeError {
    #[error("type resolution failed: {0}")]
    TypeResolution(#[from] TypeMapError),
    #[error("incompatible types: {0} and {1}")]
    IncompatibleTypes(String, String),
    #[error("undetermined type: {0}")]
    UndeterminedType(String),
    #[error("unsupported type: {0}")]
    UnsupportedType(String),
}

fn is_compatible_enum(left: &Enum, right: &Enum) -> Result<bool, TypeMapError> {
    Ok(left == right
        || left
            .alias_enum()
            .transpose()?
            .map_or(false, |en| &en == right)
        || right
            .alias_enum()
            .transpose()?
            .map_or(false, |en| &en == left))
}

pub fn deduce_concrete_type<'a>(
    left: TypeDesc<'a>,
    right: TypeDesc<'a>,
) -> Result<TypeKind<'a>, TypeError> {
    deduce_type(left, right).and_then(to_concrete_type)
}

pub fn to_concrete_type(t: TypeDesc) -> Result<TypeKind, TypeError> {
    match t {
        TypeDesc::Concrete(ty) => Ok(ty),
        TypeDesc::ConstInteger => Ok(TypeKind::INT), // fallback to default
        TypeDesc::ConstString => Ok(TypeKind::STRING),
        t @ TypeDesc::EmptyList => Err(TypeError::UndeterminedType(t.qualified_name().into())),
    }
}

pub fn to_concrete_list_type(elem_t: TypeDesc) -> Result<TypeKind, TypeError> {
    match to_concrete_type(elem_t)? {
        TypeKind::STRING => Ok(TypeKind::STRING_LIST),
        TypeKind::Pointer(t) => Ok(TypeKind::PointerList(t)),
        t => Err(TypeError::UnsupportedType(t.qualified_cxx_name().into())),
    }
}

pub fn deduce_type<'a>(left: TypeDesc<'a>, right: TypeDesc<'a>) -> Result<TypeDesc<'a>, TypeError> {
    match (left, right) {
        (left, right) if left == right => Ok(left),
        (l @ (TypeDesc::INT | TypeDesc::UINT), TypeDesc::ConstInteger) => Ok(l),
        (TypeDesc::ConstInteger, r @ (TypeDesc::INT | TypeDesc::UINT)) => Ok(r),
        (l @ TypeDesc::STRING, TypeDesc::ConstString) => Ok(l),
        (TypeDesc::ConstString, r @ TypeDesc::STRING) => Ok(r),
        (
            TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(l))),
            TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(r))),
        ) if is_compatible_enum(&l, &r)? => {
            Ok(TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(l))))
        }
        (
            TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(l))),
            TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(r))),
        ) => l
            .common_base_class(&r)
            .transpose()?
            .map(|c| TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(c))))
            .ok_or_else(|| {
                TypeError::IncompatibleTypes(
                    l.qualified_cxx_name().into(),
                    r.qualified_cxx_name().into(),
                )
            }),
        (
            l @ (TypeDesc::STRING_LIST | TypeDesc::Concrete(TypeKind::PointerList(_))),
            TypeDesc::EmptyList,
        ) => Ok(l),
        (
            TypeDesc::EmptyList,
            r @ (TypeDesc::STRING_LIST | TypeDesc::Concrete(TypeKind::PointerList(_))),
        ) => Ok(r),
        (left, right) => Err(TypeError::IncompatibleTypes(
            left.qualified_name().into(),
            right.qualified_name().into(),
        )),
    }
}

/// Method of type conversion.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TypeCastKind {
    /// Exactly the Same type. No type conversion needed.
    Noop,
    /// Not the same type, but compatible. No explicit type conversion needed.
    Implicit,
    /// Use `static_cast<T>()`.
    Static,
    /// No valid type casting available.
    Invalid,
}

/// Determine the method to convert the `actual` to the variable of the `expected` type.
pub fn pick_type_cast(
    expected: &TypeKind,
    actual: &TypeDesc,
) -> Result<TypeCastKind, TypeMapError> {
    match (expected, actual) {
        (expected, TypeDesc::Concrete(ty)) => pick_concrete_type_cast(expected, ty),
        (&TypeKind::INT | &TypeKind::UINT, TypeDesc::ConstInteger) => Ok(TypeCastKind::Implicit),
        (&TypeKind::DOUBLE, TypeDesc::ConstInteger) => Ok(TypeCastKind::Static),
        // TODO: should we allow bool <- integer cast?
        (&TypeKind::STRING, TypeDesc::ConstString) => Ok(TypeCastKind::Implicit),
        (&TypeKind::STRING_LIST | TypeKind::PointerList(_), TypeDesc::EmptyList) => {
            Ok(TypeCastKind::Implicit)
        }
        (&TypeKind::VOID, _) => Ok(TypeCastKind::Static),
        _ => Ok(TypeCastKind::Invalid),
    }
}

/// Determine the method to convert the concrete `actual` to the variable of the `expected` type.
pub fn pick_concrete_type_cast(
    expected: &TypeKind,
    actual: &TypeKind,
) -> Result<TypeCastKind, TypeMapError> {
    match (expected, actual) {
        (expected, actual) if expected == actual => Ok(TypeCastKind::Noop),
        (TypeKind::Just(NamedType::Enum(e)), TypeKind::Just(NamedType::Enum(a)))
            if is_compatible_enum(e, a)? =>
        {
            Ok(TypeCastKind::Implicit)
        }
        (TypeKind::Pointer(NamedType::Class(e)), TypeKind::Pointer(NamedType::Class(a)))
            if a.is_derived_from(e) =>
        {
            Ok(TypeCastKind::Implicit)
        }
        // TODO: covariant type is allowed to support QList<QAction*|QMenu*>, but it
        // should only work at list construction.
        (
            TypeKind::PointerList(NamedType::Class(e)),
            TypeKind::PointerList(NamedType::Class(a)),
        ) if a.is_derived_from(e) => Ok(TypeCastKind::Implicit),
        (
            &TypeKind::DOUBLE | &TypeKind::INT | &TypeKind::UINT,
            &TypeKind::DOUBLE | &TypeKind::INT | &TypeKind::UINT,
        ) => Ok(TypeCastKind::Static),
        (&TypeKind::INT | &TypeKind::UINT, TypeKind::Just(NamedType::Enum(_))) => {
            Ok(TypeCastKind::Static)
        }
        (&TypeKind::INT | &TypeKind::UINT, &TypeKind::BOOL) => Ok(TypeCastKind::Static),
        // TODO: should we allow bool <- integer cast?
        (&TypeKind::VOID, _) => Ok(TypeCastKind::Static),
        _ => Ok(TypeCastKind::Invalid),
    }
}

/// Checks if the `actual` can be assigned to the variable of the `expected` type.
pub fn is_assignable(expected: &TypeKind, actual: &TypeDesc) -> Result<bool, TypeMapError> {
    pick_type_cast(expected, actual)
        .map(|k| matches!(k, TypeCastKind::Noop | TypeCastKind::Implicit))
}

/// Checks if the `actual` can be assigned to the variable of the `expected` type.
pub fn is_concrete_assignable(
    expected: &TypeKind,
    actual: &TypeKind,
) -> Result<bool, TypeMapError> {
    pick_concrete_type_cast(expected, actual)
        .map(|k| matches!(k, TypeCastKind::Noop | TypeCastKind::Implicit))
}
