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
        TypeDesc::ConstString => panic!("should have been converted to concrete string"),
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

/// Checks if the `actual` can be assigned to the variable of the `expected` type.
pub fn is_assignable(expected: &TypeKind, actual: &TypeDesc) -> Result<bool, TypeMapError> {
    let r = match (expected, actual) {
        (expected, TypeDesc::Concrete(ty)) if expected == ty => true,
        (&TypeKind::INT | &TypeKind::UINT, TypeDesc::ConstInteger) => true,
        (
            TypeKind::Just(NamedType::Enum(e)),
            TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(a))),
        ) if is_compatible_enum(e, a)? => true,
        (
            TypeKind::Pointer(NamedType::Class(e)),
            TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(a))),
        ) if a.is_derived_from(e) => true,
        // TODO: covariant type is allowed to support QList<QAction*|QMenu*>, but it
        // should only work at list construction.
        (
            TypeKind::PointerList(NamedType::Class(e)),
            TypeDesc::Concrete(TypeKind::PointerList(NamedType::Class(a))),
        ) if a.is_derived_from(e) => true,
        (&TypeKind::STRING_LIST | TypeKind::PointerList(_), TypeDesc::EmptyList) => true,
        _ => false,
    };
    Ok(r)
}
