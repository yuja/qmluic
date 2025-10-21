use crate::diagnostic::Diagnostic;
use crate::typedexpr::TypeDesc;
use crate::typemap::{Enum, NamedType, TypeKind, TypeMapError, TypeSpace as _};
use std::ops::Range;
use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum TypeError<'a> {
    #[error("type resolution failed: {0}")]
    TypeResolution(#[from] TypeMapError),
    #[error("incompatible types: {} and {}", .0.qualified_name(), .1.qualified_name())]
    IncompatibleTypes(TypeDesc<'a>, TypeDesc<'a>),
    #[error("undetermined type: {}", .0.qualified_name())]
    UndeterminedType(TypeDesc<'a>),
}

fn is_compatible_enum(left: &Enum, right: &Enum) -> Result<bool, TypeMapError> {
    Ok(left == right
        || left
            .alias_enum()
            .transpose()?
            .is_some_and(|en| &en == right)
        || right
            .alias_enum()
            .transpose()?
            .is_some_and(|en| &en == left))
}

pub fn deduce_concrete_type<'a>(
    left: TypeDesc<'a>,
    right: TypeDesc<'a>,
) -> Result<TypeKind<'a>, TypeError<'a>> {
    deduce_type(left, right).and_then(to_concrete_type)
}

pub fn to_concrete_type(t: TypeDesc) -> Result<TypeKind, TypeError> {
    match t {
        TypeDesc::Concrete(ty) => Ok(ty),
        TypeDesc::ConstInteger => Ok(TypeKind::INT), // fallback to default
        TypeDesc::ConstString => Ok(TypeKind::STRING),
        t @ (TypeDesc::NullPointer | TypeDesc::EmptyList) => Err(TypeError::UndeterminedType(t)),
    }
}

/// Determines the compatible type from `left` and `right`.
///
/// If either type is concrete, and if it is compatible with the other concrete type,
/// the concrete type will be returned. Implicit upcast isn't allowed because it would
/// be confusing if QObject were deduced from two different types.
pub fn deduce_type<'a>(
    left: TypeDesc<'a>,
    right: TypeDesc<'a>,
) -> Result<TypeDesc<'a>, TypeError<'a>> {
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
        (l @ TypeDesc::Concrete(TypeKind::Pointer(_)), TypeDesc::NullPointer) => Ok(l),
        (TypeDesc::NullPointer, r @ TypeDesc::Concrete(TypeKind::Pointer(_))) => Ok(r),
        (l @ TypeDesc::Concrete(TypeKind::List(_)), TypeDesc::EmptyList) => Ok(l),
        (TypeDesc::EmptyList, r @ TypeDesc::Concrete(TypeKind::List(_))) => Ok(r),
        (left, right) => Err(TypeError::IncompatibleTypes(left, right)),
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
    /// Use `QVariant::value<T>()`.
    Variant,
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
        (TypeKind::Pointer(_), TypeDesc::NullPointer) => Ok(TypeCastKind::Implicit),
        (TypeKind::List(_), TypeDesc::EmptyList) => Ok(TypeCastKind::Implicit),
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
        (_, &TypeKind::VARIANT) => Ok(TypeCastKind::Variant), // TODO: check expected type
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

pub fn diagnose_bool_conversion(
    diag: &mut Diagnostic,
    byte_range: Range<usize>,
    t: &TypeDesc,
    truthy: bool,
) {
    push_type_label(diag, byte_range, t);
    let ne = if truthy { "!=" } else { "==" };
    let neg = if truthy { "!" } else { "" };
    match t {
        TypeDesc::ConstInteger | &TypeDesc::INT | &TypeDesc::UINT => {
            diag.push_note(format!("use (expr {ne} 0) to test zero"));
        }
        TypeDesc::ConstString | &TypeDesc::STRING => {
            diag.push_note(format!(r#"use ({neg}expr.isEmpty()) to test empty string"#));
        }
        TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(_))) => {
            diag.push_note(format!("use ((expr as int) {ne} 0) to test flag"));
        }
        TypeDesc::Concrete(TypeKind::Pointer(_)) => {
            diag.push_note(format!("use (expr {ne} null) to test null pointer"));
        }
        TypeDesc::Concrete(TypeKind::List(_)) => {
            diag.push_note(format!(r#"use ({neg}expr.isEmpty()) to test empty list"#));
        }
        _ => {}
    }
}

/// Adds diagnostic information about type incompatibility.
pub fn diagnose_incompatible_types(
    diag: &mut Diagnostic,
    left_byte_range: Range<usize>,
    left_t: &TypeDesc,
    right_byte_range: Range<usize>,
    right_t: &TypeDesc,
) {
    push_type_label(diag, left_byte_range, left_t);
    push_type_label(diag, right_byte_range, right_t);
    match (left_t, right_t) {
        (
            &TypeDesc::DOUBLE | &TypeDesc::INT | &TypeDesc::UINT,
            &TypeDesc::DOUBLE | &TypeDesc::INT | &TypeDesc::UINT,
        ) => {
            diag.push_note(format!(
                "use (expr as {}) or (expr as {}) for numeric cast",
                right_t.qualified_name(),
                left_t.qualified_name(),
            ));
        }
        (
            TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(l))),
            TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(r))),
        ) => {
            if matches!(
                (l.name(), r.name()),
                ("QAction", "QMenu") | ("QMenu", "QAction")
            ) {
                diag.push_note("call .menuAction() to obtain QAction* associated with menu");
            } else if let Some(Ok(b)) = l.common_base_class(r) {
                diag.push_note(format!(
                    "use (expr as {}) to upcast to base class",
                    b.name(),
                ));
            }
        }
        _ => {}
    }
}

pub fn push_type_label(diag: &mut Diagnostic, byte_range: Range<usize>, t: &TypeDesc) {
    diag.push_label(byte_range, format!("type: {}", t.qualified_name()));
}
