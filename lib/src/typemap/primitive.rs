use super::class::{Class, ClassData};
use super::namespace::{Namespace, NamespaceData};
use super::util::TypeDataRef;
use super::ParentSpace;
use crate::metatype;
use once_cell::sync::Lazy;

/// Value types provided by C++ language and Qt runtime.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum PrimitiveType {
    Bool,
    Double,
    Int,
    QString,
    QStringList,
    QVariant,
    Uint,
    Void,
}

impl PrimitiveType {
    pub(super) const ALL: [Self; 8] = [
        PrimitiveType::Bool,
        PrimitiveType::Double,
        PrimitiveType::Int,
        PrimitiveType::QString,
        PrimitiveType::QStringList,
        PrimitiveType::QVariant,
        PrimitiveType::Uint,
        PrimitiveType::Void,
    ];

    pub const fn name(&self) -> &'static str {
        match self {
            PrimitiveType::Bool => "bool",
            PrimitiveType::Double => "double",
            PrimitiveType::Int => "int",
            PrimitiveType::QString => "QString",
            PrimitiveType::QStringList => "QStringList",
            PrimitiveType::QVariant => "QVariant",
            PrimitiveType::Uint => "uint",
            PrimitiveType::Void => "void",
        }
    }

    /// Returns true if this type is supposed to be passed by `const T &`.
    pub(super) const fn is_const_ref_preferred(&self) -> bool {
        match self {
            PrimitiveType::Bool => false,
            PrimitiveType::Double => false,
            PrimitiveType::Int => false,
            PrimitiveType::QString => true,
            PrimitiveType::QStringList => true,
            PrimitiveType::QVariant => true,
            PrimitiveType::Uint => false,
            PrimitiveType::Void => false, // invalid
        }
    }

    /// Creates class representation if supported by the underlying type.
    pub fn to_class(&self) -> Option<Class<'static>> {
        match self {
            PrimitiveType::Bool => None,
            PrimitiveType::Double => None,
            PrimitiveType::Int => None,
            PrimitiveType::QString => Some(Class::new(
                TypeDataRef(&STRING_CLASS_DATA),
                make_primitive_space(),
            )),
            PrimitiveType::QStringList => None,
            PrimitiveType::QVariant => None,
            PrimitiveType::Uint => None,
            PrimitiveType::Void => None,
        }
    }
}

/// Creates a static parent space where only primitive types can be resolved.
///
/// In theory, any primitive type can have a parent module space which will be dynamically
/// created, but the const-ness of PrimitiveType is pretty useful for pattern matching.
/// Therefore, a class representation of primitive type is constrained to static space.
fn make_primitive_space() -> ParentSpace<'static> {
    ParentSpace::Namespace(Namespace::root(TypeDataRef(&PRIMITIVE_SPACE_DATA)))
}

static PRIMITIVE_SPACE_DATA: Lazy<NamespaceData> =
    Lazy::new(|| NamespaceData::with_primitive_types(&PrimitiveType::ALL));

static STRING_CLASS_DATA: Lazy<ClassData> = Lazy::new(|| {
    use PrimitiveType::*;
    let meta = metatype::Class {
        class_name: QString.name().to_owned(),
        qualified_class_name: QString.name().to_owned(),
        methods: vec![
            metatype::Method::with_argument_types("arg", QString.name(), [Double.name()]),
            metatype::Method::with_argument_types("arg", QString.name(), [Int.name()]),
            metatype::Method::with_argument_types("arg", QString.name(), [QString.name()]),
            metatype::Method::with_argument_types("arg", QString.name(), [Uint.name()]),
        ],
        ..Default::default()
    };
    ClassData::from_meta(meta)
});
