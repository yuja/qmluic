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
}
