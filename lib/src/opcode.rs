//! Function and operator constants.

/// Builtin functions.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltinFunctionKind {
    /// `qsTr()`
    Tr,
}

/// Builtin methods.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltinMethodKind {
    /// `QString::arg()`
    Arg,
}
