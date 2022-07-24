//! Function and operator constants.

use std::fmt;

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

/// Unary operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryOp {
    Arith(UnaryArithOp),
    Bitwise(UnaryBitwiseOp),
    Logical(UnaryLogicalOp),
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Arith(op) => op.fmt(f),
            UnaryOp::Bitwise(op) => op.fmt(f),
            UnaryOp::Logical(op) => op.fmt(f),
        }
    }
}

/// Unary arithmetic operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryArithOp {
    /// `-`
    Minus,
    /// `+`
    Plus,
}

impl fmt::Display for UnaryArithOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            UnaryArithOp::Minus => "-",
            UnaryArithOp::Plus => "+",
        };
        write!(f, "{}", s)
    }
}

/// Unary bitwise operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryBitwiseOp {
    /// `~`
    Not,
}

impl fmt::Display for UnaryBitwiseOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            UnaryBitwiseOp::Not => "~",
        };
        write!(f, "{}", s)
    }
}

/// Unary logical operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryLogicalOp {
    /// `!`
    Not,
}

impl fmt::Display for UnaryLogicalOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            UnaryLogicalOp::Not => "!",
        };
        write!(f, "{}", s)
    }
}

/// Binary operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BinaryOp {
    Arith(BinaryArithOp),
    Bitwise(BinaryBitwiseOp),
    Shift(ShiftOp),
    Logical(BinaryLogicalOp),
    Comparison(ComparisonOp),
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Arith(op) => op.fmt(f),
            BinaryOp::Bitwise(op) => op.fmt(f),
            BinaryOp::Shift(op) => op.fmt(f),
            BinaryOp::Logical(op) => op.fmt(f),
            BinaryOp::Comparison(op) => op.fmt(f),
        }
    }
}

/// Binary arithmetic operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BinaryArithOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
}

impl fmt::Display for BinaryArithOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            BinaryArithOp::Add => "+",
            BinaryArithOp::Sub => "-",
            BinaryArithOp::Mul => "*",
            BinaryArithOp::Div => "/",
            BinaryArithOp::Rem => "%",
        };
        write!(f, "{}", s)
    }
}

/// Binary bitwise operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BinaryBitwiseOp {
    /// `&`
    And,
    /// `^`
    Xor,
    /// `|`
    Or,
}

impl fmt::Display for BinaryBitwiseOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            BinaryBitwiseOp::And => "&",
            BinaryBitwiseOp::Xor => "^",
            BinaryBitwiseOp::Or => "|",
        };
        write!(f, "{}", s)
    }
}

/// Shift operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ShiftOp {
    /// `>>`
    RightShift,
    /// `<<`
    LeftShift,
}

impl fmt::Display for ShiftOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            ShiftOp::RightShift => ">>",
            ShiftOp::LeftShift => "<<",
        };
        write!(f, "{}", s)
    }
}

/// Binary logical operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BinaryLogicalOp {
    /// `&&`
    And,
    /// `||`
    Or,
}

impl fmt::Display for BinaryLogicalOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            BinaryLogicalOp::And => "&&",
            BinaryLogicalOp::Or => "||",
        };
        write!(f, "{}", s)
    }
}

/// Comparison operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ComparisonOp {
    /// `==`
    Equal,
    /// `!=`
    NotEqual,
    /// `<`
    LessThan,
    /// `<=`
    LessThanEqual,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanEqual,
}

impl fmt::Display for ComparisonOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            ComparisonOp::Equal => "==",
            ComparisonOp::NotEqual => "!=",
            ComparisonOp::LessThan => "<",
            ComparisonOp::LessThanEqual => "<=",
            ComparisonOp::GreaterThan => ">",
            ComparisonOp::GreaterThanEqual => ">=",
        };
        write!(f, "{}", s)
    }
}
