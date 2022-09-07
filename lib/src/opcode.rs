//! Function and operator constants.

use crate::qmlast::{BinaryOperator, UnaryOperator};
use std::fmt;

/// Builtin functions.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltinFunctionKind {
    /// `Math.max()`
    Max,
    /// `Math.min()`
    Min,
    /// `qsTr()`
    Tr,
}

/// Builtin (pseudo) namespace or object hosting static functions.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltinNamespaceKind {
    Math,
}

/// Unary operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryOp {
    Arith(UnaryArithOp),
    Bitwise(UnaryBitwiseOp),
    Logical(UnaryLogicalOp),
}

impl TryFrom<UnaryOperator> for UnaryOp {
    type Error = ();

    fn try_from(operator: UnaryOperator) -> Result<Self, Self::Error> {
        use UnaryOperator::*;
        match operator {
            LogicalNot => Ok(UnaryOp::Logical(UnaryLogicalOp::Not)),
            BitwiseNot => Ok(UnaryOp::Bitwise(UnaryBitwiseOp::Not)),
            Minus => Ok(UnaryOp::Arith(UnaryArithOp::Minus)),
            Plus => Ok(UnaryOp::Arith(UnaryArithOp::Plus)),
            Typeof | Void | Delete => Err(()),
        }
    }
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

impl TryFrom<BinaryOperator> for BinaryOp {
    type Error = ();

    fn try_from(operator: BinaryOperator) -> Result<Self, Self::Error> {
        use BinaryOperator::*;
        match operator {
            LogicalAnd => Ok(BinaryOp::Logical(BinaryLogicalOp::And)),
            LogicalOr => Ok(BinaryOp::Logical(BinaryLogicalOp::Or)),
            RightShift => Ok(BinaryOp::Shift(ShiftOp::RightShift)),
            UnsignedRightShift => Err(()),
            LeftShift => Ok(BinaryOp::Shift(ShiftOp::LeftShift)),
            BitwiseAnd => Ok(BinaryOp::Bitwise(BinaryBitwiseOp::And)),
            BitwiseXor => Ok(BinaryOp::Bitwise(BinaryBitwiseOp::Xor)),
            BitwiseOr => Ok(BinaryOp::Bitwise(BinaryBitwiseOp::Or)),
            Add => Ok(BinaryOp::Arith(BinaryArithOp::Add)),
            Sub => Ok(BinaryOp::Arith(BinaryArithOp::Sub)),
            Mul => Ok(BinaryOp::Arith(BinaryArithOp::Mul)),
            Div => Ok(BinaryOp::Arith(BinaryArithOp::Div)),
            Rem => Ok(BinaryOp::Arith(BinaryArithOp::Rem)),
            Exp => Err(()),
            Equal | StrictEqual => Ok(BinaryOp::Comparison(ComparisonOp::Equal)),
            NotEqual | StrictNotEqual => Ok(BinaryOp::Comparison(ComparisonOp::NotEqual)),
            LessThan => Ok(BinaryOp::Comparison(ComparisonOp::LessThan)),
            LessThanEqual => Ok(BinaryOp::Comparison(ComparisonOp::LessThanEqual)),
            GreaterThan => Ok(BinaryOp::Comparison(ComparisonOp::GreaterThan)),
            GreaterThanEqual => Ok(BinaryOp::Comparison(ComparisonOp::GreaterThanEqual)),
            NullishCoalesce | Instanceof | In => Err(()),
        }
    }
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
