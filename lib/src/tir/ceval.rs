//! Utility to evaluate constant expression.
//!
//! This is not for optimization. We don't want to allocate local variable without
//! concrete integer/string type. And we do want to concatenate string literals to
//! support translation.

use super::builder::ExpressionError;
use super::{
    BinaryArithOp, BinaryBitwiseOp, BinaryLogicalOp, ConstantValue, UnaryArithOp, UnaryBitwiseOp,
    UnaryLogicalOp,
};
use crate::typedexpr::DescribeType;

pub(super) fn eval_unary_arith_expression(
    op: UnaryArithOp,
    argument: ConstantValue,
) -> Result<ConstantValue, ExpressionError> {
    use UnaryArithOp::*;
    match argument {
        ConstantValue::Integer(v) => {
            let a = match op {
                Plus => Some(v),
                Minus => v.checked_neg(),
            };
            a.map(ConstantValue::Integer)
                .ok_or(ExpressionError::IntegerOverflow)
        }
        ConstantValue::Float(v) => {
            let a = match op {
                Plus => v,
                Minus => -v,
            };
            Ok(ConstantValue::Float(a))
        }
        ConstantValue::Bool(_)
        | ConstantValue::CString(_)
        | ConstantValue::QString(_)
        | ConstantValue::EmptyList => Err(ExpressionError::UnsupportedOperation(op.to_string())),
    }
}

pub(super) fn eval_unary_bitwise_expression(
    op: UnaryBitwiseOp,
    argument: ConstantValue,
) -> Result<ConstantValue, ExpressionError> {
    use UnaryBitwiseOp::*;
    match argument {
        ConstantValue::Integer(v) => {
            let a = match op {
                Not => !v,
            };
            Ok(ConstantValue::Integer(a))
        }
        ConstantValue::Bool(_)
        | ConstantValue::Float(_)
        | ConstantValue::CString(_)
        | ConstantValue::QString(_)
        | ConstantValue::EmptyList => Err(ExpressionError::UnsupportedOperation(op.to_string())),
    }
}

pub(super) fn eval_unary_logical_expression(
    op: UnaryLogicalOp,
    argument: ConstantValue,
) -> Result<ConstantValue, ExpressionError> {
    use UnaryLogicalOp::*;
    match argument {
        ConstantValue::Bool(v) => {
            let a = match op {
                Not => !v,
            };
            Ok(ConstantValue::Bool(a))
        }
        ConstantValue::Integer(_)
        | ConstantValue::Float(_)
        | ConstantValue::CString(_)
        | ConstantValue::QString(_)
        | ConstantValue::EmptyList => Err(ExpressionError::UnsupportedOperation(op.to_string())),
    }
}

pub(super) fn eval_binary_arith_expression(
    op: BinaryArithOp,
    left: ConstantValue,
    right: ConstantValue,
) -> Result<ConstantValue, ExpressionError> {
    use BinaryArithOp::*;
    match (left, right) {
        (ConstantValue::Bool(_), ConstantValue::Bool(_)) => {
            Err(ExpressionError::UnsupportedOperation(op.to_string()))
        }
        (ConstantValue::Integer(l), ConstantValue::Integer(r)) => {
            let a = match op {
                Add => i64::checked_add(l, r),
                Sub => i64::checked_sub(l, r),
                Mul => i64::checked_mul(l, r),
                Div => i64::checked_div(l, r),
                Rem => i64::checked_rem(l, r),
            };
            a.map(ConstantValue::Integer)
                .ok_or(ExpressionError::IntegerOverflow)
        }
        (ConstantValue::Float(l), ConstantValue::Float(r)) => {
            let a = match op {
                Add => l + r,
                Sub => l - r,
                Mul => l * r,
                Div => l / r,
                Rem => l % r,
            };
            Ok(ConstantValue::Float(a))
        }
        (ConstantValue::CString(l), ConstantValue::CString(r)) => match op {
            Add => Ok(ConstantValue::CString(l + &r)),
            Sub | Mul | Div | Rem => Err(ExpressionError::UnsupportedOperation(op.to_string())),
        },
        (ConstantValue::QString(_), ConstantValue::QString(_)) => {
            Err(ExpressionError::UnsupportedOperation(op.to_string()))
        }
        (left, right) => Err(ExpressionError::OperationOnIncompatibleTypes(
            op.to_string(),
            left.type_desc().qualified_name().into(),
            right.type_desc().qualified_name().into(),
        )),
    }
}

pub(super) fn eval_binary_bitwise_expression(
    op: BinaryBitwiseOp,
    left: ConstantValue,
    right: ConstantValue,
) -> Result<ConstantValue, ExpressionError> {
    use BinaryBitwiseOp::*;
    match (left, right) {
        (ConstantValue::Bool(l), ConstantValue::Bool(r)) => {
            let a = match op {
                RightShift | LeftShift => None,
                And => Some(l & r),
                Xor => Some(l ^ r),
                Or => Some(l | r),
            };
            a.map(ConstantValue::Bool)
                .ok_or_else(|| ExpressionError::UnsupportedOperation(op.to_string()))
        }
        (ConstantValue::Integer(l), ConstantValue::Integer(r)) => {
            let a = match op {
                RightShift => l.checked_shr(r.try_into()?),
                LeftShift => l.checked_shl(r.try_into()?),
                And => Some(l & r),
                Xor => Some(l ^ r),
                Or => Some(l | r),
            };
            a.map(ConstantValue::Integer)
                .ok_or(ExpressionError::IntegerOverflow)
        }
        (ConstantValue::Float(_), ConstantValue::Float(_))
        | (ConstantValue::CString(_), ConstantValue::CString(_))
        | (ConstantValue::QString(_), ConstantValue::QString(_)) => {
            Err(ExpressionError::UnsupportedOperation(op.to_string()))
        }
        (left, right) => Err(ExpressionError::OperationOnIncompatibleTypes(
            op.to_string(),
            left.type_desc().qualified_name().into(),
            right.type_desc().qualified_name().into(),
        )),
    }
}

pub(super) fn eval_binary_logical_expression(
    op: BinaryLogicalOp,
    left: ConstantValue,
    right: ConstantValue,
) -> Result<ConstantValue, ExpressionError> {
    use BinaryLogicalOp::*;
    match (left, right) {
        (ConstantValue::Bool(l), ConstantValue::Bool(r)) => {
            let a = match op {
                And => l && r,
                Or => l || r,
            };
            Ok(ConstantValue::Bool(a))
        }
        _ => Err(ExpressionError::UnsupportedOperation(op.to_string())),
    }
}
