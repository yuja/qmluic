use super::gadget::{ConstantGadget, ConstantSizePolicy};
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{BinaryOperator, Node, UiBindingMap, UiBindingValue, UnaryOperator};
use crate::typedexpr::{self, DescribeType, ExpressionVisitor, TypeDesc};
use crate::typemap::{Class, Enum, PrimitiveType, Type, TypeSpace};
use std::fmt;
use std::io;

/// Variant for the constant expressions which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub enum ConstantExpression {
    Value(ConstantValue),
    Gadget(ConstantGadget),
    SizePolicy(ConstantSizePolicy),
}

impl ConstantExpression {
    /// Generates constant expression of `ty` type from the given `binding_value`.
    pub fn from_binding_value<'a, P>(
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        ty: &Type,
        binding_value: &UiBindingValue,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self>
    where
        P: TypeSpace<'a>,
    {
        match binding_value {
            UiBindingValue::Node(n) => {
                Self::from_expression(parent_space, ty, *n, source, diagnostics)
            }
            UiBindingValue::Map(n, m) => match ty {
                Type::Class(cls) => Self::from_binding_map(cls, *n, m, source, diagnostics),
                _ => {
                    diagnostics.push(Diagnostic::error(
                        n.byte_range(),
                        format!(
                            "binding map cannot be parsed as non-class type '{}'",
                            ty.qualified_name()
                        ),
                    ));
                    None
                }
            },
        }
    }

    /// Generates constant expression of `cls` type from the given `binding_map`.
    pub fn from_binding_map(
        cls: &Class,
        node: Node,
        binding_map: &UiBindingMap,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match cls.name() {
            "QSizePolicy" => {
                ConstantSizePolicy::from_binding_map(cls, node, binding_map, source, diagnostics)
                    .map(ConstantExpression::SizePolicy)
            }
            _ => ConstantGadget::from_binding_map(cls, node, binding_map, source, diagnostics)
                .map(ConstantExpression::Gadget),
        }
    }

    /// Generates constant expression of `ty` type from the given expression `node`.
    pub fn from_expression<'a, P>(
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        ty: &Type,
        node: Node,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self>
    where
        P: TypeSpace<'a>,
    {
        ConstantValue::from_expression(parent_space, ty, node, source, diagnostics)
            .map(ConstantExpression::Value)
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use ConstantExpression::*;
        match self {
            Value(x) => x.serialize_to_xml(writer),
            Gadget(x) => x.serialize_to_xml(writer),
            SizePolicy(x) => x.serialize_to_xml(writer),
        }
    }
}

/// Constant expression which can be serialized to UI XML as a simple tagged value.
#[derive(Clone, Debug)]
pub enum ConstantValue {
    Bool(bool),
    Number(f64),
    String(String),
    Enum(String),
    Set(String),
}

impl ConstantValue {
    /// Generates value of `ty` type from the given `binding_value`.
    pub fn from_binding_value<'a, P>(
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        ty: &Type,
        binding_value: &UiBindingValue,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self>
    where
        P: TypeSpace<'a>,
    {
        extract_expression_node_for_type(ty, binding_value, diagnostics)
            .and_then(|n| Self::from_expression(parent_space, ty, n, source, diagnostics))
    }

    /// Generates value of `ty` type from the given expression `node`.
    pub fn from_expression<'a, P>(
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        ty: &Type,
        node: Node,
        source: &str,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self>
    where
        P: TypeSpace<'a>,
    {
        match ty {
            Type::Class(_) => {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!(
                        "unsupported constant value expression: class '{}'",
                        ty.qualified_name(),
                    ),
                ));
                None
            }
            Type::Enum(en) => {
                let (res_t, res_expr, _) = typedexpr::walk(
                    parent_space,
                    node,
                    source,
                    &ExpressionFormatter,
                    diagnostics,
                )?;
                match &res_t {
                    TypeDesc::Enum(res_en) if is_compatible_enum(res_en, en) => {
                        if en.is_flag() {
                            Some(ConstantValue::Set(res_expr))
                        } else {
                            Some(ConstantValue::Enum(res_expr))
                        }
                    }
                    _ => {
                        diagnostics.push(Diagnostic::error(
                            node.byte_range(),
                            format!(
                                "expression type mismatch (expected: {}, actual: {})",
                                ty.qualified_name(),
                                res_t.qualified_name()
                            ),
                        ));
                        None
                    }
                }
            }
            Type::Namespace(_) => {
                diagnostics.push(Diagnostic::error(
                    node.byte_range(),
                    format!(
                        "unsupported constant value expression: namespace '{}'",
                        ty.qualified_name(),
                    ),
                ));
                None
            }
            Type::Primitive(p) => {
                let res = typedexpr::walk(
                    parent_space,
                    node,
                    source,
                    &ExpressionEvaluator,
                    diagnostics,
                )?;
                if !describe_primitive_type(*p).map_or(false, |t| res.type_desc() == t) {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!(
                            "evaluated type mismatch (expected: {}, actual: {})",
                            ty.qualified_name(),
                            res.type_desc().qualified_name()
                        ),
                    ));
                    return None;
                }
                match res {
                    EvaluatedValue::Bool(v) => Some(ConstantValue::Bool(v)),
                    EvaluatedValue::Number(v) => Some(ConstantValue::Number(v)),
                    EvaluatedValue::String(v) => Some(ConstantValue::String(v)),
                }
            }
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use ConstantValue::*;
        match self {
            Bool(_) => xmlutil::write_tagged_str(writer, "bool", self.to_string()),
            Number(_) => xmlutil::write_tagged_str(writer, "number", self.to_string()),
            String(_) => xmlutil::write_tagged_str(writer, "string", self.to_string()),
            Enum(_) => xmlutil::write_tagged_str(writer, "enum", self.to_string()),
            Set(_) => xmlutil::write_tagged_str(writer, "set", self.to_string()),
        }
    }
}

impl fmt::Display for ConstantValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ConstantValue::*;
        match self {
            Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Number(d) => write!(f, "{}", d),
            String(s) | Enum(s) | Set(s) => write!(f, "{}", s),
        }
    }
}

fn extract_expression_node_for_type<'t>(
    ty: &Type,
    binding_value: &UiBindingValue<'t, '_>,
    diagnostics: &mut Diagnostics,
) -> Option<Node<'t>> {
    match binding_value {
        UiBindingValue::Node(n) => Some(*n),
        UiBindingValue::Map(n, _) => {
            diagnostics.push(Diagnostic::error(
                n.byte_range(),
                format!(
                    "binding map cannot be parsed as value type '{}'",
                    ty.qualified_name()
                ),
            ));
            None
        }
    }
}

fn describe_primitive_type(t: PrimitiveType) -> Option<TypeDesc<'static>> {
    match t {
        PrimitiveType::Bool => Some(TypeDesc::Bool),
        PrimitiveType::Int => Some(TypeDesc::Number),
        PrimitiveType::QReal => Some(TypeDesc::Number),
        PrimitiveType::QString => Some(TypeDesc::String),
        PrimitiveType::UInt => Some(TypeDesc::Number),
        PrimitiveType::Void => None,
    }
}

/// Evaluates the given `binding_value` to number.
pub(super) fn evaluate_number<'a, P>(
    parent_space: &P, // TODO: should be QML space, not C++ metatype space
    binding_value: &UiBindingValue,
    source: &str,
    diagnostics: &mut Diagnostics,
) -> Option<f64>
where
    P: TypeSpace<'a>,
{
    let ty = Type::Primitive(PrimitiveType::QReal); // don't care for the exact type
    let c =
        ConstantValue::from_binding_value(parent_space, &ty, binding_value, source, diagnostics)?;
    if let ConstantValue::Number(v) = c {
        Some(v)
    } else {
        None // diagnostic message should have been pushed
    }
}

/// Evaluates expression tree as arbitrary constant value expression.
///
/// Here we don't follow the JavaScript language model, but try to be stricter.
#[derive(Debug)]
struct ExpressionEvaluator;

#[derive(Clone, Debug)]
enum EvaluatedValue {
    Bool(bool),
    Number(f64),
    String(String),
}

impl DescribeType<'_> for EvaluatedValue {
    fn type_desc(&self) -> TypeDesc<'static> {
        match self {
            EvaluatedValue::Bool(_) => TypeDesc::Bool,
            EvaluatedValue::Number(_) => TypeDesc::Number,
            EvaluatedValue::String(_) => TypeDesc::String,
        }
    }
}

impl<'a> ExpressionVisitor<'a> for ExpressionEvaluator {
    type Item = EvaluatedValue;

    fn visit_number(&self, value: f64) -> Option<Self::Item> {
        Some(EvaluatedValue::Number(value))
    }

    fn visit_string(&self, value: String) -> Option<Self::Item> {
        Some(EvaluatedValue::String(value))
    }

    fn visit_bool(&self, value: bool) -> Option<Self::Item> {
        Some(EvaluatedValue::Bool(value))
    }

    fn visit_enum(&self, _enum_ty: Enum<'a>, _variant: &str) -> Option<Self::Item> {
        None // enum value is unknown
    }

    fn visit_call_expression(
        &self,
        function: &str,
        mut arguments: Vec<Self::Item>,
    ) -> Option<Self::Item> {
        match function {
            "qsTr" if arguments.len() == 1 => arguments.pop(),
            _ => None,
        }
    }

    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        argument: Self::Item,
    ) -> Option<Self::Item> {
        use UnaryOperator::*;
        match argument {
            EvaluatedValue::Bool(a) => match operator {
                LogicalNot => Some(EvaluatedValue::Bool(!a)),
                BitwiseNot => None,
                Minus | Plus => None,
                Typeof | Void | Delete => None,
            },
            EvaluatedValue::Number(a) => match operator {
                // TODO: handle overflow, etc.
                LogicalNot => None, // TODO: !
                BitwiseNot => None, // TODO: ~
                Minus => Some(EvaluatedValue::Number(-a)),
                Plus => Some(EvaluatedValue::Number(a)),
                Typeof | Void | Delete => None,
            },
            EvaluatedValue::String(_) => None,
        }
    }

    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
    ) -> Option<Self::Item> {
        use BinaryOperator::*;
        match (left, right) {
            #[allow(clippy::bool_comparison)]
            (EvaluatedValue::Bool(l), EvaluatedValue::Bool(r)) => match operator {
                LogicalAnd => Some(EvaluatedValue::Bool(l && r)),
                LogicalOr => Some(EvaluatedValue::Bool(l || r)),
                RightShift | UnsignedRightShift | LeftShift => None,
                BitwiseAnd => Some(EvaluatedValue::Bool(l & r)),
                BitwiseXor => Some(EvaluatedValue::Bool(l ^ r)),
                BitwiseOr => Some(EvaluatedValue::Bool(l | r)),
                Add | Sub | Mul | Div | Rem | Exp => None,
                Equal => Some(EvaluatedValue::Bool(l == r)),
                StrictEqual => Some(EvaluatedValue::Bool(l == r)),
                NotEqual => Some(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Some(EvaluatedValue::Bool(l != r)),
                LessThan => Some(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Some(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Some(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Some(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce | Instanceof | In => None,
            },
            (EvaluatedValue::Number(l), EvaluatedValue::Number(r)) => match operator {
                // TODO: handle overflow, etc.
                LogicalAnd | LogicalOr => None,
                // TODO: >>, (unsigned)>>, <<
                RightShift => None,
                UnsignedRightShift => None,
                LeftShift => None,
                // TODO: &, ^, |
                BitwiseAnd => None,
                BitwiseXor => None,
                BitwiseOr => None,
                Add => Some(EvaluatedValue::Number(l + r)),
                Sub => Some(EvaluatedValue::Number(l - r)),
                Mul => Some(EvaluatedValue::Number(l * r)),
                Div => Some(EvaluatedValue::Number(l / r)),
                Rem => Some(EvaluatedValue::Number(l % r)),
                Exp => Some(EvaluatedValue::Number(l.powf(r))),
                Equal => Some(EvaluatedValue::Bool(l == r)),
                StrictEqual => Some(EvaluatedValue::Bool(l == r)),
                NotEqual => Some(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Some(EvaluatedValue::Bool(l != r)),
                LessThan => Some(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Some(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Some(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Some(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce => None,
                Instanceof => None,
                In => None,
            },
            (EvaluatedValue::String(l), EvaluatedValue::String(r)) => match operator {
                LogicalAnd | LogicalOr => None,
                RightShift | UnsignedRightShift | LeftShift => None,
                BitwiseAnd | BitwiseXor | BitwiseOr => None,
                Add => Some(EvaluatedValue::String(l + &r)),
                Sub | Mul | Div | Rem | Exp => None,
                Equal => Some(EvaluatedValue::Bool(l == r)),
                StrictEqual => Some(EvaluatedValue::Bool(l == r)),
                NotEqual => Some(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Some(EvaluatedValue::Bool(l != r)),
                LessThan => Some(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Some(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Some(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Some(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce => None,
                Instanceof => None,
                In => None,
            },
            _ => None,
        }
    }
}

/// Formats expression tree as arbitrary constant value expression.
///
/// Here we don't strictly follow the JavaScript language model, but try 1:1 mapping.
#[derive(Debug)]
struct ExpressionFormatter;

impl<'a> DescribeType<'a> for (TypeDesc<'a>, String, u32) {
    fn type_desc(&self) -> TypeDesc<'a> {
        self.0.clone()
    }
}

impl<'a> ExpressionVisitor<'a> for ExpressionFormatter {
    type Item = (TypeDesc<'a>, String, u32);

    fn visit_number(&self, value: f64) -> Option<Self::Item> {
        Some((TypeDesc::Number, value.to_string(), PREC_TERM))
    }

    fn visit_string(&self, value: String) -> Option<Self::Item> {
        Some((TypeDesc::String, format!("{:?}", value), PREC_TERM)) // TODO: escape per C spec)
    }

    fn visit_bool(&self, value: bool) -> Option<Self::Item> {
        Some((
            TypeDesc::Bool,
            if value {
                "true".to_owned()
            } else {
                "false".to_owned()
            },
            PREC_TERM,
        ))
    }

    fn visit_enum(&self, enum_ty: Enum<'a>, variant: &str) -> Option<Self::Item> {
        let res_expr = enum_ty.qualify_variant_name(variant);
        Some((TypeDesc::Enum(enum_ty), res_expr, PREC_SCOPE))
    }

    fn visit_call_expression(
        &self,
        function: &str,
        arguments: Vec<Self::Item>,
    ) -> Option<Self::Item> {
        match function {
            "qsTr" if arguments.len() == 1 => {
                if let (TypeDesc::String, expr, _prec) = &arguments[0] {
                    Some((TypeDesc::String, format!("qsTr({})", expr), PREC_CALL))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        (arg_t, arg_expr, arg_prec): Self::Item,
    ) -> Option<Self::Item> {
        use UnaryOperator::*;
        let res_prec = unary_operator_precedence(operator);
        let res_expr = [
            unary_operator_str(operator),
            &maybe_paren(res_prec, arg_expr, arg_prec),
        ]
        .concat();
        match arg_t {
            TypeDesc::Bool => match operator {
                LogicalNot => Some((TypeDesc::Bool, res_expr, res_prec)),
                BitwiseNot => None,
                Minus | Plus => None,
                Typeof | Void | Delete => None,
            },
            TypeDesc::Number => match operator {
                LogicalNot => Some((TypeDesc::Bool, res_expr, res_prec)),
                BitwiseNot | Minus | Plus => Some((TypeDesc::Number, res_expr, res_prec)),
                Typeof | Void | Delete => None,
            },
            TypeDesc::String => None,
            TypeDesc::Enum(en) => match operator {
                LogicalNot => Some((TypeDesc::Bool, res_expr, res_prec)),
                BitwiseNot => Some((TypeDesc::Enum(en), res_expr, res_prec)),
                Minus | Plus => None,
                Typeof | Void | Delete => None,
            },
        }
    }

    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        (left_t, left_expr, left_prec): Self::Item,
        (right_t, right_expr, right_prec): Self::Item,
    ) -> Option<Self::Item> {
        use BinaryOperator::*;
        let res_prec = binary_operator_precedence(operator);
        match (left_t, right_t) {
            (TypeDesc::Bool, TypeDesc::Bool) => {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Some((TypeDesc::Bool, res_expr, res_prec)),
                    RightShift | UnsignedRightShift | LeftShift => None,
                    BitwiseAnd | BitwiseXor | BitwiseOr => {
                        Some((TypeDesc::Bool, res_expr, res_prec))
                    }
                    Add | Sub | Mul | Div | Rem | Exp => None,
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Some((TypeDesc::Bool, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => None,
                }
            }
            (TypeDesc::Number, TypeDesc::Number) => {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => None,
                    // TODO: >>, (unsigned)>>, <<
                    RightShift => None,
                    UnsignedRightShift => None,
                    LeftShift => None,
                    // TODO: &, ^, |
                    BitwiseAnd => None,
                    BitwiseXor => None,
                    BitwiseOr => None,
                    Add | Sub | Mul | Div | Rem => Some((TypeDesc::Number, res_expr, res_prec)),
                    Exp => None, // TODO
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Some((TypeDesc::Bool, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => None,
                }
            }
            (TypeDesc::String, TypeDesc::String) => {
                let (left_expr, left_prec) = (format!("QString({})", left_expr), PREC_CALL);
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => None,
                    RightShift | UnsignedRightShift | LeftShift => None,
                    BitwiseAnd | BitwiseXor | BitwiseOr => None,
                    Add => Some((TypeDesc::String, res_expr, res_prec)),
                    Sub | Mul | Div | Rem | Exp => None,
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Some((TypeDesc::Bool, res_expr, res_prec)),

                    NullishCoalesce | Instanceof | In => None,
                }
            }
            (TypeDesc::Enum(left_en), TypeDesc::Enum(right_en))
                if is_compatible_enum(&left_en, &right_en) =>
            {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Some((TypeDesc::Bool, res_expr, res_prec)),
                    RightShift | UnsignedRightShift | LeftShift => None,
                    BitwiseAnd | BitwiseXor | BitwiseOr => {
                        Some((TypeDesc::Enum(left_en), res_expr, res_prec))
                    }
                    Add | Sub | Mul | Div | Rem | Exp => None,
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Some((TypeDesc::Bool, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => None,
                }
            }
            _ => None,
        }
    }
}

fn is_compatible_enum(left_en: &Enum, right_en: &Enum) -> bool {
    left_en == right_en
        || left_en.alias_enum().map_or(false, |en| &en == right_en)
        || right_en.alias_enum().map_or(false, |en| &en == left_en)
}

fn maybe_paren(res_prec: u32, arg_expr: String, arg_prec: u32) -> String {
    if res_prec >= arg_prec {
        arg_expr
    } else {
        format!("({})", arg_expr)
    }
}

// 1-17: https://en.cppreference.com/w/cpp/language/operator_precedence
const PREC_TERM: u32 = 0;
const PREC_SCOPE: u32 = 1;
const PREC_CALL: u32 = 2;

fn unary_operator_precedence(operator: UnaryOperator) -> u32 {
    use UnaryOperator::*;
    match operator {
        LogicalNot => 3,
        BitwiseNot => 3,
        Minus => 3,
        Plus => 3,
        Typeof | Void | Delete => u32::MAX, // unsupported
    }
}

fn binary_operator_precedence(operator: BinaryOperator) -> u32 {
    use BinaryOperator::*;
    match operator {
        LogicalAnd => 14,
        LogicalOr => 15,
        RightShift | UnsignedRightShift => 7,
        LeftShift => 7,
        BitwiseAnd => 11,
        BitwiseXor => 12,
        BitwiseOr => 13,
        Add => 6,
        Sub => 6,
        Mul => 5,
        Div => 5,
        Rem => 5,
        Exp => u32::MAX, // unsupported
        Equal | StrictEqual => 10,
        NotEqual | StrictNotEqual => 10,
        LessThan => 9,
        LessThanEqual => 9,
        GreaterThan => 9,
        GreaterThanEqual => 9,
        NullishCoalesce | Instanceof | In => u32::MAX, // unsupported
    }
}

fn unary_operator_str(operator: UnaryOperator) -> &'static str {
    use UnaryOperator::*;
    match operator {
        LogicalNot => "!",
        BitwiseNot => "~",
        Minus => "-",
        Plus => "+",
        Typeof | Void | Delete => "/* unsupported */",
    }
}

fn binary_operator_str(operator: BinaryOperator) -> &'static str {
    use BinaryOperator::*;
    match operator {
        LogicalAnd => "&&",
        LogicalOr => "||",
        RightShift | UnsignedRightShift => ">>",
        LeftShift => "<<",
        BitwiseAnd => "&",
        BitwiseXor => "^",
        BitwiseOr => "|",
        Add => "+",
        Sub => "-",
        Mul => "*",
        Div => "/",
        Rem => "%",
        Exp => "/* unsupported */",
        Equal | StrictEqual => "==",
        NotEqual | StrictNotEqual => "!=",
        LessThan => "<",
        LessThanEqual => "<=",
        GreaterThan => ">",
        GreaterThanEqual => ">=",
        NullishCoalesce | Instanceof | In => "/* unsupported */",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Diagnostics;
    use crate::metatype;
    use crate::qmlast::{UiDocument, UiObjectDefinition, UiProgram};
    use crate::typemap::TypeMap;

    struct Env {
        doc: UiDocument,
        type_map: TypeMap,
    }

    impl Env {
        fn new(expr_source: &str) -> Self {
            let mut type_map = TypeMap::with_primitive_types();
            let mut foo_meta = metatype::Class::new("Foo");
            foo_meta
                .enums
                .push(metatype::Enum::with_values("Bar", ["Bar0", "Bar1", "Bar2"]));
            type_map.extend([foo_meta]);
            Env {
                doc: UiDocument::parse(format!("A {{ a: {expr_source}}}"), None),
                type_map,
            }
        }

        fn node(&self) -> Node {
            let program = UiProgram::from_node(self.doc.root_node()).unwrap();
            let obj = UiObjectDefinition::from_node(program.root_object_node(), self.doc.source())
                .unwrap();
            let map = obj.build_binding_map(self.doc.source()).unwrap();
            map.get("a").unwrap().get_node().unwrap()
        }

        fn format(&self) -> (TypeDesc, String, u32) {
            self.try_format().unwrap()
        }

        fn try_format(&self) -> Result<(TypeDesc, String, u32), Diagnostics> {
            let mut diagnostics = Diagnostics::new();
            let type_space = self.type_map.root();
            let node = self.node();
            typedexpr::walk(
                &type_space,
                node,
                self.doc.source(),
                &ExpressionFormatter,
                &mut diagnostics,
            )
            .ok_or(diagnostics)
        }
    }

    fn format_expr(expr_source: &str) -> String {
        let (_, expr, _) = Env::new(expr_source).format();
        expr
    }

    #[test]
    fn format_flags() {
        assert_eq!(format_expr("Foo.Bar0"), "Foo::Bar0");
        assert_eq!(
            format_expr("Foo.Bar0 | Foo.Bar1 | Foo.Bar2"),
            "Foo::Bar0|Foo::Bar1|Foo::Bar2"
        );
        assert_eq!(format_expr("Foo.Bar0 & ~Foo.Bar1"), "Foo::Bar0&~Foo::Bar1");
    }

    #[test]
    fn format_operator_precedence() {
        assert_eq!(format_expr("1 + (2 * 3) + 4"), "1+2*3+4");
        assert_eq!(format_expr("((1 + 2) * 3) + 4"), "(1+2)*3+4");
        assert_eq!(format_expr("1 + (2 * (3 + 4))"), "1+2*(3+4)");
        assert_eq!(format_expr("+(-1)"), "+-1");
        assert_eq!(format_expr("+((-1) + 1)"), "+(-1+1)");
    }

    #[test]
    fn format_operator_str() {
        assert_eq!(format_expr("1 === 1"), "1==1");
        assert_eq!(format_expr("1 !== 2"), "1!=2");
        assert_eq!(format_expr("'foo' + 'bar'"), r#"QString("foo")+"bar""#);
    }
}
