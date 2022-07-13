//! Type-checked intermediate representation of expressions.

use super::typeutil::{self, TypeError};
use crate::typedexpr::{BuiltinFunctionKind, BuiltinMethodKind, DescribeType, TypeDesc};
use crate::typemap::{Class, Enum, NamedType, Property, TypeKind};
use std::fmt;

/// Container of type-checked IR code.
#[derive(Clone, Debug)]
pub struct CodeBody<'a> {
    pub basic_blocks: Vec<BasicBlock<'a>>,
    pub locals: Vec<Local<'a>>,
}

impl CodeBody<'_> {
    pub(super) fn empty() -> Self {
        CodeBody {
            basic_blocks: vec![BasicBlock::empty()],
            locals: vec![],
        }
    }

    /// Checks if the return type is compatible with the given type.
    ///
    /// If not compatible, `TypeError::IncompatibleTypes(expected, actual)` will be returned.
    pub fn verify_return_type(&self, expected: &TypeKind) -> Result<(), TypeError> {
        for b in self.basic_blocks.iter() {
            if let Terminator::Return(a) = b.terminator() {
                typeutil::verify_concrete_type(expected, &a.type_desc())?;
            }
        }
        Ok(())
    }
}

/// List of statements to be run sequentially.
#[derive(Clone, Debug, PartialEq)]
pub struct BasicBlock<'a> {
    pub statements: Vec<Statement<'a>>,
    terminator: Option<Terminator<'a>>,
}

impl<'a> BasicBlock<'a> {
    pub(super) fn empty() -> Self {
        BasicBlock {
            statements: Vec::new(),
            terminator: None,
        }
    }

    pub fn terminator(&self) -> &Terminator<'a> {
        self.terminator
            .as_ref()
            .expect("terminator must have been set by builder")
    }

    pub(super) fn push_statement(&mut self, stmt: Statement<'a>) {
        assert!(self.terminator.is_none());
        self.statements.push(stmt);
    }

    pub(super) fn finalize(&mut self, term: Terminator<'a>) {
        assert!(self.terminator.is_none());
        self.terminator = Some(term);
    }
}

/// Basic block index.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BasicBlockRef(pub usize);

impl BasicBlockRef {
    pub(super) fn next(&self) -> Self {
        BasicBlockRef(self.0 + 1)
    }
}

/// Variant for statements.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    /// `<local> = <rvalue>`
    Assign(LocalRef, Rvalue<'a>),
}

/// Last instruction to exit from `BasicBlock`.
#[derive(Clone, Debug, PartialEq)]
pub enum Terminator<'a> {
    /// `goto <block>`
    Br(BasicBlockRef),
    /// `goto (<cond> ? <block1> : <block2>)`
    BrCond(Operand<'a>, BasicBlockRef, BasicBlockRef),
    /// `return <operand>`
    Return(Operand<'a>),
}

/// Variable or constant value.
#[derive(Clone, Debug, PartialEq)]
pub enum Operand<'a> {
    Constant(ConstantValue),
    EnumVariant(EnumVariant<'a>),
    Local(Local<'a>),
    NamedObject(NamedObject<'a>),
}

impl<'a> DescribeType<'a> for Operand<'a> {
    fn type_desc(&self) -> TypeDesc<'a> {
        match &self {
            Operand::Constant(x) => x.type_desc(),
            Operand::EnumVariant(x) => {
                TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(x.ty.clone())))
            }
            Operand::Local(x) => TypeDesc::Concrete(x.ty.clone()),
            Operand::NamedObject(x) => {
                TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(x.cls.clone())))
            }
        }
    }
}

/// Constant value which concrete type might not be determined yet.
#[derive(Clone, Debug, PartialEq)]
pub enum ConstantValue {
    Bool(bool),
    Integer(i64),
    Float(f64),
    /// Default string literal.
    CString(String),
    /// `QStringLiteral("")`
    QString(String),
    /// Empty list which element type is unknown.
    EmptyList,
}

impl DescribeType<'static> for ConstantValue {
    fn type_desc(&self) -> TypeDesc<'static> {
        match self {
            ConstantValue::Bool(_) => TypeDesc::BOOL,
            ConstantValue::Integer(_) => TypeDesc::ConstInteger,
            ConstantValue::Float(_) => TypeDesc::DOUBLE,
            ConstantValue::CString(_) => TypeDesc::ConstString,
            ConstantValue::QString(_) => TypeDesc::STRING,
            ConstantValue::EmptyList => TypeDesc::EmptyList,
        }
    }
}

/// Enum variant.
///
/// An enum variant is a constant, but the exact value is unknown.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumVariant<'a> {
    pub ty: Enum<'a>,
    pub variant: String,
}

impl<'a> EnumVariant<'a> {
    pub(super) fn new(ty: Enum<'a>, variant: impl Into<String>) -> Self {
        EnumVariant {
            ty,
            variant: variant.into(),
        }
    }

    pub fn cxx_expression(&self) -> String {
        self.ty.qualify_cxx_variant_name(&self.variant)
    }
}

/// Local (auto) variable with type information.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Local<'a> {
    pub name: LocalRef,
    pub ty: TypeKind<'a>,
}

impl<'a> Local<'a> {
    pub(super) fn new(name: usize, ty: TypeKind<'a>) -> Self {
        Local {
            name: LocalRef(name),
            ty,
        }
    }
}

/// Local (auto) variable index.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LocalRef(pub usize);

/// Member object with type information.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NamedObject<'a> {
    pub name: NamedObjectRef,
    pub cls: Class<'a>,
}

impl<'a> NamedObject<'a> {
    pub(super) fn new(name: impl Into<String>, cls: Class<'a>) -> Self {
        NamedObject {
            name: NamedObjectRef(name.into()),
            cls,
        }
    }
}

/// Member object name.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct NamedObjectRef(pub String);

/// Variant for rvalue expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum Rvalue<'a> {
    Copy(Operand<'a>),
    /// `<op> <arg>:<ty> -> <ty>`
    UnaryArithOp(UnaryArithOp, Operand<'a>),
    /// `<op> <arg>:<ty> -> <ty>`
    UnaryBitwiseOp(UnaryBitwiseOp, Operand<'a>),
    /// `<op> <arg>:<ty> -> bool`
    UnaryLogicalOp(UnaryLogicalOp, Operand<'a>),
    /// `<left>:<ty> <op> <right>:<ty> -> <ty>`
    BinaryArithOp(BinaryArithOp, Operand<'a>, Operand<'a>),
    /// `<left>:<ty> <op> <right>:<ty> -> <ty>`
    BinaryBitwiseOp(BinaryBitwiseOp, Operand<'a>, Operand<'a>),
    /// `<left>:<ty> <op> <right>:<ty> -> bool`
    BinaryLogicalOp(BinaryLogicalOp, Operand<'a>, Operand<'a>),
    /// `<left>:<ty> <op> <right>:<ty> -> bool`
    ComparisonOp(ComparisonOp, Operand<'a>, Operand<'a>),
    /// `<function>(<args>)`
    CallBuiltinFunction(BuiltinFunctionKind, Vec<Operand<'a>>),
    /// `<obj> -> <method>(<args>)`
    CallBuiltinMethod(Operand<'a>, BuiltinMethodKind, Vec<Operand<'a>>),
    /// `<obj> -> <read_property>()`
    ReadProperty(Operand<'a>, Property<'a>),
    /// `{<0>, <1>, ...}`
    MakeList(Vec<Operand<'a>>),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(super) enum UnaryOp {
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
    /// `>>`
    RightShift,
    /// `<<`
    LeftShift,
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
            BinaryBitwiseOp::RightShift => ">>",
            BinaryBitwiseOp::LeftShift => "<<",
            BinaryBitwiseOp::And => "&",
            BinaryBitwiseOp::Xor => "^",
            BinaryBitwiseOp::Or => "|",
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
