//! Type-checked intermediate representation of expressions.

use crate::typedexpr::{BuiltinFunctionKind, BuiltinMethodKind, DescribeType, TypeDesc};
use crate::typemap::{Class, Enum, Method, NamedType, Property, TypeKind};
use crate::typeutil::{self, TypeError};
use std::fmt;
use std::mem;
use std::ops::Range;

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

    /// Patches up terminators from the specified block so that the completion values
    /// will be returned.
    pub(super) fn finalize_completion_values(
        &mut self,
        start_ref: BasicBlockRef,
        byte_range: Range<usize>,
    ) {
        let start_block = &mut self.basic_blocks[start_ref.0];
        assert!(start_block.terminator.is_none());
        if let Some(a) = start_block.completion_value.take() {
            // common fast path: no need to build reverse "br" map
            start_block.terminator = Some(Terminator::Return(a));
            return;
        }

        // build reverse "br" map: conditional branches aren't collected since they
        // cannot be turned into "return", and a condition value isn't considered
        // a completion value.
        let mut incoming_map = vec![vec![]; self.basic_blocks.len()];
        let mut reachable = vec![false; self.basic_blocks.len()];
        reachable[0] = true;
        for (i, b) in self.basic_blocks.iter().enumerate() {
            match &b.terminator {
                Some(Terminator::Br(l)) => incoming_map[l.0].push(i),
                Some(Terminator::BrCond(_, a, b)) => {
                    reachable[a.0] = true;
                    reachable[b.0] = true;
                }
                Some(Terminator::Return(_) | Terminator::Unreachable) | None => {}
            }
        }

        // turn "br" into "return" while distance from the start_ref block is 0, where
        // distance = completion_value + statements.len()
        let mut to_visit = vec![start_ref.0];
        while let Some(i) = to_visit.pop() {
            let b = &mut self.basic_blocks[i];
            assert!(matches!(b.terminator, Some(Terminator::Br(_)) | None));
            if let Some(a) = b.completion_value.take() {
                b.terminator = Some(Terminator::Return(a));
            } else {
                b.terminator = if reachable[i] {
                    let end = byte_range.end; // implicit return should be at end
                    Some(Terminator::Return(Operand::Void(Void::new(end..end))))
                } else {
                    Some(Terminator::Unreachable)
                };
                if b.statements.is_empty() {
                    to_visit.extend(mem::take(&mut incoming_map[i]));
                }
            }
        }
    }
}

/// List of statements to be run sequentially.
#[derive(Clone, Debug, PartialEq)]
pub struct BasicBlock<'a> {
    pub statements: Vec<Statement<'a>>,
    completion_value: Option<Operand<'a>>,
    terminator: Option<Terminator<'a>>,
}

impl<'a> BasicBlock<'a> {
    pub(super) fn empty() -> Self {
        BasicBlock {
            statements: Vec::new(),
            completion_value: None,
            terminator: None,
        }
    }

    pub fn terminator(&self) -> &Terminator<'a> {
        self.terminator
            .as_ref()
            .expect("terminator must have been set by builder")
    }

    pub(super) fn set_completion_value(&mut self, value: Operand<'a>) {
        assert!(self.terminator.is_none());
        self.completion_value = Some(value);
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
    /// `<rvalue>`
    Exec(Rvalue<'a>),
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
    /// `unreachable()`
    Unreachable,
}

/// Variable or constant value.
#[derive(Clone, Debug, PartialEq)]
pub enum Operand<'a> {
    Constant(Constant),
    EnumVariant(EnumVariant<'a>),
    Local(Local<'a>),
    NamedObject(NamedObject<'a>),
    Void(Void),
}

impl Operand<'_> {
    /// Source code location of the expression that this operand represents.
    pub fn byte_range(&self) -> Range<usize> {
        match self {
            Operand::Constant(x) => x.byte_range.clone(),
            Operand::EnumVariant(x) => x.byte_range.clone(),
            Operand::Local(x) => x.byte_range.clone(),
            Operand::NamedObject(x) => x.byte_range.clone(),
            Operand::Void(x) => x.byte_range.clone(),
        }
    }
}

impl<'a> DescribeType<'a> for Operand<'a> {
    fn type_desc(&self) -> TypeDesc<'a> {
        match self {
            Operand::Constant(x) => x.value.type_desc(),
            Operand::EnumVariant(x) => {
                TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(x.ty.clone())))
            }
            Operand::Local(x) => TypeDesc::Concrete(x.ty.clone()),
            Operand::NamedObject(x) => {
                TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(x.cls.clone())))
            }
            Operand::Void(_) => TypeDesc::VOID,
        }
    }
}

/// Constant value which concrete type might not be determined yet.
#[derive(Clone, Debug, PartialEq)]
pub struct Constant {
    pub value: ConstantValue,
    pub byte_range: Range<usize>,
}

impl Constant {
    pub(super) fn new(value: ConstantValue, byte_range: Range<usize>) -> Self {
        Constant { value, byte_range }
    }
}

/// Variant for constant values.
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
    pub byte_range: Range<usize>,
}

impl<'a> EnumVariant<'a> {
    pub(super) fn new(ty: Enum<'a>, variant: impl Into<String>, byte_range: Range<usize>) -> Self {
        EnumVariant {
            ty,
            variant: variant.into(),
            byte_range,
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
    /// Source code location of the value expression that this variable holds.
    pub byte_range: Range<usize>,
}

impl<'a> Local<'a> {
    pub(super) fn new(name: usize, ty: TypeKind<'a>, byte_range: Range<usize>) -> Self {
        assert!(ty != TypeKind::VOID);
        Local {
            name: LocalRef(name),
            ty,
            byte_range,
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
    pub byte_range: Range<usize>,
}

impl<'a> NamedObject<'a> {
    pub(super) fn new(name: impl Into<String>, cls: Class<'a>, byte_range: Range<usize>) -> Self {
        NamedObject {
            name: NamedObjectRef(name.into()),
            cls,
            byte_range,
        }
    }
}

/// Member object name.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct NamedObjectRef(pub String);

/// Placeholder representing void expression.
///
/// Since void expression cannot be assigned to lvalue, the type of [`Local`](Local)
/// shouldn't be [`void`](TypeKind::VOID). Use `Void` instead.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Void {
    /// Source code location of the holding void expression.
    pub byte_range: Range<usize>,
}

impl Void {
    pub(super) fn new(byte_range: Range<usize>) -> Self {
        Void { byte_range }
    }
}

/// Variant for rvalue expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum Rvalue<'a> {
    Copy(Operand<'a>),
    /// `<op> <arg>`
    UnaryOp(UnaryOp, Operand<'a>),
    /// `<left> <op> <right>`
    BinaryOp(BinaryOp, Operand<'a>, Operand<'a>),
    /// `<function>(<args>)`
    CallBuiltinFunction(BuiltinFunctionKind, Vec<Operand<'a>>),
    /// `<obj> -> <method>(<args>)`
    CallBuiltinMethod(Operand<'a>, BuiltinMethodKind, Vec<Operand<'a>>),
    /// `<obj> -> <method>(<args>)`
    CallMethod(Operand<'a>, Method<'a>, Vec<Operand<'a>>),
    /// `<obj> -> <read_property>()`
    ReadProperty(Operand<'a>, Property<'a>),
    /// `<obj> -> <write_property>(<right>)`
    WriteProperty(Operand<'a>, Property<'a>, Operand<'a>),
    /// `{<0>, <1>, ...}`
    MakeList(Vec<Operand<'a>>),
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
