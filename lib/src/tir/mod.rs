//! Type-checked intermediate representation of expressions.

use crate::typedexpr::{DescribeType, TypeDesc};
use crate::typemap::{Class, NamedType, Property, TypeKind};
use std::fmt;

mod builder;

pub use self::builder::build; // re-export

/// Container of type-checked IR code.
#[derive(Clone, Debug)]
pub struct CodeBody<'a> {
    pub basic_blocks: Vec<BasicBlock<'a>>,
    pub locals: Vec<Local<'a>>,
}

impl CodeBody<'_> {
    fn empty() -> Self {
        CodeBody {
            basic_blocks: vec![BasicBlock::empty()],
            locals: vec![],
        }
    }
}

/// List of statements to be run sequentially.
#[derive(Clone, Debug, PartialEq)]
pub struct BasicBlock<'a> {
    pub statements: Vec<Statement<'a>>,
    terminator: Option<Terminator<'a>>,
}

impl<'a> BasicBlock<'a> {
    fn empty() -> Self {
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

    fn push_statement(&mut self, stmt: Statement<'a>) {
        assert!(self.terminator.is_none());
        self.statements.push(stmt);
    }

    fn finalize(&mut self, term: Terminator<'a>) {
        assert!(self.terminator.is_none());
        self.terminator = Some(term);
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
    /// `return <operand>`
    Return(Operand<'a>),
}

/// Variable or constant value.
#[derive(Clone, Debug, PartialEq)]
pub enum Operand<'a> {
    Constant(ConstantValue),
    Local(Local<'a>),
    NamedObject(NamedObject<'a>),
}

impl<'a> DescribeType<'a> for Operand<'a> {
    fn type_desc(&self) -> TypeDesc<'a> {
        match &self {
            Operand::Constant(x) => x.type_desc(),
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
}

impl DescribeType<'static> for ConstantValue {
    fn type_desc(&self) -> TypeDesc<'static> {
        match self {
            ConstantValue::Bool(_) => TypeDesc::BOOL,
            ConstantValue::Integer(_) => TypeDesc::ConstInteger,
            ConstantValue::Float(_) => TypeDesc::DOUBLE,
            ConstantValue::CString(_) => TypeDesc::ConstString,
            ConstantValue::QString(_) => TypeDesc::STRING,
        }
    }
}

/// Local (auto) variable with type information.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Local<'a> {
    pub name: LocalRef,
    pub ty: TypeKind<'a>,
}

impl<'a> Local<'a> {
    fn new(name: usize, ty: TypeKind<'a>) -> Self {
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
    fn new(name: impl Into<String>, cls: Class<'a>) -> Self {
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
    /// `<left>:<ty> <op> <right>:<ty> -> <ty>`
    BinaryArithOp(BinaryArithOp, Operand<'a>, Operand<'a>),
    /// `<obj> -> <read_property>()`
    ReadProperty(Operand<'a>, Property<'a>),
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
