//! QML parser interface and document model.

use std::error::Error;
use std::fmt;
use std::ops::Range;

mod astutil;
mod expr;
mod object;
mod term;

pub use self::expr::*; // re-export
pub use self::object::*; // re-export
pub use self::term::*; // re-export
pub use tree_sitter::Node; // re-export

/// Semantic or syntax error occurred while parsing QML source.
#[derive(Clone, Debug)]
pub struct ParseError<'tree> {
    node: Node<'tree>,
    kind: ParseErrorKind,
}

/// Details of QML parse error.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ParseErrorKind {
    InvalidSyntax,
    UnexpectedNodeKind,
    MissingField(&'static str),
    DuplicatedBinding,
}

impl<'tree> ParseError<'tree> {
    pub fn new(node: Node<'tree>, kind: ParseErrorKind) -> Self {
        ParseError { node, kind }
    }

    pub fn node(&self) -> Node<'tree> {
        self.node
    }

    pub fn kind(&self) -> ParseErrorKind {
        self.kind
    }

    pub fn start_byte(&self) -> usize {
        self.node.start_byte()
    }

    pub fn end_byte(&self) -> usize {
        self.node.end_byte()
    }

    pub fn byte_range(&self) -> Range<usize> {
        self.node.byte_range()
    }
}

impl fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseErrorKind::*;
        match self.kind {
            InvalidSyntax => write!(f, "syntax error"),
            UnexpectedNodeKind => write!(f, "unexpected node kind: {}", self.node.kind()),
            MissingField(name) => write!(f, "missing field: {}", name),
            DuplicatedBinding => write!(f, "duplicated binding"),
        }
    }
}

impl Error for ParseError<'_> {}
