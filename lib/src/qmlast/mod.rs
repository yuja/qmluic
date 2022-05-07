//! QML parser interface and document model.

use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::ops::Range;
use std::path::Path;
use tree_sitter::{Parser, Query, QueryCursor, Tree};

mod astutil;
mod expr;
mod object;
mod term;

pub use self::expr::*; // re-export
pub use self::object::*; // re-export
pub use self::term::*; // re-export
pub use tree_sitter::Node; // re-export

/// Object holding QML source text and parsed tree.
#[derive(Clone, Debug)]
pub struct UiDocument {
    source: String,
    tree: Tree,
}

impl UiDocument {
    /// Creates parsed tree for the given QML source.
    ///
    /// The parsing doesn't fail even if the QML source has a syntax error. Instead, a node
    /// representing the error is inserted.
    pub fn with_source(source: String) -> Self {
        let mut parser = new_parser();
        let tree = parser
            .parse(source.as_bytes(), None)
            .expect("no timeout nor cancellation should have been made");
        UiDocument { source, tree }
    }

    /// Creates parsed tree from the given QML file.
    ///
    /// The parsing doesn't fail even if the QML source has a syntax error. Instead, a node
    /// representing the error is inserted.
    pub fn read(path: impl AsRef<Path>) -> io::Result<Self> {
        let source = fs::read_to_string(path)?;
        Ok(Self::with_source(source))
    }

    pub fn has_syntax_error(&self) -> bool {
        self.tree.root_node().has_error()
    }

    /// Collects syntax errors from the parsed tree.
    pub fn collect_syntax_errors<'a, B>(&'a self) -> B
    where
        B: FromIterator<ParseError<'a>>,
    {
        let query =
            Query::new(self.tree.language(), "(ERROR) @a").expect("static query must be valid");
        let mut cursor = QueryCursor::new();
        let matches = cursor.matches(&query, self.tree.root_node(), self.source.as_bytes());
        matches
            .map(|m| ParseError::new(m.captures[0].node, ParseErrorKind::InvalidSyntax))
            .collect()
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    /// Root node of the parsed tree.
    pub fn root_node(&self) -> Node {
        self.tree.root_node()
    }
}

fn new_parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_qmljs::language())
        .expect("QML grammar should be compatible with parser");
    parser
}

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
    DuplicatedObjectId,
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
            DuplicatedObjectId => write!(f, "duplicated object id"),
        }
    }
}

impl Error for ParseError<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn syntax_error() {
        let doc = UiDocument::with_source(
            r###"
            import
            "###
            .to_owned(),
        );
        assert!(doc.has_syntax_error());
        let errors: Vec<_> = doc.collect_syntax_errors();
        assert_eq!(errors.len(), 1);
    }
}
