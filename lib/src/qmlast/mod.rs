//! QML parser interface and document model.

use camino::Utf8Path;
use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::ops::Range;
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
    type_name: Option<String>,
}

impl UiDocument {
    /// Creates parsed tree for the given QML source.
    ///
    /// The parsing doesn't fail even if the QML source has a syntax error. Instead, a node
    /// representing the error is inserted.
    pub fn parse<S>(source: S, type_name: Option<String>) -> Self
    where
        S: Into<String>,
    {
        let source = source.into();
        let mut parser = new_parser();
        let tree = parser
            .parse(source.as_bytes(), None)
            .expect("no timeout nor cancellation should have been made");
        UiDocument {
            source,
            tree,
            type_name,
        }
    }

    /// Creates parsed tree from the given QML file.
    ///
    /// The parsing doesn't fail even if the QML source has a syntax error. Instead, a node
    /// representing the error is inserted.
    pub fn read<P>(path: P) -> io::Result<Self>
    where
        P: AsRef<Utf8Path>,
    {
        let path = path.as_ref();
        let type_name = path.file_stem().map(|s| s.to_owned());
        let source = fs::read_to_string(path)?;
        Ok(Self::parse(source, type_name))
    }

    /// Type (or component) name of this QML document.
    ///
    /// It's typically the file name without ".qml" suffix.
    pub fn type_name(&self) -> Option<&str> {
        self.type_name.as_deref()
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

    fn parse(source: &str) -> UiDocument {
        UiDocument::parse(source, None)
    }

    #[test]
    fn syntax_error() {
        let doc = parse(
            r###"
            import
            "###,
        );
        assert!(doc.has_syntax_error());
        let errors: Vec<_> = doc.collect_syntax_errors();
        assert_eq!(errors.len(), 1);
    }
}
