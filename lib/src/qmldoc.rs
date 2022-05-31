//! QML source document management.

use camino::{Utf8Path, Utf8PathBuf};
use std::collections::HashMap;
use std::fs;
use std::io;
use tree_sitter::{Node, Parser, Query, QueryCursor, Tree};

/// Object holding QML source text and parsed tree.
#[derive(Clone, Debug)]
pub struct UiDocument {
    source: String,
    tree: Tree,
    path: Option<Utf8PathBuf>,
}

impl UiDocument {
    /// Creates parsed tree for the given QML source.
    ///
    /// The parsing doesn't fail even if the QML source has a syntax error. Instead, a node
    /// representing the error is inserted.
    pub fn parse<S>(source: S, path: Option<Utf8PathBuf>) -> Self
    where
        S: Into<String>,
    {
        let source = source.into();
        let mut parser = new_parser();
        let tree = parser
            .parse(source.as_bytes(), None)
            .expect("no timeout nor cancellation should have been made");
        UiDocument { source, tree, path }
    }

    /// Creates parsed tree from the given QML file.
    ///
    /// The parsing doesn't fail even if the QML source has a syntax error. Instead, a node
    /// representing the error is inserted.
    pub fn read<P>(path: P) -> io::Result<Self>
    where
        P: AsRef<Utf8Path>, // TODO: or Into<Utf8PathBuf>, but read(&path) makes more sense?
    {
        let path = path.as_ref();
        let source = fs::read_to_string(path)?;
        Ok(Self::parse(source, Some(path.to_owned())))
    }

    /// File path to this QML document.
    pub fn path(&self) -> Option<&Utf8Path> {
        self.path.as_deref()
    }

    /// Type (or component) name of this QML document.
    ///
    /// It's typically the file name without ".qml" suffix.
    pub fn type_name(&self) -> Option<&str> {
        self.path.as_ref().and_then(|p| p.file_stem())
    }

    pub fn has_syntax_error(&self) -> bool {
        self.tree.root_node().has_error()
    }

    /// Collects syntax error nodes from the parsed tree.
    pub fn collect_syntax_error_nodes<'a, B>(&'a self) -> B
    where
        B: FromIterator<Node<'a>>,
    {
        let query =
            Query::new(self.tree.language(), "(ERROR) @a").expect("static query must be valid");
        let mut cursor = QueryCursor::new();
        let matches = cursor.matches(&query, self.tree.root_node(), self.source.as_bytes());
        matches.map(|m| m.captures[0].node).collect()
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    /// Root node of the parsed tree.
    pub fn root_node(&self) -> Node {
        self.tree.root_node()
    }
}

/// Cache of [`UiDocument`]s loaded from file.
#[derive(Clone, Debug, Default)]
pub struct UiDocumentsCache {
    docs: HashMap<Utf8PathBuf, UiDocument>,
}

impl UiDocumentsCache {
    pub fn new() -> Self {
        UiDocumentsCache::default()
    }

    /// Reads the specified QML file if unavailable in cache, returns the cached document.
    pub fn read<P>(&mut self, path: P) -> io::Result<&mut UiDocument>
    where
        P: AsRef<Utf8Path>,
    {
        use std::collections::hash_map::Entry;
        let path = path.as_ref(); // user specified path to be kept in UiDocument object
        let doc = match self.docs.entry(path.canonicalize_utf8()?) {
            Entry::Occupied(e) => e.into_mut(),
            Entry::Vacant(e) => e.insert(UiDocument::read(path)?),
        };
        Ok(doc)
    }

    /// Returns the cached document for the specified path.
    pub fn get<P>(&self, path: P) -> Option<&UiDocument>
    where
        P: AsRef<Utf8Path>,
    {
        path.as_ref()
            .canonicalize_utf8()
            .ok()
            .and_then(|p| self.docs.get(&p))
    }

    /// Checks if the specified document is cached.
    pub fn contains<P>(&self, path: P) -> bool
    where
        P: AsRef<Utf8Path>,
    {
        self.get(path).is_some()
    }
}

fn new_parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_qmljs::language())
        .expect("QML grammar should be compatible with parser");
    parser
}

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
        let errors: Vec<_> = doc.collect_syntax_error_nodes();
        assert_eq!(errors.len(), 1);
    }
}
