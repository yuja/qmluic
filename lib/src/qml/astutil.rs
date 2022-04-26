//! Utility for AST parsing and building.

use super::{ParseError, ParseErrorKind};
use tree_sitter::{Node, TreeCursor};

pub(crate) fn get_child_by_field_name<'tree>(
    node: Node<'tree>,
    name: &'static str,
) -> Result<Node<'tree>, ParseError<'tree>> {
    node.child_by_field_name(name)
        .ok_or_else(|| ParseError::new(node, ParseErrorKind::MissingField(name)))
}

pub(crate) fn node_text<'tree, 'source>(node: Node<'tree>, source: &'source str) -> &'source str {
    node.utf8_text(source.as_bytes())
        .expect("source range must be valid utf-8 string")
}

pub(crate) fn skip_extras<'tree>(cursor: &mut TreeCursor<'tree>) -> Result<(), ParseError<'tree>> {
    while cursor.node().is_extra() {
        let node = cursor.node();
        if node.is_error() {
            return Err(ParseError::new(node, ParseErrorKind::InvalidSyntax));
        }
        if !cursor.goto_next_sibling() {
            return Err(ParseError::new(node, ParseErrorKind::InvalidSyntax));
        }
    }
    Ok(())
}
