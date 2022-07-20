//! Newtype for Node.

use super::stmt::Statement;
use super::ParseError;
use std::ops::Range;
use tree_sitter::{Node, TreeCursor};

/// CST node that represents a [`Statement`](Statement).
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct StatementNode<'tree>(pub(super) Node<'tree>);

impl<'tree> StatementNode<'tree> {
    pub fn parse(&self) -> Result<Statement<'tree>, ParseError<'tree>> {
        Statement::from_node(*self)
    }

    pub(super) fn inner_node(&self) -> Node<'tree> {
        self.0
    }

    pub fn byte_range(&self) -> Range<usize> {
        self.0.byte_range()
    }

    pub(super) fn walk(&self) -> TreeCursor<'tree> {
        self.0.walk()
    }
}
