//! Newtype for Node.

use super::expr::Expression;
use super::stmt::Statement;
use super::ParseError;
use std::ops::Range;
use tree_sitter::{Node, TreeCursor};

/// CST node that represents an [`Expression`](Expression).
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ExpressionNode<'tree>(pub(super) Node<'tree>);

impl<'tree> ExpressionNode<'tree> {
    pub fn parse(&self, source: &str) -> Result<Expression<'tree>, ParseError<'tree>> {
        Expression::from_node(*self, source)
    }

    /// Returns true if this is a function or arrow function node without parentheses.
    pub fn is_bare_function(&self) -> bool {
        self.0.kind() == "function" || self.0.kind() == "arrow_function"
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
