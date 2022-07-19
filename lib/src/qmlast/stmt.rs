use super::astutil;
use super::{ParseError, ParseErrorKind};
use tree_sitter::{Node, TreeCursor};

/// Variant for statements.
#[derive(Clone, Debug)]
pub enum Statement<'tree> {
    /// Expression node.
    Expression(Node<'tree>),
    /// Block of statement nodes, or an empty statement without block.
    Block(Vec<Node<'tree>>),
    // TODO: if, switch, etc., but it's unlikely we'll support export, import, etc.
}

impl<'tree> Statement<'tree> {
    pub fn from_node(node: Node<'tree>) -> Result<Self, ParseError<'tree>> {
        Self::with_cursor(&mut node.walk())
    }

    pub(super) fn with_cursor(cursor: &mut TreeCursor<'tree>) -> Result<Self, ParseError<'tree>> {
        let node = cursor.node();
        let stmt = match node.kind() {
            "expression_statement" => {
                astutil::goto_first_named_child(cursor)?;
                Statement::Expression(cursor.node())
            }
            "statement_block" => {
                let stmts = node
                    .named_children(cursor)
                    .filter(|n| !n.is_extra())
                    .collect();
                Statement::Block(stmts)
            }
            "empty_statement" => Statement::Block(vec![]),
            _ => {
                return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
            }
        };
        Ok(stmt)
    }
}

#[cfg(test)]
mod tests {
    use super::super::expr::Expression;
    use super::*;
    use crate::qmlast::{UiObjectDefinition, UiProgram};
    use crate::qmldoc::UiDocument;

    macro_rules! impl_unwrap_fn {
        ($name:ident, $pat:path, $ty:ty) => {
            fn $name(self) -> $ty {
                match self {
                    $pat(x) => x,
                    _ => panic!("unexpected statement: {self:?}"),
                }
            }
        };
    }

    impl<'tree> Statement<'tree> {
        impl_unwrap_fn!(unwrap_expression, Statement::Expression, Node<'tree>);
        impl_unwrap_fn!(unwrap_block, Statement::Block, Vec<Node<'tree>>);
    }

    fn parse(source: &str) -> UiDocument {
        UiDocument::parse(source, "MyType", None)
    }

    fn extract_stmt<'t>(doc: &'t UiDocument, name: &str) -> Result<Statement<'t>, ParseError<'t>> {
        let program = UiProgram::from_node(doc.root_node(), doc.source()).unwrap();
        let obj = UiObjectDefinition::from_node(program.root_object_node(), doc.source()).unwrap();
        let map = obj.build_binding_map(doc.source()).unwrap();
        let node = map.get(name).unwrap().get_node().unwrap();
        Statement::from_node(node)
    }

    fn unwrap_stmt<'t>(doc: &'t UiDocument, name: &str) -> Statement<'t> {
        extract_stmt(doc, name).unwrap()
    }

    #[test]
    fn trivial_statementss() {
        let doc = parse(
            r###"
            Foo {
                identifier_expr: foo
                block: {
                    /*garbage*/ 0;
                    { nested; }
                    2 /*garbage*/
                }
                empty_block: {}
                empty_no_block: ;
            }
            "###,
        );

        let n = unwrap_stmt(&doc, "identifier_expr").unwrap_expression();
        assert!(Expression::from_node(n, doc.source()).is_ok());

        let ns = unwrap_stmt(&doc, "block").unwrap_block();
        assert_eq!(ns.len(), 3);
        assert!(Expression::from_node(
            Statement::from_node(ns[0]).unwrap().unwrap_expression(),
            doc.source()
        )
        .is_ok());
        assert!(matches!(
            Statement::from_node(ns[1]).unwrap(),
            Statement::Block(_)
        ));
        assert!(Expression::from_node(
            Statement::from_node(ns[2]).unwrap().unwrap_expression(),
            doc.source()
        )
        .is_ok());

        let ns = unwrap_stmt(&doc, "empty_block").unwrap_block();
        assert!(ns.is_empty());

        let ns = unwrap_stmt(&doc, "empty_no_block").unwrap_block();
        assert!(ns.is_empty());
    }
}
