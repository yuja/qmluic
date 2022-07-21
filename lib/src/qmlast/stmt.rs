use super::astutil;
use super::term::Identifier;
use super::{ParseError, ParseErrorKind};
use tree_sitter::{Node, TreeCursor};

/// Variant for statements.
#[derive(Clone, Debug)]
pub enum Statement<'tree> {
    /// Expression node.
    Expression(Node<'tree>),
    /// Block of statement nodes, or an empty statement without block.
    Block(Vec<Node<'tree>>),

    LexicalDeclaration(LexicalDeclaration<'tree>),
    If(IfStatement<'tree>),

    /// Return with optionally expression node.
    Return(Option<Node<'tree>>),
    // TODO: switch, etc., but it's unlikely we'll support export, import, etc.
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
            "lexical_declaration" => {
                LexicalDeclaration::with_cursor(cursor).map(Statement::LexicalDeclaration)?
            }
            "if_statement" => {
                let condition = astutil::get_child_by_field_name(node, "condition")?;
                let consequence = astutil::get_child_by_field_name(node, "consequence")?;
                let alternative = node
                    .child_by_field_name("alternative")
                    .map(|n| {
                        cursor.reset(n);
                        astutil::goto_first_named_child(cursor)?;
                        Ok(cursor.node())
                    })
                    .transpose()?;
                Statement::If(IfStatement {
                    condition,
                    consequence,
                    alternative,
                })
            }
            "return_statement" => {
                let expr = node.named_children(cursor).find(|n| !n.is_extra());
                Statement::Return(expr)
            }
            _ => {
                return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
            }
        };
        Ok(stmt)
    }
}

/// Represents a "let"/"const" declaration.
#[derive(Clone, Debug)]
pub struct LexicalDeclaration<'tree> {
    pub kind: LexicalDeclarationKind,
    pub variables: Vec<VariableDeclarator<'tree>>,
}

impl<'tree> LexicalDeclaration<'tree> {
    fn with_cursor(cursor: &mut TreeCursor<'tree>) -> Result<Self, ParseError<'tree>> {
        let node = cursor.node();
        let kind_node = astutil::get_child_by_field_name(node, "kind")?;
        let kind = LexicalDeclarationKind::from_node(kind_node)?;
        let variables = node
            .named_children(cursor)
            .filter(|&n| !n.is_extra() && n != kind_node)
            .map(VariableDeclarator::from_node)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(LexicalDeclaration { kind, variables })
    }
}

/// Type of lexical declaration.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum LexicalDeclarationKind {
    Let,
    Const,
}

impl LexicalDeclarationKind {
    fn from_node(node: Node) -> Result<Self, ParseError> {
        if node.is_named() {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }
        match node.kind() {
            "let" => Ok(LexicalDeclarationKind::Let),
            "const" => Ok(LexicalDeclarationKind::Const),
            _ => Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind)),
        }
    }
}

/// Pair of variable name and initializer expression.
///
/// This does not support array/object destructuring pattern.
#[derive(Clone, Debug)]
pub struct VariableDeclarator<'tree> {
    pub name: Identifier<'tree>,
    pub value: Option<Node<'tree>>,
}

impl<'tree> VariableDeclarator<'tree> {
    fn from_node(node: Node<'tree>) -> Result<Self, ParseError<'tree>> {
        if node.kind() != "variable_declarator" {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }
        let name =
            astutil::get_child_by_field_name(node, "name").and_then(Identifier::from_node)?;
        let value = node.child_by_field_name("value");
        Ok(VariableDeclarator { name, value })
    }
}

/// Represents an "if" statement.
#[derive(Clone, Debug)]
pub struct IfStatement<'tree> {
    pub condition: Node<'tree>,
    pub consequence: Node<'tree>,
    pub alternative: Option<Node<'tree>>,
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
        impl_unwrap_fn!(
            unwrap_lexical_declaration,
            Statement::LexicalDeclaration,
            LexicalDeclaration<'tree>
        );
        impl_unwrap_fn!(unwrap_if, Statement::If, IfStatement<'tree>);
        impl_unwrap_fn!(unwrap_return, Statement::Return, Option<Node<'tree>>);
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

    fn unwrap_block_stmt<'t>(doc: &'t UiDocument, name: &str) -> Statement<'t> {
        let ns = unwrap_stmt(doc, name).unwrap_block();
        assert!(ns.len() == 1);
        Statement::from_node(ns[0]).unwrap()
    }

    #[test]
    fn trivial_statements() {
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

    #[test]
    fn lexical_declaration() {
        let doc = parse(
            r###"
            Foo {
                let_one: { let x = 1 }
                let_uninit: { let x }
                const_two: { const x = 1, y = 2 }
            }
            "###,
        );

        let x = unwrap_block_stmt(&doc, "let_one").unwrap_lexical_declaration();
        assert_eq!(x.kind, LexicalDeclarationKind::Let);
        assert_eq!(x.variables.len(), 1);
        assert_eq!(x.variables[0].name.to_str(doc.source()), "x");
        assert!(Expression::from_node(x.variables[0].value.unwrap(), doc.source()).is_ok());

        let x = unwrap_block_stmt(&doc, "let_uninit").unwrap_lexical_declaration();
        assert_eq!(x.kind, LexicalDeclarationKind::Let);
        assert_eq!(x.variables.len(), 1);
        assert_eq!(x.variables[0].name.to_str(doc.source()), "x");
        assert!(x.variables[0].value.is_none());

        let x = unwrap_block_stmt(&doc, "const_two").unwrap_lexical_declaration();
        assert_eq!(x.kind, LexicalDeclarationKind::Const);
        assert_eq!(x.variables.len(), 2);
        assert_eq!(x.variables[0].name.to_str(doc.source()), "x");
        assert!(Expression::from_node(x.variables[0].value.unwrap(), doc.source()).is_ok());
        assert_eq!(x.variables[1].name.to_str(doc.source()), "y");
        assert!(Expression::from_node(x.variables[1].value.unwrap(), doc.source()).is_ok());
    }

    #[test]
    fn if_statement() {
        let doc = parse(
            r###"
            Foo {
                if_: if (0) /*garbage*/ { 1 }
                if_else: if (0) { 1 } else /*garbage*/ { 2 }
            }
            "###,
        );

        let x = unwrap_stmt(&doc, "if_").unwrap_if();
        assert!(Expression::from_node(x.condition, doc.source()).is_ok());
        assert!(Statement::from_node(x.consequence).is_ok());
        assert!(x.alternative.is_none());

        let x = unwrap_stmt(&doc, "if_else").unwrap_if();
        assert!(Expression::from_node(x.condition, doc.source()).is_ok());
        assert!(Statement::from_node(x.consequence).is_ok());
        assert!(Statement::from_node(x.alternative.unwrap()).is_ok());
        assert_ne!(x.consequence, x.alternative.unwrap());
    }

    #[test]
    fn return_statement() {
        let doc = parse(
            r###"
            Foo {
                return_none: { return; }
                return_garbage: { return /*garbage*/; }
                return_value: { return /*garbage*/ 1; }
            }
            "###,
        );

        let x = unwrap_block_stmt(&doc, "return_none").unwrap_return();
        assert!(x.is_none());

        let x = unwrap_block_stmt(&doc, "return_garbage").unwrap_return();
        assert!(x.is_none());

        let x = unwrap_block_stmt(&doc, "return_value").unwrap_return();
        assert!(Expression::from_node(x.unwrap(), doc.source()).is_ok());
    }
}
