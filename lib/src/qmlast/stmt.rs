use super::astutil;
use super::node::{ExpressionNode, StatementNode};
use super::term::{self, Identifier, NestedIdentifier};
use super::{ParseError, ParseErrorKind};
use tree_sitter::{Node, TreeCursor};

/// Variant for statements.
#[derive(Clone, Debug)]
pub enum Statement<'tree> {
    /// Expression node.
    Expression(ExpressionNode<'tree>),
    /// Block of statement nodes, or an empty statement without block.
    Block(Vec<StatementNode<'tree>>),

    LexicalDeclaration(LexicalDeclaration<'tree>),
    If(IfStatement<'tree>),
    Switch(SwitchStatement<'tree>),

    /// Break with optional label.
    Break(Option<Identifier<'tree>>),
    /// Return with optionally expression node.
    Return(Option<ExpressionNode<'tree>>),
}

impl<'tree> Statement<'tree> {
    pub fn from_node(node: StatementNode<'tree>) -> Result<Self, ParseError<'tree>> {
        Self::with_cursor(&mut node.walk())
    }

    pub(super) fn with_cursor(cursor: &mut TreeCursor<'tree>) -> Result<Self, ParseError<'tree>> {
        let node = cursor.node();
        let stmt = match node.kind() {
            "expression_statement" => {
                astutil::goto_first_named_child(cursor)?;
                Statement::Expression(ExpressionNode(cursor.node()))
            }
            "statement_block" => {
                let stmts = node
                    .named_children(cursor)
                    .filter(|n| !n.is_extra())
                    .map(StatementNode)
                    .collect();
                Statement::Block(stmts)
            }
            "empty_statement" => Statement::Block(vec![]),
            "lexical_declaration" => {
                LexicalDeclaration::with_cursor(cursor).map(Statement::LexicalDeclaration)?
            }
            "if_statement" => {
                let condition =
                    astutil::get_child_by_field_name(node, "condition").map(ExpressionNode)?;
                let consequence =
                    astutil::get_child_by_field_name(node, "consequence").map(StatementNode)?;
                let alternative = node
                    .child_by_field_name("alternative")
                    .map(|n| {
                        cursor.reset(n);
                        astutil::goto_first_named_child(cursor)?;
                        Ok(StatementNode(cursor.node()))
                    })
                    .transpose()?;
                Statement::If(IfStatement {
                    condition,
                    consequence,
                    alternative,
                })
            }
            "switch_statement" => SwitchStatement::with_cursor(cursor).map(Statement::Switch)?,
            "break_statement" => {
                let label = node
                    .child_by_field_name("label")
                    .map(Identifier::from_node)
                    .transpose()?;
                Statement::Break(label)
            }
            "return_statement" => {
                let expr = node
                    .named_children(cursor)
                    .find(|n| !n.is_extra())
                    .map(ExpressionNode);
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
    pub ty: Option<NestedIdentifier<'tree>>,
    pub value: Option<ExpressionNode<'tree>>,
}

impl<'tree> VariableDeclarator<'tree> {
    fn from_node(node: Node<'tree>) -> Result<Self, ParseError<'tree>> {
        if node.kind() != "variable_declarator" {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }
        let name =
            astutil::get_child_by_field_name(node, "name").and_then(Identifier::from_node)?;
        let ty = node
            .child_by_field_name("type")
            .map(term::extract_type_annotation)
            .transpose()?;
        let value = node.child_by_field_name("value").map(ExpressionNode);
        Ok(VariableDeclarator { name, ty, value })
    }

    pub fn node(&self) -> Node<'tree> {
        self.name
            .node()
            .parent()
            .expect("variable declarator name node should have parent")
    }
}

/// Represents an "if" statement.
#[derive(Clone, Debug)]
pub struct IfStatement<'tree> {
    pub condition: ExpressionNode<'tree>,
    pub consequence: StatementNode<'tree>,
    pub alternative: Option<StatementNode<'tree>>,
}

/// Represents a "switch" statement.
#[derive(Clone, Debug)]
pub struct SwitchStatement<'tree> {
    /// Expression to be matched against each case clause.
    pub value: ExpressionNode<'tree>,
    /// Case clauses.
    pub cases: Vec<SwitchCase<'tree>>,
    /// Default clause.
    pub default: Option<SwitchDefault<'tree>>,
}

/// Represents a "case" clause.
#[derive(Clone, Debug)]
pub struct SwitchCase<'tree> {
    /// Expression to be matched.
    pub value: ExpressionNode<'tree>,
    /// Statements to be executed.
    pub body: Vec<StatementNode<'tree>>,
}

/// Represents a "default" clause.
#[derive(Clone, Debug)]
pub struct SwitchDefault<'tree> {
    /// Index in the fall-through list, which is usually `cases.len()`.
    pub position: usize,
    /// Statements to be executed.
    pub body: Vec<StatementNode<'tree>>,
}

impl<'tree> SwitchStatement<'tree> {
    fn with_cursor(cursor: &mut TreeCursor<'tree>) -> Result<Self, ParseError<'tree>> {
        let switch_node = cursor.node();
        let switch_value =
            astutil::get_child_by_field_name(switch_node, "value").map(ExpressionNode)?;

        let switch_body_node = astutil::get_child_by_field_name(switch_node, "body")?;
        let mut cases = Vec::new();
        let mut default = None;
        for (i, node) in switch_body_node.named_children(cursor).enumerate() {
            match node.kind() {
                "switch_case" => {
                    let value =
                        astutil::get_child_by_field_name(node, "value").map(ExpressionNode)?;
                    let body = node
                        .children_by_field_name("body", &mut node.walk())
                        .map(StatementNode)
                        .collect();
                    cases.push(SwitchCase { value, body });
                }
                "switch_default" if default.is_some() => {
                    return Err(ParseError::new(node, ParseErrorKind::MultipleDefaultLabels));
                }
                "switch_default" => {
                    let body = node
                        .children_by_field_name("body", &mut node.walk())
                        .map(StatementNode)
                        .collect();
                    default = Some(SwitchDefault { position: i, body });
                }
                _ => {
                    return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
                }
            }
        }

        Ok(SwitchStatement {
            value: switch_value,
            cases,
            default,
        })
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
        impl_unwrap_fn!(
            unwrap_expression,
            Statement::Expression,
            ExpressionNode<'tree>
        );
        impl_unwrap_fn!(unwrap_block, Statement::Block, Vec<StatementNode<'tree>>);
        impl_unwrap_fn!(
            unwrap_lexical_declaration,
            Statement::LexicalDeclaration,
            LexicalDeclaration<'tree>
        );
        impl_unwrap_fn!(unwrap_if, Statement::If, IfStatement<'tree>);
        impl_unwrap_fn!(unwrap_switch, Statement::Switch, SwitchStatement<'tree>);
        impl_unwrap_fn!(unwrap_break, Statement::Break, Option<Identifier<'tree>>);
        impl_unwrap_fn!(
            unwrap_return,
            Statement::Return,
            Option<ExpressionNode<'tree>>
        );
    }

    fn parse(source: &str) -> UiDocument {
        UiDocument::parse(source, "MyType", None)
    }

    fn extract_stmt<'t>(doc: &'t UiDocument, name: &str) -> Result<Statement<'t>, ParseError<'t>> {
        let program = UiProgram::from_node(doc.root_node(), doc.source()).unwrap();
        let obj = UiObjectDefinition::from_node(program.root_object_node(), doc.source()).unwrap();
        let map = obj.build_binding_map(doc.source()).unwrap();
        let node = map.get(name).unwrap().get_node().unwrap();
        node.parse()
    }

    fn unwrap_stmt<'t>(doc: &'t UiDocument, name: &str) -> Statement<'t> {
        extract_stmt(doc, name).unwrap()
    }

    fn unwrap_block_stmt<'t>(doc: &'t UiDocument, name: &str) -> Statement<'t> {
        let ns = unwrap_stmt(doc, name).unwrap_block();
        assert!(ns.len() == 1);
        ns[0].parse().unwrap()
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
        assert!(
            Expression::from_node(ns[0].parse().unwrap().unwrap_expression(), doc.source()).is_ok()
        );
        assert!(matches!(ns[1].parse().unwrap(), Statement::Block(_)));
        assert!(
            Expression::from_node(ns[2].parse().unwrap().unwrap_expression(), doc.source()).is_ok()
        );

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
                let_typed: { let x: int }
                const_two: { const x = 1, y = 2 }
            }
            "###,
        );

        let x = unwrap_block_stmt(&doc, "let_one").unwrap_lexical_declaration();
        assert_eq!(x.kind, LexicalDeclarationKind::Let);
        assert_eq!(x.variables.len(), 1);
        assert_eq!(x.variables[0].name.to_str(doc.source()), "x");
        assert!(x.variables[0].ty.is_none());
        assert!(Expression::from_node(x.variables[0].value.unwrap(), doc.source()).is_ok());

        let x = unwrap_block_stmt(&doc, "let_uninit").unwrap_lexical_declaration();
        assert_eq!(x.kind, LexicalDeclarationKind::Let);
        assert_eq!(x.variables.len(), 1);
        assert_eq!(x.variables[0].name.to_str(doc.source()), "x");
        assert!(x.variables[0].ty.is_none());
        assert!(x.variables[0].value.is_none());

        let x = unwrap_block_stmt(&doc, "let_typed").unwrap_lexical_declaration();
        assert_eq!(x.kind, LexicalDeclarationKind::Let);
        assert_eq!(x.variables.len(), 1);
        assert_eq!(x.variables[0].name.to_str(doc.source()), "x");
        assert_eq!(
            x.variables[0].ty.as_ref().unwrap().to_string(doc.source()),
            "int"
        );
        assert!(x.variables[0].value.is_none());

        let x = unwrap_block_stmt(&doc, "const_two").unwrap_lexical_declaration();
        assert_eq!(x.kind, LexicalDeclarationKind::Const);
        assert_eq!(x.variables.len(), 2);
        assert_eq!(x.variables[0].name.to_str(doc.source()), "x");
        assert!(x.variables[0].ty.is_none());
        assert!(Expression::from_node(x.variables[0].value.unwrap(), doc.source()).is_ok());
        assert_eq!(x.variables[1].name.to_str(doc.source()), "y");
        assert!(x.variables[1].ty.is_none());
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
        assert!(x.consequence.parse().is_ok());
        assert!(x.alternative.is_none());

        let x = unwrap_stmt(&doc, "if_else").unwrap_if();
        assert!(Expression::from_node(x.condition, doc.source()).is_ok());
        assert!(x.consequence.parse().is_ok());
        assert!(x.alternative.unwrap().parse().is_ok());
        assert_ne!(x.consequence, x.alternative.unwrap());
    }

    #[test]
    fn switch_statement() {
        let doc = parse(
            r###"
            Foo {
                switch_no_default: switch (foo) {
                case /*garbage*/ 0:
                    "case 0" /*garbage*/;
                    break;
                case 1 /*garbage*/:
                case 2:
                    break a;
                }

                switch_with_default: switch (foo) {
                case 0:
                default:
                    "default";
                }

                switch_multiple_defaults: switch (foo) {
                case 0:
                default:
                    "default";
                default:
                    "default";
                }
            }
            "###,
        );

        let x = unwrap_stmt(&doc, "switch_no_default").unwrap_switch();
        assert!(Expression::from_node(x.value, doc.source()).is_ok());
        assert_eq!(x.cases.len(), 3);
        assert!(Expression::from_node(x.cases[0].value, doc.source()).is_ok());
        assert_eq!(x.cases[0].body.len(), 2);
        assert!(x.cases[0].body[1].parse().unwrap().unwrap_break().is_none());
        assert_eq!(x.cases[1].body.len(), 0);
        assert_eq!(x.cases[2].body.len(), 1);
        assert!(x.cases[2].body[0].parse().unwrap().unwrap_break().is_some());
        assert!(x.default.is_none());

        let x = unwrap_stmt(&doc, "switch_with_default").unwrap_switch();
        assert_eq!(x.cases.len(), 1);
        assert_eq!(x.cases[0].body.len(), 0);
        assert_eq!(x.default.as_ref().unwrap().position, 1);
        assert_eq!(x.default.as_ref().unwrap().body.len(), 1);

        assert!(extract_stmt(&doc, "switch_multiple_defaults").is_err());
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
