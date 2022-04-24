use super::term::NestedIdentifier;
use super::{ParseError, ParseErrorKind};
use tree_sitter::{Node, TreeCursor};

/// Represents a QML object or top-level component.
#[derive(Clone, Debug)]
pub struct UiObjectDefinition<'tree, 'source> {
    type_name: NestedIdentifier<'source>,
    body: UiObjectBody<'tree>,
}

impl<'tree, 'source> UiObjectDefinition<'tree, 'source> {
    pub fn from_node(node: Node<'tree>, source: &'source str) -> Result<Self, ParseError<'tree>> {
        // TODO: ui_annotated_object
        if node.kind() != "ui_object_definition" {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }

        let mut cursor = node.walk();

        cursor.reset(get_child_by_field_name(node, "type_name")?);
        let type_name = NestedIdentifier::with_cursor(&mut cursor, source)?;

        cursor.reset(get_child_by_field_name(node, "initializer")?);
        let body = UiObjectBody::with_cursor(&mut cursor)?;

        Ok(UiObjectDefinition { type_name, body })
    }

    pub fn type_name(&self) -> &NestedIdentifier<'source> {
        &self.type_name
    }

    /// Nodes for the direct child objects.
    ///
    /// This does not include objects bound to the properties.
    pub fn child_object_nodes(&self) -> &[Node<'tree>] {
        &self.body.child_object_nodes
    }
}

#[derive(Clone, Debug)]
struct UiObjectBody<'tree> {
    child_object_nodes: Vec<Node<'tree>>,
    // TODO: ...
}

impl<'tree> UiObjectBody<'tree> {
    fn with_cursor(cursor: &mut TreeCursor<'tree>) -> Result<Self, ParseError<'tree>> {
        let container_node = cursor.node();
        let mut child_object_nodes = Vec::new();
        for node in container_node.named_children(cursor) {
            // TODO: ui_annotated_object_member
            match node.kind() {
                "ui_object_definition" => {
                    // TODO: if type name is lowercase, process as grouped notation
                    child_object_nodes.push(node);
                }
                // TODO: ...
                _ if node.is_error() => {
                    return Err(ParseError::new(node, ParseErrorKind::InvalidSyntax));
                }
                _ => {
                    return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
                }
            }
        }
        Ok(UiObjectBody { child_object_nodes })
    }
}

fn get_child_by_field_name<'tree>(
    node: Node<'tree>,
    name: &'static str,
) -> Result<Node<'tree>, ParseError<'tree>> {
    node.child_by_field_name(name)
        .ok_or_else(|| ParseError::new(node, ParseErrorKind::MissingField(name)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::qml::UiDocument;

    fn extract_root_object(doc: &UiDocument) -> Result<UiObjectDefinition, ParseError> {
        let node = doc.root_node().child_by_field_name("root").unwrap();
        UiObjectDefinition::from_node(node, doc.source())
    }

    #[test]
    fn trivial_object() {
        let doc = UiDocument::with_source(r"Foo.Bar {}".to_owned());
        let root_obj = extract_root_object(&doc).unwrap();
        assert_eq!(
            root_obj.type_name(),
            &NestedIdentifier::from(["Foo", "Bar"].as_ref())
        );
        assert!(root_obj.child_object_nodes().is_empty());
    }

    #[test]
    fn nested_object() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                Bar.Bar {}
                Baz {}
            }
            "###
            .to_owned(),
        );
        let root_obj = extract_root_object(&doc).unwrap();
        assert_eq!(
            root_obj.type_name(),
            &NestedIdentifier::from(["Foo"].as_ref())
        );

        assert_eq!(root_obj.child_object_nodes().len(), 2);
        let child_objs: Vec<_> = root_obj
            .child_object_nodes()
            .iter()
            .map(|&n| UiObjectDefinition::from_node(n, doc.source()).unwrap())
            .collect();
        assert_eq!(
            child_objs[0].type_name(),
            &NestedIdentifier::from(["Bar", "Bar"].as_ref())
        );
        assert_eq!(
            child_objs[1].type_name(),
            &NestedIdentifier::from(["Baz"].as_ref())
        );
    }
}
