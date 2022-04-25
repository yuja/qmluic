use super::term::{Identifier, NestedIdentifier};
use super::{ParseError, ParseErrorKind};
use std::collections::HashMap;
use tree_sitter::{Node, Query, QueryCursor, TreeCursor};

/// Represents a top-level QML program.
#[derive(Clone, Debug)]
pub struct UiProgram<'tree, 'source> {
    root_object_node: Node<'tree>,
    object_id_map: UiObjectIdMap<'tree, 'source>,
}

impl<'tree, 'source> UiProgram<'tree, 'source> {
    pub fn from_node(node: Node<'tree>, source: &'source str) -> Result<Self, ParseError<'tree>> {
        if node.kind() != "program" {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }

        // TODO: parse pragma and imports

        let root_object_node = get_child_by_field_name(node, "root")?;
        let object_id_map = build_object_id_map(root_object_node, source)?;

        Ok(UiProgram {
            root_object_node,
            object_id_map,
        })
    }

    /// Node for the top-level object (or component.)
    pub fn root_object_node(&self) -> Node<'tree> {
        self.root_object_node
    }

    /// Lookup object node by id.
    pub fn get_object_node_by_id(&self, id: &Identifier) -> Option<Node<'tree>> {
        self.object_id_map.get(id).copied()
    }

    /// Map of id to object node.
    pub fn object_id_map(&self) -> &UiObjectIdMap<'tree, 'source> {
        &self.object_id_map
    }
}

/// Map of id to object node.
pub type UiObjectIdMap<'tree, 'source> = HashMap<Identifier<'source>, Node<'tree>>;

fn build_object_id_map<'tree, 'source>(
    node: Node<'tree>,
    source: &'source str,
) -> Result<UiObjectIdMap<'tree, 'source>, ParseError<'tree>> {
    use std::collections::hash_map::Entry;

    let query = Query::new(
        node.language(),
        r###"
        (ui_object_definition
         initializer: (ui_object_initializer
                       (ui_binding
                        name: (identifier) @property
                        (#eq? @property "id")
                        value: (_) @value))) @object
        "###,
    )
    .expect("static query must be valid");
    let mut cursor = QueryCursor::new();
    let matches = cursor.matches(&query, node, source.as_bytes());

    let mut object_id_map = UiObjectIdMap::new();
    for m in matches {
        let expr_node = m
            .nodes_for_capture_index(1)
            .next()
            .expect("id expression should be captured");
        let obj_node = m
            .nodes_for_capture_index(2)
            .next()
            .expect("id object should be captured");
        let id = extract_object_id(expr_node, source)?;
        if let Entry::Vacant(e) = object_id_map.entry(id) {
            e.insert(obj_node);
        } else {
            return Err(ParseError::new(
                expr_node,
                ParseErrorKind::DuplicatedObjectId,
            ));
        }
    }
    Ok(object_id_map)
}

fn extract_object_id<'tree, 'source>(
    mut node: Node<'tree>,
    source: &'source str,
) -> Result<Identifier<'source>, ParseError<'tree>> {
    // (expression_statement (identifier))
    if node.kind() == "expression_statement" {
        node = node
            .child(0)
            .ok_or_else(|| ParseError::new(node, ParseErrorKind::InvalidSyntax))?;
    }
    Identifier::from_node(node, source)
}

/// Represents a QML object or top-level component.
#[derive(Clone, Debug)]
pub struct UiObjectDefinition<'tree, 'source> {
    type_name: NestedIdentifier<'source>,
    body: UiObjectBody<'tree, 'source>,
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
        let body = UiObjectBody::with_cursor(&mut cursor, source)?;

        Ok(UiObjectDefinition { type_name, body })
    }

    pub fn type_name(&self) -> &NestedIdentifier<'source> {
        &self.type_name
    }

    /// Object id which should be unique within the QML program.
    pub fn object_id(&self) -> Option<Identifier<'source>> {
        self.body.object_id
    }

    /// Nodes for the direct child objects.
    ///
    /// This does not include objects bound to the properties.
    pub fn child_object_nodes(&self) -> &[Node<'tree>] {
        &self.body.child_object_nodes
    }

    /// Map of property bindings.
    pub fn binding_map(&self) -> &UiBindingMap<'tree, 'source> {
        &self.body.binding_map
    }
}

#[derive(Clone, Debug)]
struct UiObjectBody<'tree, 'source> {
    object_id: Option<Identifier<'source>>,
    child_object_nodes: Vec<Node<'tree>>,
    binding_map: UiBindingMap<'tree, 'source>,
    // TODO: ...
}

impl<'tree, 'source> UiObjectBody<'tree, 'source> {
    fn with_cursor(
        cursor: &mut TreeCursor<'tree>,
        source: &'source str,
    ) -> Result<Self, ParseError<'tree>> {
        let container_node = cursor.node();
        let mut object_id = None;
        let mut child_object_nodes = Vec::new();
        let mut binding_map = UiBindingMap::new();
        for node in container_node.named_children(cursor) {
            // TODO: ui_annotated_object_member
            match node.kind() {
                "ui_object_definition" => {
                    // TODO: if type name is lowercase, process as grouped notation
                    child_object_nodes.push(node);
                }
                "ui_binding" => {
                    // TODO: split attached type name
                    let name = NestedIdentifier::from_node(
                        get_child_by_field_name(node, "name")?,
                        source,
                    )?;
                    let value_node = get_child_by_field_name(node, "value")?;
                    if name.components() == [Identifier::new("id")] {
                        if object_id.is_some() {
                            return Err(ParseError::new(node, ParseErrorKind::DuplicatedBinding));
                        }
                        object_id = Some(extract_object_id(value_node, source)?);
                    } else {
                        try_insert_ui_binding_node(&mut binding_map, node, &name, value_node)?;
                    }
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
        Ok(UiObjectBody {
            object_id,
            child_object_nodes,
            binding_map,
        })
    }
}

/// Map of property binding name to value (or nested binding map.)
pub type UiBindingMap<'tree, 'source> =
    HashMap<Identifier<'source>, UiBindingValue<'tree, 'source>>;

/// Variant for the property binding map.
#[derive(Clone, Debug)]
pub enum UiBindingValue<'tree, 'source> {
    Node(Node<'tree>),
    Map(UiBindingMap<'tree, 'source>),
}

impl<'tree, 'source> UiBindingValue<'tree, 'source> {
    /// Returns whether this is a (nested) map or not.
    pub fn is_map(&self) -> bool {
        self.get_map().is_some()
    }

    /// Returns the node if this isn't a (nested) map.
    pub fn get_node(&self) -> Option<Node<'tree>> {
        match self {
            UiBindingValue::Node(n) => Some(*n),
            UiBindingValue::Map(_) => None,
        }
    }

    /// Returns a reference to the map if this is a (nested) map.
    pub fn get_map(&self) -> Option<&UiBindingMap<'tree, 'source>> {
        match self {
            UiBindingValue::Node(_) => None,
            UiBindingValue::Map(m) => Some(m),
        }
    }

    /// Returns a reference to the value corresponding to the key.
    ///
    /// If this isn't a (nested) map, returns None.
    pub fn get_map_value(
        &self,
        k: &Identifier<'source>,
    ) -> Option<&UiBindingValue<'tree, 'source>> {
        self.get_map().and_then(|m| m.get(k))
    }
}

fn try_insert_ui_binding_node<'tree, 'source>(
    mut map: &mut UiBindingMap<'tree, 'source>,
    binding_node: Node<'tree>,
    name: &NestedIdentifier<'source>,
    value_node: Node<'tree>,
) -> Result<(), ParseError<'tree>> {
    use std::collections::hash_map::Entry;

    let (&last, bases) = name
        .components()
        .split_last()
        .ok_or_else(|| ParseError::new(binding_node, ParseErrorKind::InvalidSyntax))?;
    for &n in bases {
        match map
            .entry(n)
            .or_insert_with(|| UiBindingValue::Map(UiBindingMap::new()))
        {
            UiBindingValue::Node(_) => {
                return Err(ParseError::new(
                    binding_node,
                    ParseErrorKind::DuplicatedBinding,
                ));
            }
            UiBindingValue::Map(m) => {
                map = m;
            }
        }
    }

    if let Entry::Vacant(e) = map.entry(last) {
        e.insert(UiBindingValue::Node(value_node));
    } else {
        return Err(ParseError::new(
            binding_node,
            ParseErrorKind::DuplicatedBinding,
        ));
    }
    Ok(())
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
        let program = UiProgram::from_node(doc.root_node(), doc.source()).unwrap();
        UiObjectDefinition::from_node(program.root_object_node(), doc.source())
    }

    #[test]
    fn object_id_map() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                id: foo
                Bar.Bar { id: bar }
                Baz {
                    whatever: 0
                    id: baz
                }
                Blah.Blah {}
            }
            "###
            .to_owned(),
        );

        let program = UiProgram::from_node(doc.root_node(), doc.source()).unwrap();
        assert_eq!(
            program
                .get_object_node_by_id(&Identifier::new("foo"))
                .unwrap(),
            program.root_object_node()
        );

        let get_object_by_id = |id: &Identifier| {
            let n = program.get_object_node_by_id(id).unwrap();
            UiObjectDefinition::from_node(n, doc.source()).unwrap()
        };
        assert_eq!(
            get_object_by_id(&Identifier::new("bar")).type_name(),
            &NestedIdentifier::from(["Bar", "Bar"].as_ref())
        );
        assert_eq!(
            get_object_by_id(&Identifier::new("baz")).type_name(),
            &NestedIdentifier::from(["Baz"].as_ref())
        );
    }

    #[test]
    fn duplicated_object_ids() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                id: foo
                Foo { id: foo }
            }
            "###
            .to_owned(),
        );
        assert!(UiProgram::from_node(doc.root_node(), doc.source()).is_err());
    }

    #[test]
    fn non_trivial_object_id_expression() {
        let doc = UiDocument::with_source(r"Foo { id: (expr) }".to_owned());
        assert!(UiProgram::from_node(doc.root_node(), doc.source()).is_err());
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

    #[test]
    fn property_bindings() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                id: whatever
                bar: 0
                nested.a: 1
                nested.b: 2
                nested.c: 3
            }
            "###
            .to_owned(),
        );
        let root_obj = extract_root_object(&doc).unwrap();
        assert_eq!(root_obj.object_id(), Some(Identifier::new("whatever")));
        let map = root_obj.binding_map();
        assert_eq!(map.len(), 2);
        assert!(!map.get(&Identifier::new("bar")).unwrap().is_map());
        let nested = map.get(&Identifier::new("nested")).unwrap();
        assert!(nested.is_map());
        assert_eq!(nested.get_map().unwrap().len(), 3);
        assert!(!nested
            .get_map_value(&Identifier::new("a"))
            .unwrap()
            .is_map());
        assert!(!nested
            .get_map_value(&Identifier::new("b"))
            .unwrap()
            .is_map());
        assert!(!nested
            .get_map_value(&Identifier::new("c"))
            .unwrap()
            .is_map());
    }

    #[test]
    fn duplicated_property_bindings() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                bar: 0
                bar: 1
            }
            "###
            .to_owned(),
        );
        assert!(extract_root_object(&doc).is_err());
    }

    #[test]
    fn duplicated_property_bindings_map_to_node() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                bar.baz: 0
                bar: 1
            }
            "###
            .to_owned(),
        );
        assert!(extract_root_object(&doc).is_err());
    }

    #[test]
    fn duplicated_property_bindings_node_to_map() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                bar: 0
                bar.baz: 1
            }
            "###
            .to_owned(),
        );
        assert!(extract_root_object(&doc).is_err());
    }

    #[test]
    fn duplicated_object_id_bindings() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                id: foo
                id: bar
            }
            "###
            .to_owned(),
        );
        assert!(extract_root_object(&doc).is_err());
    }
}
