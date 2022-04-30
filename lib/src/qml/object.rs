use super::astutil;
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

        let root_object_node = astutil::get_child_by_field_name(node, "root")?;
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

        cursor.reset(astutil::get_child_by_field_name(node, "type_name")?);
        let type_name = NestedIdentifier::with_cursor(&mut cursor, source)?;

        cursor.reset(astutil::get_child_by_field_name(node, "initializer")?);
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

    /// Map of attached type property bindings.
    pub fn attached_type_map(&self) -> &UiAttachedTypeBindingMap<'tree, 'source> {
        &self.body.attached_type_map
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
    attached_type_map: UiAttachedTypeBindingMap<'tree, 'source>,
    binding_map: UiBindingMap<'tree, 'source>,
    // TODO: ...
}

impl<'tree, 'source> UiObjectBody<'tree, 'source> {
    fn with_cursor(
        cursor: &mut TreeCursor<'tree>,
        source: &'source str,
    ) -> Result<Self, ParseError<'tree>> {
        let container_node = cursor.node();
        if container_node.kind() != "ui_object_initializer" {
            return Err(ParseError::new(
                container_node,
                ParseErrorKind::UnexpectedNodeKind,
            ));
        }

        let mut object_id = None;
        let mut child_object_nodes = Vec::new();
        let mut attached_type_map = UiAttachedTypeBindingMap::new();
        let mut binding_map = UiBindingMap::new();
        for node in container_node.named_children(cursor) {
            // TODO: ui_annotated_object_member
            match node.kind() {
                "ui_object_definition" => {
                    let name_node = astutil::get_child_by_field_name(node, "type_name")?;
                    let name = NestedIdentifier::from_node(name_node, source)?;
                    if name.maybe_starts_with_type_name() {
                        child_object_nodes.push(node);
                    } else {
                        // grouped binding notation: base { prop: ...; ... }
                        try_insert_ui_grouped_binding_node(
                            &mut binding_map,
                            &name,
                            name_node,
                            astutil::get_child_by_field_name(node, "initializer")?,
                            source,
                        )?;
                    }
                }
                "ui_binding" => {
                    let name_node = astutil::get_child_by_field_name(node, "name")?;
                    let name = NestedIdentifier::from_node(name_node, source)?;
                    let value_node = astutil::get_child_by_field_name(node, "value")?;
                    if name.components() == [Identifier::new("id")] {
                        if object_id.is_some() {
                            return Err(ParseError::new(
                                name_node,
                                ParseErrorKind::DuplicatedBinding,
                            ));
                        }
                        object_id = Some(extract_object_id(value_node, source)?);
                    } else if let Some((type_name, prop_name)) = name.split_type_name_prefix() {
                        let map = attached_type_map.entry(type_name).or_default();
                        try_insert_ui_binding_node(map, &prop_name, name_node, value_node)?;
                    } else {
                        try_insert_ui_binding_node(&mut binding_map, &name, name_node, value_node)?;
                    }
                }
                // TODO: ...
                // order matters: (ERROR) node is extra
                _ if node.is_error() => {
                    return Err(ParseError::new(node, ParseErrorKind::InvalidSyntax));
                }
                _ if node.is_extra() => {}
                _ => {
                    return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
                }
            }
        }
        Ok(UiObjectBody {
            object_id,
            child_object_nodes,
            attached_type_map,
            binding_map,
        })
    }
}

/// Map of type name to attached property binding map.
pub type UiAttachedTypeBindingMap<'tree, 'source> =
    HashMap<NestedIdentifier<'source>, UiBindingMap<'tree, 'source>>;

/// Map of property binding name to value (or nested binding map.)
pub type UiBindingMap<'tree, 'source> =
    HashMap<Identifier<'source>, UiBindingValue<'tree, 'source>>;

/// Variant for the property binding map.
#[derive(Clone, Debug)]
pub enum UiBindingValue<'tree, 'source> {
    Node(Node<'tree>),
    Map(Node<'tree>, UiBindingMap<'tree, 'source>),
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
            UiBindingValue::Map(..) => None,
        }
    }

    /// Returns a reference to the map if this is a (nested) map.
    pub fn get_map(&self) -> Option<&UiBindingMap<'tree, 'source>> {
        match self {
            UiBindingValue::Node(_) => None,
            UiBindingValue::Map(_, m) => Some(m),
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

fn ensure_ui_binding_map_bases<'tree, 'source, 'map>(
    mut map: &'map mut UiBindingMap<'tree, 'source>,
    bases: &[Identifier<'source>],
    name_node: Node<'tree>,
) -> Result<&'map mut UiBindingMap<'tree, 'source>, ParseError<'tree>> {
    for &n in bases {
        match map
            .entry(n)
            .or_insert_with(|| UiBindingValue::Map(name_node, UiBindingMap::new()))
        {
            UiBindingValue::Node(_) => {
                return Err(ParseError::new(
                    name_node,
                    ParseErrorKind::DuplicatedBinding,
                ));
            }
            UiBindingValue::Map(_, m) => {
                map = m;
            }
        }
    }
    Ok(map)
}

fn try_insert_ui_binding_node<'tree, 'source>(
    map: &mut UiBindingMap<'tree, 'source>,
    name: &NestedIdentifier<'source>,
    name_node: Node<'tree>,
    value_node: Node<'tree>,
) -> Result<(), ParseError<'tree>> {
    use std::collections::hash_map::Entry;

    let (&last, bases) = name
        .components()
        .split_last()
        .ok_or_else(|| ParseError::new(name_node, ParseErrorKind::InvalidSyntax))?;

    let map = ensure_ui_binding_map_bases(map, bases, name_node)?;
    if let Entry::Vacant(e) = map.entry(last) {
        e.insert(UiBindingValue::Node(value_node));
    } else {
        return Err(ParseError::new(
            name_node,
            ParseErrorKind::DuplicatedBinding,
        ));
    }
    Ok(())
}

fn try_insert_ui_grouped_binding_node<'tree, 'source>(
    map: &mut UiBindingMap<'tree, 'source>,
    group_name: &NestedIdentifier<'source>,
    group_name_node: Node<'tree>,
    container_node: Node<'tree>,
    source: &'source str,
) -> Result<(), ParseError<'tree>> {
    if container_node.kind() != "ui_object_initializer" {
        return Err(ParseError::new(
            container_node,
            ParseErrorKind::UnexpectedNodeKind,
        ));
    }

    // Intermediate maps are created by the group_name_node, but the bottom map should be
    // attached to the container node if it's newly created.
    let map = {
        let (&last, bases) = group_name
            .components()
            .split_last()
            .ok_or_else(|| ParseError::new(group_name_node, ParseErrorKind::InvalidSyntax))?;
        let v = ensure_ui_binding_map_bases(map, bases, group_name_node)?
            .entry(last)
            .or_insert_with(|| UiBindingValue::Map(container_node, UiBindingMap::new()));
        match v {
            UiBindingValue::Node(_) => {
                return Err(ParseError::new(
                    group_name_node,
                    ParseErrorKind::DuplicatedBinding,
                ));
            }
            UiBindingValue::Map(_, m) => m,
        }
    };

    for node in container_node.named_children(&mut container_node.walk()) {
        // TODO: ui_annotated_object_member
        match node.kind() {
            "ui_object_definition" => {
                let name_node = astutil::get_child_by_field_name(node, "type_name")?;
                let name = NestedIdentifier::from_node(name_node, source)?;
                try_insert_ui_grouped_binding_node(
                    map,
                    &name,
                    name_node,
                    astutil::get_child_by_field_name(node, "initializer")?,
                    source,
                )?;
            }
            "ui_binding" => {
                let name_node = astutil::get_child_by_field_name(node, "name")?;
                let name = NestedIdentifier::from_node(name_node, source)?;
                let value_node = astutil::get_child_by_field_name(node, "value")?;
                try_insert_ui_binding_node(map, &name, name_node, value_node)?;
            }
            // order matters: (ERROR) node is extra
            _ if node.is_error() => {
                return Err(ParseError::new(node, ParseErrorKind::InvalidSyntax));
            }
            _ if node.is_extra() => {}
            _ => {
                return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
            }
        }
    }

    Ok(())
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
                // comment
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

    #[test]
    fn object_id_binding_with_comment() {
        let doc = UiDocument::with_source(r"Foo { id: /*what*/ ever /*never*/ }".to_owned());
        let root_obj = extract_root_object(&doc).unwrap();
        assert_eq!(root_obj.object_id(), Some(Identifier::new("ever")));
    }

    #[test]
    fn grouped_property_bindings() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                nested.a: 1
                nested { b: 2; /* comment */ c: 3 }
            }
            "###
            .to_owned(),
        );
        let root_obj = extract_root_object(&doc).unwrap();
        let map = root_obj.binding_map();
        assert_eq!(map.len(), 1);
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
    fn nested_grouped_property_bindings() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                a { b { c: 1 } }
            }
            "###
            .to_owned(),
        );
        let root_obj = extract_root_object(&doc).unwrap();
        let map = root_obj.binding_map();
        assert!(!map
            .get(&Identifier::new("a"))
            .unwrap()
            .get_map_value(&Identifier::new("b"))
            .unwrap()
            .get_map_value(&Identifier::new("c"))
            .unwrap()
            .is_map());
    }

    #[test]
    fn attached_property_bindings() {
        let doc = UiDocument::with_source(
            r###"
            Foo {
                Bar.baz: 0
                A.B.c.d: 1
                Bar.bar: 2
            }
            "###
            .to_owned(),
        );
        let root_obj = extract_root_object(&doc).unwrap();
        let type_map = root_obj.attached_type_map();
        assert_eq!(type_map.len(), 2);

        let bar_map = type_map
            .get(&NestedIdentifier::from(["Bar"].as_ref()))
            .unwrap();
        assert_eq!(bar_map.len(), 2);
        assert!(!bar_map.get(&Identifier::new("bar")).unwrap().is_map());
        assert!(!bar_map.get(&Identifier::new("baz")).unwrap().is_map());

        let ab_map = type_map
            .get(&NestedIdentifier::from(["A", "B"].as_ref()))
            .unwrap();
        assert_eq!(ab_map.len(), 1);
        assert!(ab_map.get(&Identifier::new("c")).unwrap().is_map());
        assert!(!ab_map
            .get(&Identifier::new("c"))
            .unwrap()
            .get_map_value(&Identifier::new("d"))
            .unwrap()
            .is_map());
    }
}
