use super::astutil;
use super::node::StatementNode;
use super::stmt::Statement;
use super::term::{Identifier, NestedIdentifier};
use super::{ParseError, ParseErrorKind};
use std::collections::HashMap;
use tree_sitter::{Node, TreeCursor};

/// Represents a top-level QML program.
#[derive(Clone, Debug)]
pub struct UiProgram<'tree> {
    imports: Vec<UiImport<'tree>>,
    root_object_node: Node<'tree>,
}

impl<'tree> UiProgram<'tree> {
    pub fn from_node(node: Node<'tree>, source: &str) -> Result<Self, ParseError<'tree>> {
        Self::with_cursor(&mut node.walk(), source)
    }

    fn with_cursor(
        cursor: &mut TreeCursor<'tree>,
        source: &str,
    ) -> Result<Self, ParseError<'tree>> {
        let node = cursor.node();
        if node.kind() != "program" {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }

        let mut imports = Vec::new();
        for n in node.named_children(cursor) {
            match n.kind() {
                "ui_import" => imports.push(UiImport::from_node(n, source)?),
                // TODO: ui_pragma
                "ui_object_definition" | "ui_annotated_object" => {} // root
                _ => astutil::handle_uninteresting_node(n)?,
            }
        }

        let root_object_node = astutil::get_child_by_field_name(node, "root")?;

        Ok(UiProgram {
            imports,
            root_object_node,
        })
    }

    /// Import statements.
    pub fn imports(&self) -> &[UiImport<'tree>] {
        &self.imports
    }

    /// Node for the top-level object (or component.)
    pub fn root_object_node(&self) -> Node<'tree> {
        self.root_object_node
    }
}

/// Represents an import statement.
#[derive(Clone, Debug)]
pub struct UiImport<'tree> {
    node: Node<'tree>,
    source: UiImportSource<'tree>,
    version: Option<(u8, Option<u8>)>,
    alias: Option<Identifier<'tree>>,
}

/// Variant for the import source.
#[derive(Clone, Debug)]
pub enum UiImportSource<'tree> {
    /// (Dotted) identifier for a named module.
    Identifier(NestedIdentifier<'tree>),
    /// String which is typically a directory path.
    String(String),
}

impl<'tree> UiImport<'tree> {
    pub fn from_node(node: Node<'tree>, source: &str) -> Result<Self, ParseError<'tree>> {
        if node.kind() != "ui_import" {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }

        let source_node = astutil::get_child_by_field_name(node, "source")?;
        let source_mod = match source_node.kind() {
            "identifier" | "nested_identifier" => {
                UiImportSource::Identifier(NestedIdentifier::from_node(source_node)?)
            }
            "string" => UiImportSource::String(astutil::parse_string(source_node, source)?),
            _ => {
                return Err(ParseError::new(
                    source_node,
                    ParseErrorKind::UnexpectedNodeKind,
                ))
            }
        };

        let version = if let Some(version_node) = node.child_by_field_name("version") {
            let parse = |n| -> Result<u8, ParseError<'tree>> {
                astutil::node_text(n, source)
                    .parse()
                    .map_err(|_| ParseError::new(n, ParseErrorKind::InvalidSyntax))
            };
            Some((
                parse(astutil::get_child_by_field_name(version_node, "major")?)?,
                version_node
                    .child_by_field_name("minor")
                    .map(parse)
                    .transpose()?,
            ))
        } else {
            None
        };

        let alias = node
            .child_by_field_name("alias")
            .map(Identifier::from_node)
            .transpose()?;

        Ok(UiImport {
            node,
            source: source_mod,
            version,
            alias,
        })
    }

    pub fn node(&self) -> Node<'tree> {
        self.node
    }

    pub fn source(&self) -> &UiImportSource<'tree> {
        &self.source
    }

    pub fn version(&self) -> Option<(u8, Option<u8>)> {
        self.version
    }

    pub fn alias(&self) -> Option<Identifier<'tree>> {
        self.alias
    }
}

fn extract_object_id(node: StatementNode) -> Result<Identifier, ParseError> {
    if let Statement::Expression(n) = node.parse()? {
        Identifier::from_node(n)
    } else {
        Err(ParseError::new(
            node.inner_node(),
            ParseErrorKind::UnexpectedNodeKind,
        ))
    }
}

/// Represents a QML object or top-level component.
#[derive(Clone, Debug)]
pub struct UiObjectDefinition<'tree> {
    node: Node<'tree>,
    type_name: NestedIdentifier<'tree>,
    body: UiObjectBody<'tree>,
}

impl<'tree> UiObjectDefinition<'tree> {
    pub fn from_node(node: Node<'tree>, source: &str) -> Result<Self, ParseError<'tree>> {
        // TODO: ui_annotated_object
        if node.kind() != "ui_object_definition" {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }

        let mut cursor = node.walk();

        cursor.reset(astutil::get_child_by_field_name(node, "type_name")?);
        let type_name = NestedIdentifier::with_cursor(&mut cursor)?;

        cursor.reset(astutil::get_child_by_field_name(node, "initializer")?);
        let body = UiObjectBody::with_cursor(&mut cursor, source)?;

        Ok(UiObjectDefinition {
            node,
            type_name,
            body,
        })
    }

    pub fn node(&self) -> Node<'tree> {
        self.node
    }

    pub fn type_name(&self) -> &NestedIdentifier<'tree> {
        &self.type_name
    }

    /// Object id which should be unique within the QML program.
    pub fn object_id(&self) -> Option<Identifier<'tree>> {
        self.body.object_id
    }

    /// Nodes for the direct child objects.
    ///
    /// This does not include objects bound to the properties.
    pub fn child_object_nodes(&self) -> &[Node<'tree>] {
        &self.body.child_object_nodes
    }

    /// Nodes for the attached type property bindings.
    pub fn attached_type_bindings(&self) -> &[UiBinding<'tree>] {
        &self.body.attached_type_bindings
    }

    /// Creates map of attached type property bindings.
    pub fn build_attached_type_map<'s>(
        &self,
        source: &'s str,
    ) -> Result<UiAttachedTypeBindingMap<'tree, 's>, ParseError<'tree>> {
        let mut type_map = UiAttachedTypeBindingMap::new();
        for UiBinding { name, notation } in self.attached_type_bindings() {
            let (type_name, prop_name) = name
                .split_type_name_prefix(source)
                .expect("attached binding name should have type prefix");
            let (_, map) = type_map
                .entry(
                    type_name
                        .components()
                        .iter()
                        .map(|p| p.to_str(source))
                        .collect(),
                )
                .or_insert_with(|| (type_name, HashMap::new()));
            match *notation {
                UiBindingNotation::Scalar(n) => {
                    try_insert_ui_binding_node(map, &prop_name, n, source)?
                }
                UiBindingNotation::Grouped(_) => unreachable!("attached binding can't be grouped"),
            }
        }
        Ok(type_map)
    }

    /// Nodes for the property bindings.
    pub fn bindings(&self) -> &[UiBinding<'tree>] {
        &self.body.bindings
    }

    /// Creates map of property bindings.
    pub fn build_binding_map<'s>(
        &self,
        source: &'s str,
    ) -> Result<UiBindingMap<'tree, 's>, ParseError<'tree>> {
        let mut map = HashMap::new();
        for UiBinding { name, notation } in self.bindings() {
            match *notation {
                UiBindingNotation::Scalar(n) => {
                    try_insert_ui_binding_node(&mut map, name, n, source)?;
                }
                UiBindingNotation::Grouped(n) => {
                    try_insert_ui_grouped_binding_node(&mut map, name, n, source)?;
                }
            }
        }
        Ok(map)
    }
}

#[derive(Clone, Debug)]
struct UiObjectBody<'tree> {
    object_id: Option<Identifier<'tree>>,
    child_object_nodes: Vec<Node<'tree>>,
    attached_type_bindings: Vec<UiBinding<'tree>>,
    bindings: Vec<UiBinding<'tree>>,
    // TODO: ...
}

impl<'tree> UiObjectBody<'tree> {
    fn with_cursor(
        cursor: &mut TreeCursor<'tree>,
        source: &str,
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
        let mut attached_type_bindings = Vec::new();
        let mut bindings = Vec::new();
        for node in container_node.named_children(cursor) {
            // TODO: ui_annotated_object_member
            match node.kind() {
                "ui_object_definition" => {
                    let name_node = astutil::get_child_by_field_name(node, "type_name")?;
                    let name = NestedIdentifier::from_node(name_node)?;
                    if name.maybe_starts_with_type_name(source) {
                        child_object_nodes.push(node);
                    } else {
                        // grouped binding notation: base { prop: ...; ... }
                        let value_node = astutil::get_child_by_field_name(node, "initializer")?;
                        bindings.push(UiBinding {
                            name,
                            notation: UiBindingNotation::Grouped(value_node),
                        });
                    }
                }
                "ui_binding" => {
                    let name_node = astutil::get_child_by_field_name(node, "name")?;
                    let name = NestedIdentifier::from_node(name_node)?;
                    let value_node =
                        astutil::get_child_by_field_name(node, "value").map(StatementNode)?;
                    if name.to_string(source) == "id" {
                        if object_id.is_some() {
                            return Err(ParseError::new(
                                name_node,
                                ParseErrorKind::DuplicatedBinding,
                            ));
                        }
                        object_id = Some(extract_object_id(value_node)?);
                    } else if name.split_type_name_prefix(source).is_some() {
                        attached_type_bindings.push(UiBinding {
                            name,
                            notation: UiBindingNotation::Scalar(value_node),
                        });
                    } else {
                        bindings.push(UiBinding {
                            name,
                            notation: UiBindingNotation::Scalar(value_node),
                        });
                    }
                }
                // TODO: ...
                _ => astutil::handle_uninteresting_node(node)?,
            }
        }

        Ok(UiObjectBody {
            object_id,
            child_object_nodes,
            attached_type_bindings,
            bindings,
        })
    }
}

/// Represents a property binding.
#[derive(Clone, Debug)]
pub struct UiBinding<'tree> {
    name: NestedIdentifier<'tree>,
    notation: UiBindingNotation<'tree>,
}

// TODO: public interface to UiBinding

/// Variant for the property binding notation.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UiBindingNotation<'tree> {
    Scalar(StatementNode<'tree>),
    Grouped(Node<'tree>),
}

/// Map of type name to attached property binding map.
pub type UiAttachedTypeBindingMap<'tree, 'source> =
    HashMap<Vec<&'source str>, (NestedIdentifier<'tree>, UiBindingMap<'tree, 'source>)>;

/// Map of property binding name to value (or nested binding map.)
pub type UiBindingMap<'tree, 'source> = HashMap<&'source str, UiBindingValue<'tree, 'source>>;

/// Variant for the property binding map.
#[derive(Clone, Debug)]
pub enum UiBindingValue<'tree, 'source> {
    // TODO: rename Node and get_node() which are ambiguous
    Node(StatementNode<'tree>),
    Map(Node<'tree>, UiBindingMap<'tree, 'source>),
}

impl<'tree, 'source> UiBindingValue<'tree, 'source> {
    /// Node representing this expression or group.
    pub fn node(&self) -> Node<'tree> {
        match self {
            UiBindingValue::Node(n) => n.inner_node(),
            UiBindingValue::Map(n, _) => *n,
        }
    }

    /// Node representing the binding notation of this value.
    ///
    /// If this is an intermediate map, the parent (nested) identifier node will be
    /// returned. That wouldn't be ideal, but should be okay for error reporting.
    pub fn binding_node(&self) -> Node<'tree> {
        self.node()
            .parent()
            .expect("binding value node should have parent")
    }

    /// Returns whether this is a (nested) map or not.
    pub fn is_map(&self) -> bool {
        self.get_map().is_some()
    }

    /// Returns the node if this isn't a (nested) map.
    pub fn get_node(&self) -> Option<StatementNode<'tree>> {
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
    pub fn get_map_value(&self, k: &str) -> Option<&UiBindingValue<'tree, 'source>> {
        self.get_map().and_then(|m| m.get(k))
    }
}

fn ensure_ui_binding_map_bases<'tree, 'source, 'map>(
    mut map: &'map mut UiBindingMap<'tree, 'source>,
    full_name: &NestedIdentifier<'tree>,
    source: &'source str,
) -> Result<(&'map mut UiBindingMap<'tree, 'source>, Identifier<'tree>), ParseError<'tree>> {
    let mut name_iter = full_name.components().iter().copied();
    let mut cur = name_iter
        .next()
        .expect("identifier should have at least one name");
    for next in name_iter {
        // new map of name 'cur' is created for the 'next' node.
        match map
            .entry(cur.to_str(source))
            .or_insert_with(|| UiBindingValue::Map(next.node(), HashMap::new()))
        {
            UiBindingValue::Node(_) => {
                return Err(ParseError::new(
                    cur.node(),
                    ParseErrorKind::DuplicatedBinding,
                ));
            }
            UiBindingValue::Map(_, m) => {
                map = m;
            }
        }
        cur = next;
    }
    Ok((map, cur))
}

fn try_insert_ui_binding_node<'tree, 'source>(
    map: &mut UiBindingMap<'tree, 'source>,
    name: &NestedIdentifier<'tree>,
    value_node: StatementNode<'tree>,
    source: &'source str,
) -> Result<(), ParseError<'tree>> {
    use std::collections::hash_map::Entry;

    let (map, last) = ensure_ui_binding_map_bases(map, name, source)?;
    if let Entry::Vacant(e) = map.entry(last.to_str(source)) {
        e.insert(UiBindingValue::Node(value_node));
    } else {
        return Err(ParseError::new(
            last.node(),
            ParseErrorKind::DuplicatedBinding,
        ));
    }
    Ok(())
}

fn try_insert_ui_grouped_binding_node<'tree, 'source>(
    map: &mut UiBindingMap<'tree, 'source>,
    group_name: &NestedIdentifier<'tree>,
    container_node: Node<'tree>,
    source: &'source str,
) -> Result<(), ParseError<'tree>> {
    if container_node.kind() != "ui_object_initializer" {
        return Err(ParseError::new(
            container_node,
            ParseErrorKind::UnexpectedNodeKind,
        ));
    }

    // Intermediate maps are created by the group_name node, but the bottom map should be
    // attached to the container node if it's newly created.
    let map = {
        let (m, last) = ensure_ui_binding_map_bases(map, group_name, source)?;
        let v = m
            .entry(last.to_str(source))
            .or_insert_with(|| UiBindingValue::Map(container_node, HashMap::new()));
        match v {
            UiBindingValue::Node(_) => {
                return Err(ParseError::new(
                    last.node(),
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
                let name = NestedIdentifier::from_node(name_node)?;
                try_insert_ui_grouped_binding_node(
                    map,
                    &name,
                    astutil::get_child_by_field_name(node, "initializer")?,
                    source,
                )?;
            }
            "ui_binding" => {
                let name_node = astutil::get_child_by_field_name(node, "name")?;
                let name = NestedIdentifier::from_node(name_node)?;
                let value_node =
                    astutil::get_child_by_field_name(node, "value").map(StatementNode)?;
                try_insert_ui_binding_node(map, &name, value_node, source)?;
            }
            _ => astutil::handle_uninteresting_node(node)?,
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::qmldoc::UiDocument;

    fn parse(source: &str) -> UiDocument {
        UiDocument::parse(source, "MyType", None)
    }

    fn extract_root_object(doc: &UiDocument) -> Result<UiObjectDefinition, ParseError> {
        let program = UiProgram::from_node(doc.root_node(), doc.source()).unwrap();
        UiObjectDefinition::from_node(program.root_object_node(), doc.source())
    }

    impl<'tree> UiImportSource<'tree> {
        fn unwrap_identifier(&self) -> &NestedIdentifier<'tree> {
            match self {
                UiImportSource::Identifier(x) => x,
                _ => panic!("not an identifier"),
            }
        }

        fn unwrap_string(&self) -> &str {
            match self {
                UiImportSource::String(x) => x,
                _ => panic!("not a string"),
            }
        }
    }

    #[test]
    fn import_statements() {
        let doc = parse(
            r###"
            import Foo.Bar
            import Baz 2
            import Blah.Blah 2.1 as A
            import "Dir" as B
            Foo {}
            "###,
        );

        let program = UiProgram::from_node(doc.root_node(), doc.source()).unwrap();
        let imports = program.imports();
        assert_eq!(imports.len(), 4);

        assert_eq!(
            imports[0]
                .source()
                .unwrap_identifier()
                .to_string(doc.source()),
            "Foo.Bar"
        );
        assert_eq!(imports[0].version(), None);
        assert!(imports[0].alias().is_none());

        assert_eq!(
            imports[1]
                .source()
                .unwrap_identifier()
                .to_string(doc.source()),
            "Baz"
        );
        assert_eq!(imports[1].version(), Some((2, None)));
        assert!(imports[1].alias().is_none());

        assert_eq!(
            imports[2]
                .source()
                .unwrap_identifier()
                .to_string(doc.source()),
            "Blah.Blah"
        );
        assert_eq!(imports[2].version(), Some((2, Some(1))));
        assert_eq!(imports[2].alias().unwrap().to_str(doc.source()), "A");

        assert_eq!(imports[3].source().unwrap_string(), "Dir");
        assert_eq!(imports[3].version(), None);
        assert_eq!(imports[3].alias().unwrap().to_str(doc.source()), "B");
    }

    #[test]
    fn non_trivial_object_id_expression() {
        let doc = parse(r"Foo { id: (expr) }");
        assert!(extract_root_object(&doc).is_err());
    }

    #[test]
    fn trivial_object() {
        let doc = parse(r"Foo.Bar {}");
        let root_obj = extract_root_object(&doc).unwrap();
        assert_eq!(root_obj.type_name().to_string(doc.source()), "Foo.Bar");
        assert!(root_obj.child_object_nodes().is_empty());
    }

    #[test]
    fn nested_object() {
        let doc = parse(
            r###"
            Foo {
                // comment
                Bar.Bar {}
                Baz {}
            }
            "###,
        );
        let root_obj = extract_root_object(&doc).unwrap();
        assert_eq!(root_obj.type_name().to_string(doc.source()), "Foo");

        assert_eq!(root_obj.child_object_nodes().len(), 2);
        let child_objs: Vec<_> = root_obj
            .child_object_nodes()
            .iter()
            .map(|&n| UiObjectDefinition::from_node(n, doc.source()).unwrap())
            .collect();
        assert_eq!(child_objs[0].type_name().to_string(doc.source()), "Bar.Bar");
        assert_eq!(child_objs[1].type_name().to_string(doc.source()), "Baz");
    }

    #[test]
    fn property_bindings() {
        let doc = parse(
            r###"
            Foo {
                id: whatever
                bar: 0
                nested.a: 1
                nested.b: 2
                nested.c: 3
            }
            "###,
        );
        let root_obj = extract_root_object(&doc).unwrap();
        assert_eq!(
            root_obj.object_id().unwrap().to_str(doc.source()),
            "whatever"
        );
        let map = root_obj.build_binding_map(doc.source()).unwrap();
        assert_eq!(map.len(), 2);
        assert!(!map.get("bar").unwrap().is_map());
        let nested = map.get("nested").unwrap();
        assert!(nested.is_map());
        assert_eq!(nested.get_map().unwrap().len(), 3);
        assert!(!nested.get_map_value("a").unwrap().is_map());
        assert!(!nested.get_map_value("b").unwrap().is_map());
        assert!(!nested.get_map_value("c").unwrap().is_map());
    }

    #[test]
    fn duplicated_property_bindings() {
        let doc = parse(
            r###"
            Foo {
                bar: 0
                bar: 1
            }
            "###,
        );
        let root_obj = extract_root_object(&doc).unwrap();
        assert!(root_obj.build_binding_map(doc.source()).is_err());
    }

    #[test]
    fn duplicated_property_bindings_map_to_node() {
        let doc = parse(
            r###"
            Foo {
                bar.baz: 0
                bar: 1
            }
            "###,
        );
        let root_obj = extract_root_object(&doc).unwrap();
        assert!(root_obj.build_binding_map(doc.source()).is_err());
    }

    #[test]
    fn duplicated_property_bindings_node_to_map() {
        let doc = parse(
            r###"
            Foo {
                bar: 0
                bar.baz: 1
            }
            "###,
        );
        let root_obj = extract_root_object(&doc).unwrap();
        assert!(root_obj.build_binding_map(doc.source()).is_err());
    }

    #[test]
    fn duplicated_object_id_bindings() {
        let doc = parse(
            r###"
            Foo {
                id: foo
                id: bar
            }
            "###,
        );
        assert!(extract_root_object(&doc).is_err());
    }

    #[test]
    fn object_id_binding_with_comment() {
        let doc = parse(r"Foo { id: /*what*/ ever /*never*/ }");
        let root_obj = extract_root_object(&doc).unwrap();
        assert_eq!(root_obj.object_id().unwrap().to_str(doc.source()), "ever");
    }

    #[test]
    fn grouped_property_bindings() {
        let doc = parse(
            r###"
            Foo {
                nested.a: 1
                nested { b: 2; /* comment */ c: 3 }
            }
            "###,
        );
        let root_obj = extract_root_object(&doc).unwrap();
        let map = root_obj.build_binding_map(doc.source()).unwrap();
        assert_eq!(map.len(), 1);
        let nested = map.get("nested").unwrap();
        assert!(nested.is_map());
        assert_eq!(nested.get_map().unwrap().len(), 3);
        assert!(!nested.get_map_value("a").unwrap().is_map());
        assert!(!nested.get_map_value("b").unwrap().is_map());
        assert!(!nested.get_map_value("c").unwrap().is_map());
    }

    #[test]
    fn nested_grouped_property_bindings() {
        let doc = parse(
            r###"
            Foo {
                a { b { c: 1 } }
            }
            "###,
        );
        let root_obj = extract_root_object(&doc).unwrap();
        let map = root_obj.build_binding_map(doc.source()).unwrap();
        assert!(!map
            .get("a")
            .unwrap()
            .get_map_value("b")
            .unwrap()
            .get_map_value("c")
            .unwrap()
            .is_map());
    }

    #[test]
    fn attached_property_bindings() {
        let doc = parse(
            r###"
            Foo {
                Bar.baz: 0
                A.B.c.d: 1
                Bar.bar: 2
            }
            "###,
        );
        let root_obj = extract_root_object(&doc).unwrap();
        let type_map = root_obj.build_attached_type_map(doc.source()).unwrap();
        assert_eq!(type_map.len(), 2);

        let (_, bar_map) = type_map.get(["Bar"].as_ref()).unwrap();
        assert_eq!(bar_map.len(), 2);
        assert!(!bar_map.get("bar").unwrap().is_map());
        assert!(!bar_map.get("baz").unwrap().is_map());

        let (_, ab_map) = type_map.get(["A", "B"].as_ref()).unwrap();
        assert_eq!(ab_map.len(), 1);
        assert!(ab_map.get("c").unwrap().is_map());
        assert!(!ab_map
            .get("c")
            .unwrap()
            .get_map_value("d")
            .unwrap()
            .is_map());
    }
}
