//! Tree of QML objects.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Node, UiObjectDefinition};
use crate::typemap::{Class, NamedType, TypeSpace};
use std::collections::HashMap;

/// Tree of object definitions and the corresponding types.
///
/// This provides a global view of the QML document.
#[derive(Clone, Debug)]
pub struct ObjectTree<'a, 't> {
    nodes: Vec<ObjectNodeData<'a, 't>>,
    id_map: HashMap<String, usize>,
}

impl<'a, 't> ObjectTree<'a, 't> {
    /// Builds object tree from the given `root_node`.
    ///
    /// Returns None if the root node couldn't be parsed properly. Otherwise returns the tree
    /// even if some intermediate nodes couldn't be parsed.
    pub fn build(
        root_node: Node<'t>,
        source: &str,
        type_space: &impl TypeSpace<'a>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let mut tree = ObjectTree {
            nodes: Vec::new(),
            id_map: HashMap::new(),
        };
        if let Some(index) = tree.populate_node_rec(root_node, source, type_space, diagnostics) {
            assert!(index + 1 == tree.nodes.len());
            Some(tree)
        } else {
            None
        }
    }

    fn populate_node_rec(
        &mut self,
        node: Node<'t>,
        source: &str,
        type_space: &impl TypeSpace<'a>,
        diagnostics: &mut Diagnostics,
    ) -> Option<usize> {
        let obj = diagnostics.consume_err(UiObjectDefinition::from_node(node, source))?;
        let type_name = obj.type_name().to_string(source);
        // TODO: look up qualified name with '.' separator
        let (class, is_custom_type) = match type_space.get_type(&type_name) {
            Some(NamedType::Class(cls)) => (cls, false),
            Some(NamedType::QmlComponent(ns)) => (ns.into_class(), true),
            Some(NamedType::Enum(_) | NamedType::Namespace(_) | NamedType::Primitive(_)) | None => {
                diagnostics.push(Diagnostic::error(
                    obj.type_name().node().byte_range(),
                    format!("unknown object type: {type_name}"),
                ));
                return None;
            }
        };

        let child_indices = obj
            .child_object_nodes()
            .iter()
            .filter_map(|&n| self.populate_node_rec(n, source, type_space, diagnostics))
            .collect();

        let index = self.nodes.len();
        if let Some(id) = obj.object_id() {
            use std::collections::hash_map::Entry;
            match self.id_map.entry(id.to_str(source).to_owned()) {
                Entry::Vacant(e) => {
                    e.insert(index);
                }
                Entry::Occupied(_) => {
                    // TODO: add note pointing to the previous object id
                    diagnostics.push(Diagnostic::error(
                        id.node().byte_range(),
                        "duplicated object id",
                    ));
                }
            }
        }
        self.nodes.push(ObjectNodeData {
            class,
            is_custom_type,
            obj,
            child_indices,
        });

        Some(index)
    }

    /// Reference to the root object.
    pub fn root<'b>(&'b self) -> ObjectNode<'a, 't, 'b> {
        ObjectNode::new(self.nodes.last().expect("root node must exist"), self)
    }

    /// Iterates over the all objects in the tree.
    pub fn flat_iter<'b>(&'b self) -> impl Iterator<Item = ObjectNode<'a, 't, 'b>> {
        self.nodes.iter().map(|d| ObjectNode::new(d, self))
    }

    /// Number of the objects in the tree.
    pub fn flat_len(&self) -> usize {
        self.nodes.len()
    }

    /// Check whether or not the specified object id exists.
    pub fn contains_id<S>(&self, object_id: S) -> bool
    where
        S: AsRef<str>,
    {
        self.id_map.contains_key(object_id.as_ref())
    }

    /// Reference to the object specified by id.
    pub fn get_by_id<'b, S>(&'b self, object_id: S) -> Option<ObjectNode<'a, 't, 'b>>
    where
        S: AsRef<str>,
    {
        self.id_map
            .get(object_id.as_ref())
            .map(|&i| ObjectNode::new(&self.nodes[i], self))
    }
}

/// Reference to object definition with the type information.
#[derive(Clone, Copy, Debug)]
pub struct ObjectNode<'a, 't, 'b> {
    data: &'b ObjectNodeData<'a, 't>,
    tree: &'b ObjectTree<'a, 't>,
}

#[derive(Clone, Debug)]
struct ObjectNodeData<'a, 't> {
    class: Class<'a>,
    is_custom_type: bool,
    obj: UiObjectDefinition<'t>,
    child_indices: Vec<usize>,
}

impl<'a, 't, 'b> ObjectNode<'a, 't, 'b> {
    fn new(data: &'b ObjectNodeData<'a, 't>, tree: &'b ObjectTree<'a, 't>) -> Self {
        ObjectNode { data, tree }
    }

    pub fn class(&self) -> &Class<'a> {
        &self.data.class
    }

    pub fn is_custom_type(&self) -> bool {
        self.data.is_custom_type
    }

    pub fn obj(&self) -> &UiObjectDefinition<'t> {
        &self.data.obj
    }

    /// Iterates over the direct child objects.
    pub fn children(&self) -> impl Iterator<Item = ObjectNode<'a, 't, 'b>> {
        self.data
            .child_indices
            .iter()
            .map(|&i| ObjectNode::new(&self.tree.nodes[i], self.tree))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Diagnostics;
    use crate::metatype;
    use crate::qmlast::UiProgram;
    use crate::qmldoc::UiDocument;
    use crate::typemap::{ModuleData, ModuleId, TypeMap};
    use std::ptr;

    struct Env {
        doc: UiDocument,
        type_map: TypeMap,
        module_id: ModuleId<'static>,
    }

    impl Env {
        fn new(source: &str) -> Self {
            let mut type_map = TypeMap::with_primitive_types();
            let module_id = ModuleId::Named("foo".into());
            let mut module_data = ModuleData::with_builtins();
            module_data.extend([metatype::Class::new("Foo")]);
            type_map.insert_module(module_id.clone(), module_data);
            Env {
                doc: UiDocument::parse(source, None),
                type_map,
                module_id,
            }
        }

        fn try_build_tree(&self) -> Result<ObjectTree, Diagnostics> {
            let mut diagnostics = Diagnostics::new();
            let program = UiProgram::from_node(self.doc.root_node(), self.doc.source()).unwrap();
            let tree = ObjectTree::build(
                program.root_object_node(),
                self.doc.source(),
                &self.type_map.get_module(&self.module_id).unwrap(),
                &mut diagnostics,
            );
            if diagnostics.is_empty() {
                Ok(tree.unwrap())
            } else {
                Err(diagnostics)
            }
        }
    }

    #[test]
    fn lookup_by_object_id() {
        let env = Env::new(
            r###"
            Foo {
                id: root
                Foo { id: child0 }
                Foo {}
            }
            "###,
        );
        let tree = env.try_build_tree().unwrap();
        assert!(ptr::eq(
            tree.get_by_id("root").unwrap().obj(),
            tree.root().obj()
        ));
        assert!(ptr::eq(
            tree.get_by_id("child0").unwrap().obj(),
            tree.root().children().nth(0).unwrap().obj()
        ));
        assert!(!tree.contains_id("unknown"));
        assert!(tree.get_by_id("unknown").is_none());
    }

    #[test]
    fn duplicated_object_id() {
        let env = Env::new(
            r###"
            Foo {
                id: root
                Foo { id: dup }
                Foo { id: dup }
            }
            "###,
        );
        assert!(env.try_build_tree().is_err());
    }
}
