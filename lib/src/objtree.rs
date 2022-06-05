//! Tree of QML objects.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Node, UiObjectDefinition};
use crate::typemap::{Class, Type, TypeSpace};

/// Tree of object definitions and the corresponding types.
///
/// This provides a global view of the QML document.
#[derive(Clone, Debug)]
pub struct ObjectTree<'a, 't> {
    nodes: Vec<ObjectNodeData<'a, 't>>,
    // TODO: maybe build a {id: index} map
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
        let mut tree = ObjectTree { nodes: vec![] };
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
            Some(Type::Class(cls)) => (cls, false),
            Some(Type::QmlComponent(ns)) => (ns.into_class(), true),
            Some(Type::Enum(_) | Type::Namespace(_) | Type::Primitive(_)) | None => {
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
