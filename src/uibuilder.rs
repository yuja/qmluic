use qmluic::diagnostic::Diagnostics;
use qmluic::qmlast;
use qmluic::typemap::{self, TypeMap};
use qmluic::uigen::{LayoutItem, LayoutItemContent, UiForm, UiObject};

pub struct UiBuilder<'a> {
    type_map: &'a TypeMap,
    doc: &'a qmlast::UiDocument,
    diagnostics: &'a mut Diagnostics,
}

impl<'a> UiBuilder<'a> {
    pub fn new(
        type_map: &'a TypeMap,
        doc: &'a qmlast::UiDocument,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        UiBuilder {
            type_map,
            doc,
            diagnostics,
        }
    }

    pub fn build(&mut self) -> Option<UiForm> {
        self.diagnostics
            .consume_err(qmlast::UiProgram::from_node(self.doc.root_node()))
            .and_then(|p| self.generate_object_rec(p.root_object_node()))
            .map(|root_object| UiForm {
                class: self.doc.type_name().map(|s| s.to_owned()),
                root_object,
            })
    }

    fn resolve_object_definition(
        &mut self,
        node: qmlast::Node<'a>,
    ) -> Option<(qmlast::UiObjectDefinition<'a>, typemap::Class<'a>)> {
        let obj = match qmlast::UiObjectDefinition::from_node(node, self.doc.source()) {
            Ok(x) => x,
            Err(e) => {
                self.diagnostics.push(e);
                return None;
            }
        };
        // TODO: resolve against imported types: Qml.Type -> Cxx::Type -> type object
        let type_name = obj.type_name().to_string(self.doc.source());
        if let Some(typemap::Type::Class(cls)) = self.type_map.get_type(&type_name) {
            Some((obj, cls))
        } else {
            None
        }
    }

    fn generate_object_rec(&mut self, node: qmlast::Node<'a>) -> Option<UiObject> {
        let (obj, cls) = self.resolve_object_definition(node)?;
        let mut ui_obj =
            UiObject::from_object_definition(&cls, &obj, self.doc.source(), self.diagnostics)?;
        match &mut ui_obj {
            UiObject::Action(_) => self.confine_children(&obj),
            UiObject::Layout(layout) => {
                layout.children.extend(
                    obj.child_object_nodes()
                        .iter()
                        .filter_map(|&n| self.generate_layout_item_rec(n)),
                );
            }
            UiObject::Widget(widget) => {
                widget.children.extend(
                    obj.child_object_nodes()
                        .iter()
                        .filter_map(|&n| self.generate_object_rec(n)),
                );
            }
        }
        Some(ui_obj)
    }

    fn generate_layout_item_rec(&mut self, node: qmlast::Node<'a>) -> Option<LayoutItem> {
        let (obj, cls) = self.resolve_object_definition(node)?;
        let mut item =
            LayoutItem::from_object_definition(&cls, &obj, self.doc.source(), self.diagnostics)?;
        match &mut item.content {
            LayoutItemContent::Layout(layout) => {
                layout.children.extend(
                    obj.child_object_nodes()
                        .iter()
                        .filter_map(|&n| self.generate_layout_item_rec(n)),
                );
            }
            LayoutItemContent::SpacerItem(_) => self.confine_children(&obj),
            LayoutItemContent::Widget(widget) => {
                widget.children.extend(
                    obj.child_object_nodes()
                        .iter()
                        .filter_map(|&n| self.generate_object_rec(n)),
                );
            }
        }
        Some(item)
    }

    fn confine_children(&mut self, obj: &qmlast::UiObjectDefinition<'a>) {
        // action shouldn't have any children
        self.diagnostics.extend(
            obj.child_object_nodes()
                .iter()
                .copied()
                .map(unexpected_node),
        );
    }
}

fn unexpected_node(node: qmlast::Node) -> qmlast::ParseError {
    qmlast::ParseError::new(node, qmlast::ParseErrorKind::UnexpectedNodeKind)
}
