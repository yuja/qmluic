use qmluic::qmlast;
use qmluic::typemap::{self, TypeMap};
use qmluic::uigen::{LayoutItem, LayoutItemContent, UiObject};
use quick_xml::events::{BytesStart, BytesText, Event};
use std::io;

pub struct UiBuilder<'a, W>
where
    W: io::Write,
{
    writer: quick_xml::Writer<W>,
    type_map: &'a TypeMap,
    doc: &'a qmlast::UiDocument,
    errors: Vec<qmlast::ParseError<'a>>,
}

impl<'a, W> UiBuilder<'a, W>
where
    W: io::Write,
{
    pub fn new(dest: W, type_map: &'a TypeMap, doc: &'a qmlast::UiDocument) -> Self {
        let writer = quick_xml::Writer::new_with_indent(dest, b' ', 1);
        UiBuilder {
            writer,
            type_map,
            doc,
            errors: Vec::new(),
        }
    }

    pub fn build(&mut self) -> quick_xml::Result<()> {
        let ui_tag = BytesStart::borrowed_name(b"ui")
            .with_attributes([(b"version".as_ref(), b"4.0".as_ref())]);
        self.writer
            .write_event(Event::Start(ui_tag.to_borrowed()))?;
        if let Some(name) = self.doc.type_name() {
            write_tagged_str(&mut self.writer, b"class", name)?;
        }
        self.process_program_node(self.doc.root_node())?;
        self.writer.write_event(Event::End(ui_tag.to_end()))?;
        self.writer.write(b"\n")?;
        Ok(())
    }

    fn process_program_node(&mut self, node: qmlast::Node<'a>) -> quick_xml::Result<()> {
        match qmlast::UiProgram::from_node(node) {
            Ok(x) => self
                .generate_object_rec(x.root_object_node())
                .map(|w| w.serialize_to_xml(&mut self.writer))
                .unwrap_or(Ok(()))?,
            Err(e) => self.errors.push(e),
        };
        Ok(())
    }

    fn resolve_object_definition(
        &mut self,
        node: qmlast::Node<'a>,
    ) -> Option<(qmlast::UiObjectDefinition<'a>, typemap::Class<'a>)> {
        let obj = match qmlast::UiObjectDefinition::from_node(node, self.doc.source()) {
            Ok(x) => x,
            Err(e) => {
                self.errors.push(e);
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
            UiObject::from_object_definition(&cls, &obj, self.doc.source(), &mut self.errors)?;
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
            LayoutItem::from_object_definition(&cls, &obj, self.doc.source(), &mut self.errors)?;
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
        self.errors.extend(
            obj.child_object_nodes()
                .iter()
                .copied()
                .map(unexpected_node),
        );
    }

    pub fn errors(&self) -> &[qmlast::ParseError<'a>] {
        &self.errors
    }
}

fn unexpected_node(node: qmlast::Node) -> qmlast::ParseError {
    qmlast::ParseError::new(node, qmlast::ParseErrorKind::UnexpectedNodeKind)
}

fn write_tagged_str<W>(
    writer: &mut quick_xml::Writer<W>,
    tag: &[u8],
    content: &str,
) -> quick_xml::Result<()>
where
    W: io::Write,
{
    let tag = BytesStart::borrowed_name(tag);
    writer.write_event(Event::Start(tag.to_borrowed()))?;
    writer.write_event(Event::Text(BytesText::from_plain_str(content)))?;
    writer.write_event(Event::End(tag.to_end()))?;
    Ok(())
}
