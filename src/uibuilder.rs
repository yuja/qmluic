use qmluic::qmlast;
use qmluic::typemap::{self, PrimitiveType, Type, TypeMap};
use qmluic::uigen::{Action, ConstantValue, Layout, SpacerItem, Widget};
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
            Ok(x) => self.process_object_definition_node(x.root_object_node())?,
            Err(e) => self.errors.push(e),
        };
        Ok(())
    }

    fn process_object_definition_node(&mut self, node: qmlast::Node<'a>) -> quick_xml::Result<()> {
        match qmlast::UiObjectDefinition::from_node(node, self.doc.source()) {
            Ok(x) => self.process_object_definition(&x)?,
            Err(e) => self.errors.push(e),
        };
        Ok(())
    }

    fn process_object_definition(
        &mut self,
        obj: &qmlast::UiObjectDefinition<'a>,
    ) -> quick_xml::Result<()> {
        // TODO: resolve against imported types: Qml.Type -> Cxx::Type -> type object
        let type_name = obj.type_name().to_string(self.doc.source());
        if let Some(typemap::Type::Class(cls)) = self.type_map.get_type(&type_name) {
            if cls.name() == "QAction" {
                self.process_action_definition(&cls, obj)
            } else if type_name.ends_with("Layout") {
                self.process_layout_definition(&cls, obj)
            } else if cls.name() == "QSpacerItem" {
                self.process_spacer_definition(&cls, obj)
            } else {
                self.process_widget_definition(&cls, obj)
            }
        } else {
            // TODO: better error message
            self.errors.push(unexpected_node(obj.node()));
            Ok(())
        }
    }

    fn process_action_definition(
        &mut self,
        cls: &typemap::Class,
        obj: &qmlast::UiObjectDefinition<'a>,
    ) -> quick_xml::Result<()> {
        Action::from_object_definition(cls, obj, self.doc.source(), &mut self.errors)
            .map(|w| w.serialize_to_xml(&mut self.writer))
            .unwrap_or(Ok(()))?;

        // action shouldn't have any children
        self.errors.extend(
            obj.child_object_nodes()
                .iter()
                .copied()
                .map(unexpected_node),
        );

        Ok(())
    }

    fn process_layout_definition(
        &mut self,
        cls: &typemap::Class,
        obj: &qmlast::UiObjectDefinition<'a>,
    ) -> quick_xml::Result<()> {
        Layout::from_object_definition(cls, obj, self.doc.source(), &mut self.errors)
            .map(|w| w.serialize_to_xml(&mut self.writer))
            .unwrap_or(Ok(()))?;

        let obj_tag = BytesStart::borrowed_name(b"layout");

        for &n in obj.child_object_nodes() {
            let child = match qmlast::UiObjectDefinition::from_node(n, self.doc.source()) {
                Ok(x) => x,
                Err(e) => {
                    self.errors.push(e);
                    continue;
                }
            };
            let attached_type_map = match child.build_attached_type_map(self.doc.source()) {
                Ok(x) => x,
                Err(e) => {
                    self.errors.push(e);
                    continue;
                }
            };

            let mut item_tag = BytesStart::borrowed_name(b"item");
            // TODO: resolve against imported types
            if let Some(map) = attached_type_map.get(["QLayoutItem"].as_ref()) {
                // TODO: look up attached type
                for (name, value) in collect_sorted_binding_pairs(map) {
                    match value {
                        qmlast::UiBindingValue::Node(n) => {
                            if let Some(v) = ConstantValue::from_expression(
                                &Type::Primitive(PrimitiveType::Int),
                                *n,
                                self.doc.source(),
                                &mut self.errors,
                            ) {
                                item_tag.push_attribute((name, v.to_string().as_ref()));
                            }
                        }
                        qmlast::UiBindingValue::Map(n, _) => self.errors.push(unexpected_node(*n)),
                    }
                }
            }
            self.writer
                .write_event(Event::Start(item_tag.to_borrowed()))?;

            self.process_object_definition(&child)?;

            self.writer.write_event(Event::End(item_tag.to_end()))?;
        }

        self.writer.write_event(Event::End(obj_tag.to_end()))?;
        Ok(())
    }

    fn process_spacer_definition(
        &mut self,
        cls: &typemap::Class,
        obj: &qmlast::UiObjectDefinition<'a>,
    ) -> quick_xml::Result<()> {
        SpacerItem::from_object_definition(cls, obj, self.doc.source(), &mut self.errors)
            .map(|w| w.serialize_to_xml(&mut self.writer))
            .unwrap_or(Ok(()))?;

        // action shouldn't have any children
        self.errors.extend(
            obj.child_object_nodes()
                .iter()
                .copied()
                .map(unexpected_node),
        );

        Ok(())
    }

    fn process_widget_definition(
        &mut self,
        cls: &typemap::Class,
        obj: &qmlast::UiObjectDefinition<'a>,
    ) -> quick_xml::Result<()> {
        Widget::from_object_definition(cls, obj, self.doc.source(), &mut self.errors)
            .map(|w| w.serialize_to_xml(&mut self.writer))
            .unwrap_or(Ok(()))?;

        let obj_tag = BytesStart::borrowed_name(b"widget");

        for &n in obj.child_object_nodes() {
            self.process_object_definition_node(n)?;
        }

        self.writer.write_event(Event::End(obj_tag.to_end()))?;
        Ok(())
    }

    pub fn errors(&self) -> &[qmlast::ParseError<'a>] {
        &self.errors
    }
}

/// Builds a list of sorted binding map pairs to stabilize the output.
fn collect_sorted_binding_pairs<'tree, 'source, 'a>(
    map: &'a qmlast::UiBindingMap<'tree, 'source>,
) -> Vec<(&'source str, &'a qmlast::UiBindingValue<'tree, 'source>)> {
    let mut pairs: Vec<_> = map.iter().map(|(&k, v)| (k, v)).collect();
    pairs.sort_by_key(|&(k, _)| k);
    pairs
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
