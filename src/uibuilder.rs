use qmluic::qml;
use quick_xml::events::{BytesStart, BytesText, Event};
use std::io;

pub struct UiBuilder<'doc, W>
where
    W: io::Write,
{
    writer: quick_xml::Writer<W>,
    doc: &'doc qml::UiDocument,
    class_name: String, // TODO: maybe UiDocument should have the name?
    errors: Vec<qml::ParseError<'doc>>,
}

impl<'doc, W> UiBuilder<'doc, W>
where
    W: io::Write,
{
    pub fn new(dest: W, doc: &'doc qml::UiDocument, class_name: impl AsRef<str>) -> Self {
        let writer = quick_xml::Writer::new_with_indent(dest, b' ', 1);
        UiBuilder {
            writer,
            doc,
            class_name: class_name.as_ref().to_owned(),
            errors: Vec::new(),
        }
    }

    pub fn build(&mut self) -> quick_xml::Result<()> {
        let ui_tag = BytesStart::borrowed_name(b"ui")
            .with_attributes([(b"version".as_ref(), b"4.0".as_ref())]);
        self.writer
            .write_event(Event::Start(ui_tag.to_borrowed()))?;
        write_tagged_str(&mut self.writer, b"class", &self.class_name)?;
        self.process_program_node(self.doc.root_node())?;
        self.writer.write_event(Event::End(ui_tag.to_end()))?;
        self.writer.write(b"\n")?;
        Ok(())
    }

    fn process_program_node(&mut self, node: qml::Node<'doc>) -> quick_xml::Result<()> {
        match qml::UiProgram::from_node(node, self.doc.source()) {
            Ok(x) => self.process_object_definition_node(x.root_object_node())?,
            Err(e) => self.errors.push(e),
        };
        Ok(())
    }

    fn process_object_definition_node(&mut self, node: qml::Node<'doc>) -> quick_xml::Result<()> {
        match qml::UiObjectDefinition::from_node(node, self.doc.source()) {
            Ok(x) => self.process_object_definition(&x)?,
            Err(e) => self.errors.push(e),
        };
        Ok(())
    }

    fn process_object_definition(
        &mut self,
        obj: &qml::UiObjectDefinition<'doc, 'doc>,
    ) -> quick_xml::Result<()> {
        // TODO: resolve against imported types
        let type_name = obj.type_name().to_string();
        if type_name == "QAction" {
            self.process_action_definition(obj)
        } else if type_name.ends_with("Layout") {
            self.process_layout_definition(&type_name, obj)
        } else {
            self.process_widget_definition(&type_name, obj)
        }
    }

    fn process_action_definition(
        &mut self,
        obj: &qml::UiObjectDefinition<'doc, 'doc>,
    ) -> quick_xml::Result<()> {
        let mut obj_tag = BytesStart::borrowed_name(b"action");
        if let Some(id) = obj.object_id() {
            obj_tag.push_attribute(("name", id.as_str()));
        }
        self.writer
            .write_event(Event::Start(obj_tag.to_borrowed()))?;

        self.process_binding_map(obj.binding_map())?;

        // action shouldn't have any children
        self.errors.extend(
            obj.child_object_nodes()
                .iter()
                .copied()
                .map(unexpected_node),
        );

        self.writer.write_event(Event::End(obj_tag.to_end()))?;
        Ok(())
    }

    fn process_layout_definition(
        &mut self,
        type_name: &str,
        obj: &qml::UiObjectDefinition<'doc, 'doc>,
    ) -> quick_xml::Result<()> {
        let mut obj_tag = BytesStart::borrowed_name(b"layout");
        obj_tag.push_attribute(("class", type_name.as_ref()));
        if let Some(id) = obj.object_id() {
            obj_tag.push_attribute(("name", id.as_str()));
        }
        self.writer
            .write_event(Event::Start(obj_tag.to_borrowed()))?;

        self.process_binding_map(obj.binding_map())?;

        for &n in obj.child_object_nodes() {
            let child = match qml::UiObjectDefinition::from_node(n, self.doc.source()) {
                Ok(x) => x,
                Err(e) => {
                    self.errors.push(e);
                    continue;
                }
            };

            let mut item_tag = BytesStart::borrowed_name(b"item");
            // TODO: resolve against imported types
            if let Some(map) = child
                .attached_type_map()
                .get(&qml::NestedIdentifier::from(["QLayoutItem"].as_ref()))
            {
                for (name, value) in collect_sorted_binding_pairs(map) {
                    // TODO: needs name_node.parent() for error reporting
                    match format_constant_binding_value(n, value, self.doc.source()) {
                        Ok((_, s)) => item_tag.push_attribute((name.as_str(), s.as_ref())),
                        Err(e) => self.errors.push(e),
                    }
                }
            }
            self.writer
                .write_event(Event::Start(item_tag.to_borrowed()))?;

            // TODO: resolve against imported types
            if child.type_name().to_string() == "QSpacerItem" {
                self.process_spacer_definition(&child)?;
            } else {
                self.process_object_definition(&child)?;
            }

            self.writer.write_event(Event::End(item_tag.to_end()))?;
        }

        self.writer.write_event(Event::End(obj_tag.to_end()))?;
        Ok(())
    }

    fn process_spacer_definition(
        &mut self,
        obj: &qml::UiObjectDefinition<'doc, 'doc>,
    ) -> quick_xml::Result<()> {
        let mut obj_tag = BytesStart::borrowed_name(b"spacer");
        if let Some(id) = obj.object_id() {
            obj_tag.push_attribute(("name", id.as_str()));
        }
        self.writer
            .write_event(Event::Start(obj_tag.to_borrowed()))?;

        self.process_binding_map(obj.binding_map())?;

        // action shouldn't have any children
        self.errors.extend(
            obj.child_object_nodes()
                .iter()
                .copied()
                .map(unexpected_node),
        );

        self.writer.write_event(Event::End(obj_tag.to_end()))?;
        Ok(())
    }

    fn process_widget_definition(
        &mut self,
        type_name: &str,
        obj: &qml::UiObjectDefinition<'doc, 'doc>,
    ) -> quick_xml::Result<()> {
        let mut obj_tag = BytesStart::borrowed_name(b"widget");
        obj_tag.push_attribute(("class", type_name.as_ref()));
        if let Some(id) = obj.object_id() {
            obj_tag.push_attribute(("name", id.as_str()));
        }
        self.writer
            .write_event(Event::Start(obj_tag.to_borrowed()))?;

        self.process_binding_map(obj.binding_map())?;

        for &n in obj.child_object_nodes() {
            self.process_object_definition_node(n)?;
        }

        self.writer.write_event(Event::End(obj_tag.to_end()))?;
        Ok(())
    }

    fn process_binding_map(
        &mut self,
        map: &qml::UiBindingMap<'doc, 'doc>,
    ) -> quick_xml::Result<()> {
        for (name, value) in collect_sorted_binding_pairs(map) {
            let prop_tag =
                BytesStart::borrowed_name(b"property").with_attributes([("name", name.as_str())]);
            self.writer
                .write_event(Event::Start(prop_tag.to_borrowed()))?;

            // TODO: use type provided by .qmltypes instead of guessing it
            match value {
                qml::UiBindingValue::Node(n) => self.process_binding_value_node(*n)?,
                qml::UiBindingValue::Map(m) => {
                    // TODO: size, sizepolicy, rect, etc.
                }
            }

            self.writer.write_event(Event::End(prop_tag.to_end()))?;
        }
        Ok(()) // TODO
    }

    fn process_binding_value_node(&mut self, node: qml::Node<'doc>) -> quick_xml::Result<()> {
        match format_constant_expression(node, self.doc.source()) {
            Ok((kind, formatted)) => write_tagged_str(&mut self.writer, kind, &formatted)?,
            Err(e) => self.errors.push(e),
        };
        Ok(())
    }

    pub fn errors(&self) -> &[qml::ParseError<'doc>] {
        &self.errors
    }
}

fn format_constant_expression<'tree, 'source>(
    node: qml::Node<'tree>,
    source: &'source str,
) -> Result<(&'static [u8], String), qml::ParseError<'tree>> {
    let (kind, formatted) = match qml::Expression::from_node(node, source)? {
        qml::Expression::Number(v) => (b"number".as_ref(), v.to_string()),
        qml::Expression::String(s) => (b"string".as_ref(), s),
        qml::Expression::Bool(b) => (
            b"bool".as_ref(),
            if b {
                "true".to_owned()
            } else {
                "false".to_owned()
            },
        ),
        // TODO: enum (but could be set): qml::Expression::MemberExpression(x)
        qml::Expression::CallExpression(x) => {
            match parse_as_identifier(x.function, source)?.as_str() {
                "qsTr" if x.arguments.len() == 1 => {
                    (b"string".as_ref(), parse_as_string(x.arguments[0], source)?)
                }
                _ => return Err(unexpected_node(node)),
            }
        }
        qml::Expression::UnaryExpression(_) => {
            let v = eval_number(node, source)?; // handle -num
            (b"number".as_ref(), v.to_string())
        }
        // TODO: handle set: qml::Expression::BinaryExpression(x)
        _ => return Err(unexpected_node(node)),
    };
    Ok((kind, formatted))
}

fn format_constant_binding_value<'tree, 'source>(
    key_node: qml::Node<'tree>,
    value: &qml::UiBindingValue<'tree, 'source>,
    source: &'source str,
) -> Result<(&'static [u8], String), qml::ParseError<'tree>> {
    match value {
        qml::UiBindingValue::Node(n) => format_constant_expression(*n, source),
        qml::UiBindingValue::Map(_) => Err(unexpected_node(key_node)),
    }
}

fn parse_as_identifier<'tree, 'source>(
    node: qml::Node<'tree>,
    source: &'source str,
) -> Result<qml::Identifier<'source>, qml::ParseError<'tree>> {
    match qml::Expression::from_node(node, source)? {
        qml::Expression::Identifier(n) => Ok(n),
        _ => Err(unexpected_node(node)),
    }
}

fn parse_as_string<'tree, 'source>(
    node: qml::Node<'tree>,
    source: &'source str,
) -> Result<String, qml::ParseError<'tree>> {
    match qml::Expression::from_node(node, source)? {
        qml::Expression::String(s) => Ok(s),
        _ => Err(unexpected_node(node)),
    }
}

fn eval_number<'tree, 'source>(
    node: qml::Node<'tree>,
    source: &'source str,
) -> Result<f64, qml::ParseError<'tree>> {
    // TODO: handle overflow, etc.
    let v = match qml::Expression::from_node(node, source)? {
        qml::Expression::Number(v) => v,
        qml::Expression::UnaryExpression(x) => {
            let arg = eval_number(x.argument, source);
            match x.operator {
                qml::UnaryOperator::Minus => -arg?,
                qml::UnaryOperator::Plus => arg?,
                // TODO: ...
                _ => return Err(unexpected_node(node)),
            }
        }
        qml::Expression::BinaryExpression(x) => {
            let left = eval_number(x.left, source);
            let right = eval_number(x.right, source);
            match x.operator {
                qml::BinaryOperator::Add => left? + right?,
                qml::BinaryOperator::Sub => left? - right?,
                qml::BinaryOperator::Mul => left? * right?,
                qml::BinaryOperator::Div => left? / right?,
                qml::BinaryOperator::Exp => left?.powf(right?),
                // TODO: ...
                _ => return Err(unexpected_node(node)),
            }
        }
        _ => return Err(unexpected_node(node)),
    };
    Ok(v)
}

/// Builds a list of sorted binding map pairs to stabilize the output.
fn collect_sorted_binding_pairs<'tree, 'source, 'a>(
    map: &'a qml::UiBindingMap<'tree, 'source>,
) -> Vec<(
    &'a qml::Identifier<'source>,
    &'a qml::UiBindingValue<'tree, 'source>,
)> {
    let mut pairs: Vec<_> = map.iter().collect();
    pairs.sort_by_key(|(&k, _)| k);
    pairs
}

fn unexpected_node<'tree>(node: qml::Node<'tree>) -> qml::ParseError<'tree> {
    qml::ParseError::new(node, qml::ParseErrorKind::UnexpectedNodeKind)
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
