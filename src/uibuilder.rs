use qmluic::qmlast;
use qmluic::typemap::{self, TypeMap};
use qmluic::uigen::{ConstantGadget, ConstantValue};
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
        let mut obj_tag = BytesStart::borrowed_name(b"action");
        if let Some(id) = obj.object_id() {
            obj_tag.push_attribute(("name", id.to_str(self.doc.source())));
        }
        self.writer
            .write_event(Event::Start(obj_tag.to_borrowed()))?;

        self.process_binding_map(cls, obj)?;

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
        cls: &typemap::Class,
        obj: &qmlast::UiObjectDefinition<'a>,
    ) -> quick_xml::Result<()> {
        let mut obj_tag = BytesStart::borrowed_name(b"layout");
        obj_tag.push_attribute(("class", cls.name()));
        if let Some(id) = obj.object_id() {
            obj_tag.push_attribute(("name", id.to_str(self.doc.source())));
        }
        self.writer
            .write_event(Event::Start(obj_tag.to_borrowed()))?;

        self.process_binding_map(cls, obj)?;

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
                        qmlast::UiBindingValue::Node(n) => match eval_number(*n, self.doc.source())
                        {
                            Ok(d) => item_tag.push_attribute((name, d.to_string().as_ref())),
                            Err(e) => self.errors.push(e),
                        },
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
        let mut obj_tag = BytesStart::borrowed_name(b"spacer");
        if let Some(id) = obj.object_id() {
            obj_tag.push_attribute(("name", id.to_str(self.doc.source())));
        }
        self.writer
            .write_event(Event::Start(obj_tag.to_borrowed()))?;

        self.process_binding_map(cls, obj)?;

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
        cls: &typemap::Class,
        obj: &qmlast::UiObjectDefinition<'a>,
    ) -> quick_xml::Result<()> {
        let mut obj_tag = BytesStart::borrowed_name(b"widget");
        obj_tag.push_attribute(("class", cls.name()));
        if let Some(id) = obj.object_id() {
            obj_tag.push_attribute(("name", id.to_str(self.doc.source())));
        }
        self.writer
            .write_event(Event::Start(obj_tag.to_borrowed()))?;

        self.process_binding_map(cls, obj)?;

        for &n in obj.child_object_nodes() {
            self.process_object_definition_node(n)?;
        }

        self.writer.write_event(Event::End(obj_tag.to_end()))?;
        Ok(())
    }

    fn process_binding_map(
        &mut self,
        cls: &typemap::Class,
        obj: &qmlast::UiObjectDefinition<'a>,
    ) -> quick_xml::Result<()> {
        let map = match obj.build_binding_map(self.doc.source()) {
            Ok(x) => x,
            Err(e) => {
                self.errors.push(e);
                return Ok(());
            }
        };
        for (name, value) in collect_sorted_binding_pairs(&map) {
            if name == "actions" {
                // TODO: only for QWidget subclasses
                match value {
                    qmlast::UiBindingValue::Node(n) => {
                        self.process_binding_action_value_node(*n)?
                    }
                    qmlast::UiBindingValue::Map(n, _) => self.errors.push(unexpected_node(*n)),
                }
            } else if let Some(ty) = cls.get_property_type(name) {
                let prop_tag =
                    BytesStart::borrowed_name(b"property").with_attributes([("name", name)]);
                self.writer
                    .write_event(Event::Start(prop_tag.to_borrowed()))?;

                match value {
                    qmlast::UiBindingValue::Node(n) => {
                        self.process_binding_value_node(&ty, *n)?;
                    }
                    qmlast::UiBindingValue::Map(n, m) => {
                        self.process_binding_grouped_value(&ty, *n, m)?
                    }
                }

                self.writer.write_event(Event::End(prop_tag.to_end()))?;
            } else {
                self.errors.push(unexpected_node(value.node())); // TODO: unknown property/type
            }
        }
        Ok(()) // TODO
    }

    fn process_binding_value_node(
        &mut self,
        ty: &typemap::Type,
        node: qmlast::Node<'a>,
    ) -> quick_xml::Result<()> {
        ConstantValue::from_expression(ty, node, self.doc.source(), &mut self.errors)
            .map(|v| v.serialize_to_xml(&mut self.writer))
            .unwrap_or(Ok(()))
    }

    fn process_binding_grouped_value(
        &mut self,
        ty: &typemap::Type,
        node: qmlast::Node<'a>,
        map: &qmlast::UiBindingMap<'a, 'a>,
    ) -> quick_xml::Result<()> {
        let cls = match ty {
            typemap::Type::Class(x) => x,
            _ => {
                self.errors.push(unexpected_node(node));
                return Ok(());
            }
        };
        match cls.name() {
            "QRect" | "QSize" => ConstantGadget::from_binding_map(
                cls,
                node,
                map,
                self.doc.source(),
                &mut self.errors,
            )
            .map(|g| g.serialize_to_xml(&mut self.writer))
            .unwrap_or(Ok(()))?,
            "QSizePolicy" => {
                // TODO: QSizePolicy is special in that
                // a) it is registered as gadget type but have no useful property,
                // b) .ui XML attribute names are diverged from method/property names.
                match SizePolicy::from_binding_map(node, map, self.doc.source()) {
                    Ok(x) => x.write_ui_xml(&mut self.writer)?,
                    Err(e) => self.errors.push(e),
                }
            }
            _ => self.errors.push(unexpected_node(node)),
        };
        Ok(())
    }

    fn process_binding_action_value_node(
        &mut self,
        node: qmlast::Node<'a>,
    ) -> quick_xml::Result<()> {
        match parse_as_identifier_array(node, self.doc.source()) {
            Ok(names) => {
                for n in names {
                    let tag = BytesStart::borrowed_name(b"addaction")
                        .with_attributes([("name", n.to_str(self.doc.source()))]);
                    self.writer.write_event(Event::Empty(tag))?;
                }
            }
            Err(e) => self.errors.push(e),
        };
        Ok(())
    }

    pub fn errors(&self) -> &[qmlast::ParseError<'a>] {
        &self.errors
    }
}

fn format_as_size_policy<'tree>(
    node: qmlast::Node<'tree>,
    source: &str,
) -> Result<String, qmlast::ParseError<'tree>> {
    let s = format_as_nested_identifier(node, source)?;
    s.strip_prefix("QSizePolicy::")
        .map(str::to_owned)
        .ok_or_else(|| unexpected_node(node))
}

fn format_as_nested_identifier<'tree>(
    node: qmlast::Node<'tree>,
    source: &str,
) -> Result<String, qmlast::ParseError<'tree>> {
    match qmlast::Expression::from_node(node, source)? {
        qmlast::Expression::Identifier(n) => Ok(n.to_str(source).to_owned()),
        qmlast::Expression::MemberExpression(x) => {
            let object = format_as_nested_identifier(x.object, source)?;
            Ok(format!("{}::{}", object, x.property.to_str(source)))
        }
        _ => Err(unexpected_node(node)),
    }
}

fn parse_as_identifier<'tree>(
    node: qmlast::Node<'tree>,
    source: &str,
) -> Result<qmlast::Identifier<'tree>, qmlast::ParseError<'tree>> {
    match qmlast::Expression::from_node(node, source)? {
        qmlast::Expression::Identifier(n) => Ok(n),
        _ => Err(unexpected_node(node)),
    }
}

fn parse_as_identifier_array<'tree>(
    node: qmlast::Node<'tree>,
    source: &str,
) -> Result<Vec<qmlast::Identifier<'tree>>, qmlast::ParseError<'tree>> {
    match qmlast::Expression::from_node(node, source)? {
        qmlast::Expression::Array(ns) => {
            ns.iter().map(|&n| parse_as_identifier(n, source)).collect()
        }
        _ => Err(unexpected_node(node)),
    }
}

fn eval_number<'tree>(
    node: qmlast::Node<'tree>,
    source: &str,
) -> Result<f64, qmlast::ParseError<'tree>> {
    // TODO: handle overflow, etc.
    let v = match qmlast::Expression::from_node(node, source)? {
        qmlast::Expression::Number(v) => v,
        qmlast::Expression::UnaryExpression(x) => {
            let arg = eval_number(x.argument, source);
            match x.operator {
                qmlast::UnaryOperator::Minus => -arg?,
                qmlast::UnaryOperator::Plus => arg?,
                // TODO: ...
                _ => return Err(unexpected_node(node)),
            }
        }
        qmlast::Expression::BinaryExpression(x) => {
            let left = eval_number(x.left, source);
            let right = eval_number(x.right, source);
            match x.operator {
                qmlast::BinaryOperator::Add => left? + right?,
                qmlast::BinaryOperator::Sub => left? - right?,
                qmlast::BinaryOperator::Mul => left? * right?,
                qmlast::BinaryOperator::Div => left? / right?,
                qmlast::BinaryOperator::Exp => left?.powf(right?),
                // TODO: ...
                _ => return Err(unexpected_node(node)),
            }
        }
        _ => return Err(unexpected_node(node)),
    };
    Ok(v)
}

struct SizePolicy {
    pub horizontal_policy: String,
    pub vertical_policy: String,
    pub horizontal_stretch: f64,
    pub vertical_stretch: f64,
}

impl SizePolicy {
    fn from_binding_map<'tree>(
        node: qmlast::Node<'tree>,
        map: &qmlast::UiBindingMap<'tree, '_>,
        source: &str,
    ) -> Result<Self, qmlast::ParseError<'tree>> {
        if let (
            Some(qmlast::UiBindingValue::Node(hpol)),
            Some(qmlast::UiBindingValue::Node(vpol)),
            Some(qmlast::UiBindingValue::Node(hstretch)),
            Some(qmlast::UiBindingValue::Node(vstretch)),
        ) = (
            map.get("horizontalPolicy"),
            map.get("verticalPolicy"),
            map.get("horizontalStretch"),
            map.get("verticalStretch"),
        ) {
            let policy = SizePolicy {
                horizontal_policy: format_as_size_policy(*hpol, source)?,
                vertical_policy: format_as_size_policy(*vpol, source)?,
                horizontal_stretch: eval_number(*hstretch, source)?,
                vertical_stretch: eval_number(*vstretch, source)?,
            };
            Ok(policy)
        } else {
            Err(unexpected_node(node))
        }
    }

    fn write_ui_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> quick_xml::Result<()>
    where
        W: io::Write,
    {
        let tag = BytesStart::borrowed_name(b"sizepolicy").with_attributes([
            ("hsizetype", self.horizontal_policy.as_ref()),
            ("vsizetype", self.vertical_policy.as_ref()),
        ]);
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        write_tagged_str(writer, b"horstretch", &self.horizontal_stretch.to_string())?;
        write_tagged_str(writer, b"verstretch", &self.vertical_stretch.to_string())?;

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
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
