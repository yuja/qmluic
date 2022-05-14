use super::expr::ConstantExpression;
use super::{XmlResult, XmlWriter};
use crate::qmlast::{
    Expression, Node, ParseError, ParseErrorKind, UiBindingMap, UiBindingValue, UiObjectDefinition,
};
use crate::typemap::Class;
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::io;

/// Action definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Action {
    pub name: Option<String>,
    pub properties: HashMap<String, ConstantExpression>,
}

impl Action {
    /// Generates action of `cls` type from the given `obj` definition.
    ///
    /// The given `cls` is supposed to be of `QAction` type.
    pub fn from_object_definition<'tree>(
        cls: &Class,
        obj: &UiObjectDefinition<'tree>,
        source: &str,
        diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
    ) -> Option<Self> {
        let binding_map = consume_err(diagnostics, obj.build_binding_map(source))?;
        Some(Action {
            name: obj.object_id().map(|n| n.to_str(source).to_owned()),
            properties: collect_properties(cls, &binding_map, source, diagnostics)?,
        })
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"action");
        if let Some(n) = &self.name {
            tag.push_attribute(("name", n.as_ref()));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        serialize_properties_to_xml(writer, &self.properties)?;

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Widget definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Widget {
    pub class: String,
    pub name: Option<String>,
    pub properties: HashMap<String, ConstantExpression>,
    pub actions: Vec<String>,
    //pub children: Vec<Object>,
}

impl Widget {
    /// Generates widget of `cls` type from the given `obj` definition.
    ///
    /// Child objects are NOT constructed recursively.
    pub fn from_object_definition<'tree>(
        cls: &Class,
        obj: &UiObjectDefinition<'tree>,
        source: &str,
        diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
    ) -> Option<Self> {
        let binding_map = consume_err(diagnostics, obj.build_binding_map(source))?;
        let properties_opt = collect_properties(cls, &binding_map, source, diagnostics);
        let actions_opt = binding_map
            .get("actions")
            .map(|v| collect_identifiers(v, source, diagnostics))
            .unwrap_or(Some(vec![]));

        // late return on error so as many diagnostics will be generated as possible
        Some(Widget {
            class: cls.name().to_owned(),
            name: obj.object_id().map(|n| n.to_str(source).to_owned()),
            properties: properties_opt?,
            actions: actions_opt?,
            //children: vec![]
        })
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"widget");
        tag.push_attribute(("class", self.class.as_ref()));
        if let Some(n) = &self.name {
            tag.push_attribute(("name", n.as_ref()));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        serialize_properties_to_xml(writer, &self.properties)?;

        for n in &self.actions {
            writer.write_event(Event::Empty(
                BytesStart::borrowed_name(b"addaction").with_attributes([("name", n.as_ref())]),
            ))?;
        }

        /*
        for c in &self.children {
            c.serialize_to_xml(writer)?;
        }

        writer.write_event(Event::End(tag.to_end()))?;
        */
        Ok(())
    }
}

fn collect_properties<'tree>(
    cls: &Class,
    binding_map: &UiBindingMap<'tree, '_>,
    source: &str,
    diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
) -> Option<HashMap<String, ConstantExpression>> {
    binding_map
        .iter()
        .filter(|(&name, _)| name != "actions") // TODO: only for QWidget subclasses
        .map(|(&name, value)| {
            if let Some(ty) = cls.get_property_type(name) {
                ConstantExpression::from_binding_value(&ty, value, source, diagnostics)
            } else {
                diagnostics.push(unexpected_node(value.node())); // TODO: unknown property/type
                None
            }
            .map(|v| (name.to_owned(), v))
        })
        .collect()
}

fn collect_identifiers<'tree>(
    value: &UiBindingValue<'tree, '_>,
    source: &str,
    diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
) -> Option<Vec<String>> {
    match value {
        UiBindingValue::Node(n) => parse_as_identifier_array(*n, source, diagnostics),
        UiBindingValue::Map(n, _) => {
            diagnostics.push(unexpected_node(*n));
            None
        }
    }
}

fn parse_as_identifier_string<'tree>(
    node: Node<'tree>,
    source: &str,
    diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
) -> Option<String> {
    match consume_err(diagnostics, Expression::from_node(node, source))? {
        Expression::Identifier(n) => Some(n.to_str(source).to_owned()),
        _ => {
            diagnostics.push(unexpected_node(node));
            None
        }
    }
}

fn parse_as_identifier_array<'tree>(
    node: Node<'tree>,
    source: &str,
    diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
) -> Option<Vec<String>> {
    match consume_err(diagnostics, Expression::from_node(node, source))? {
        Expression::Array(ns) => ns
            .iter()
            .map(|&n| parse_as_identifier_string(n, source, diagnostics))
            .collect(),
        _ => {
            diagnostics.push(unexpected_node(node));
            None
        }
    }
}

fn consume_err<'tree, T>(
    diagnostics: &mut Vec<ParseError<'tree>>, // TODO: diagnostic wrapper
    res: Result<T, ParseError<'tree>>,
) -> Option<T> {
    match res {
        Ok(x) => Some(x),
        Err(e) => {
            diagnostics.push(e);
            None
        }
    }
}

fn unexpected_node(node: Node) -> ParseError {
    ParseError::new(node, ParseErrorKind::UnexpectedNodeKind)
}

fn serialize_properties_to_xml<W>(
    writer: &mut XmlWriter<W>,
    properties: &HashMap<String, ConstantExpression>,
) -> XmlResult<()>
where
    W: io::Write,
{
    let mut pairs: Vec<_> = properties.iter().collect();
    pairs.sort_by_key(|&(k, _)| k);
    for (k, v) in pairs {
        let tag = BytesStart::borrowed_name(b"property").with_attributes([("name", k.as_ref())]);
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        v.serialize_to_xml(writer)?;
        writer.write_event(Event::End(tag.to_end()))?;
    }
    Ok(())
}
