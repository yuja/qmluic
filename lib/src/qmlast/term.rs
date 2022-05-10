use super::astutil;
use super::{ParseError, ParseErrorKind};
use std::borrow::Cow;
use tree_sitter::{Node, TreeCursor};

/// Represents a primitive identifier.
#[derive(Clone, Copy, Debug)]
pub struct Identifier<'tree>(Node<'tree>);

impl<'tree> Identifier<'tree> {
    pub fn from_node(node: Node<'tree>) -> Result<Self, ParseError<'tree>> {
        if node.kind() != "identifier" && node.kind() != "property_identifier" {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }
        Ok(Identifier(node))
    }

    pub fn node(&self) -> Node<'tree> {
        self.0
    }

    /// Checks if this identifier looks like a type name.
    pub fn maybe_type_name(&self, source: &str) -> bool {
        self.to_str(source).starts_with(char::is_uppercase)
    }

    pub fn to_str<'s>(&self, source: &'s str) -> &'s str {
        astutil::node_text(self.0, source)
    }
}

/// Represents a (possibly nested) identifier.
#[derive(Clone, Debug)]
pub struct NestedIdentifier<'tree> {
    // TODO: optimize with smallvec or store the nested_identifier node?
    components: Vec<Identifier<'tree>>,
}

impl<'tree> NestedIdentifier<'tree> {
    pub fn from_node(node: Node<'tree>) -> Result<Self, ParseError<'tree>> {
        Self::with_cursor(&mut node.walk())
    }

    pub(crate) fn with_cursor(cursor: &mut TreeCursor<'tree>) -> Result<Self, ParseError<'tree>> {
        let mut depth: usize = 0;
        while cursor.node().kind() == "nested_identifier" {
            if !cursor.goto_first_child() {
                return Err(ParseError::new(
                    cursor.node(),
                    ParseErrorKind::InvalidSyntax,
                ));
            }
            astutil::skip_until_named(cursor)?;
            depth += 1;
        }

        let components = if depth == 0 {
            // (identifier)
            vec![Identifier::from_node(cursor.node())?]
        } else {
            // (nested_identifier (nested_identifier (identifier) (identifier)) (identifier))
            let mut components = Vec::with_capacity(depth + 1);
            'pop: loop {
                let node = cursor.node();
                match node.kind() {
                    "identifier" => {
                        components.push(Identifier(node));
                    }
                    _ => astutil::handle_uninteresting_node(node)?,
                }
                while !cursor.goto_next_sibling() {
                    if !cursor.goto_parent() {
                        break 'pop;
                    }
                }
            }
            components
        };

        Ok(NestedIdentifier { components })
    }

    pub fn components(&self) -> &[Identifier<'tree>] {
        &self.components
    }

    /// Checks if this looks like an identifier prefixed with a type name.
    pub fn maybe_starts_with_type_name(&self, source: &str) -> bool {
        self.components
            .first()
            .map(|p| p.maybe_type_name(source))
            .unwrap_or(false)
    }

    /// Splits this to type-name-a-like and the remainder parts.
    ///
    /// If either part is empty, returns None.
    pub fn split_type_name_prefix(&self, source: &str) -> Option<(Self, Self)> {
        match self
            .components
            .iter()
            .position(|p| !p.maybe_type_name(source))
        {
            Some(n) if n > 0 => {
                let (type_parts, rem_parts) = self.components.split_at(n);
                debug_assert!(!type_parts.is_empty());
                debug_assert!(!rem_parts.is_empty());
                Some((
                    NestedIdentifier {
                        components: type_parts.to_owned(),
                    },
                    NestedIdentifier {
                        components: rem_parts.to_owned(),
                    },
                ))
            }
            _ => None,
        }
    }

    pub fn to_string<'s>(&self, source: &'s str) -> Cow<'s, str> {
        if self.components.len() == 1 {
            Cow::Borrowed(self.components[0].to_str(source))
        } else {
            let dotted_path = self
                .components
                .iter()
                .map(|p| p.to_str(source))
                .collect::<Vec<_>>()
                .join(".");
            Cow::Owned(dotted_path)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::qmlast::UiDocument;

    fn parse(source: &str) -> UiDocument {
        UiDocument::parse(source.to_owned(), None)
    }

    fn extract_type_id(doc: &UiDocument) -> Result<NestedIdentifier, ParseError> {
        let node = doc
            .root_node()
            .child_by_field_name("root")
            .unwrap()
            .child_by_field_name("type_name")
            .unwrap();
        NestedIdentifier::with_cursor(&mut node.walk())
    }

    #[test]
    fn trivial_identifier() {
        let doc = parse(r"Foo {}");
        let id = extract_type_id(&doc).unwrap();
        assert_eq!(id.components().len(), 1);
        assert_eq!(id.to_string(doc.source()), "Foo");
    }

    #[test]
    fn nested_identifier() {
        let doc = parse(r"Foo.Bar.Baz {}");
        let id = extract_type_id(&doc).unwrap();
        assert_eq!(id.components().len(), 3);
        assert_eq!(id.to_string(doc.source()), "Foo.Bar.Baz");
    }

    #[test]
    fn nested_identifier_with_comments() {
        let doc = parse(r"Foo. /*Bar.*/ Baz {}");
        let id = extract_type_id(&doc).unwrap();
        assert_eq!(id.components().len(), 2);
        assert_eq!(id.to_string(doc.source()), "Foo.Baz");
    }

    #[test]
    fn doubled_dots_in_identifier() {
        // this is syntax error, but recovered as Foo./* something bad */Bar.
        let doc = parse(r"Foo..Bar {}");
        let id = extract_type_id(&doc).unwrap();
        assert_eq!(id.to_string(doc.source()), "Foo.Bar");
    }
}
