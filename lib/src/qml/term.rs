use super::astutil;
use super::{ParseError, ParseErrorKind};
use std::fmt;
use tree_sitter::{Node, TreeCursor};

/// Represents a primitive identifier.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier<'source>(&'source str);

impl<'source> Identifier<'source> {
    pub const fn new(s: &'source str) -> Self {
        Identifier(s)
    }

    pub fn from_node<'tree>(
        node: Node<'tree>,
        source: &'source str,
    ) -> Result<Self, ParseError<'tree>> {
        if node.kind() != "identifier" && node.kind() != "property_identifier" {
            return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
        }
        Ok(Self::new(astutil::node_text(node, source)))
    }

    /// Checks if this identifier looks like a type name.
    pub fn maybe_type_name(&self) -> bool {
        self.0.starts_with(char::is_uppercase)
    }

    pub fn as_str(&self) -> &str {
        self.0
    }
}

impl AsRef<str> for Identifier<'_> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl fmt::Display for Identifier<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Represents a (possibly nested) identifier.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct NestedIdentifier<'source> {
    // TODO: optimize with smallvec?
    components: Vec<Identifier<'source>>,
}

impl<'source> NestedIdentifier<'source> {
    // TODO: make it more capable?
    pub fn new(components: impl AsRef<[Identifier<'source>]>) -> Self {
        NestedIdentifier {
            components: components.as_ref().to_owned(),
        }
    }

    pub fn from_node<'tree>(
        node: Node<'tree>,
        source: &'source str,
    ) -> Result<Self, ParseError<'tree>> {
        Self::with_cursor(&mut node.walk(), source)
    }

    pub(crate) fn with_cursor<'tree>(
        cursor: &mut TreeCursor<'tree>,
        source: &'source str,
    ) -> Result<Self, ParseError<'tree>> {
        let mut depth: usize = 0;
        while cursor.node().kind() == "nested_identifier" {
            if !cursor.goto_first_child() {
                return Err(ParseError::new(
                    cursor.node(),
                    ParseErrorKind::InvalidSyntax,
                ));
            }
            astutil::skip_extras(cursor)?;
            depth += 1;
        }

        let components = if depth == 0 {
            // (identifier)
            vec![Identifier::from_node(cursor.node(), source)?]
        } else {
            // (nested_identifier (nested_identifier (identifier) (identifier)) (identifier))
            let mut components = Vec::with_capacity(depth + 1);
            while depth > 0 {
                let node = cursor.node();
                match node.kind() {
                    "identifier" => {
                        components.push(Identifier::new(astutil::node_text(node, source)));
                    }
                    // order matters: (ERROR) node is extra
                    _ if node.is_error() => {
                        return Err(ParseError::new(node, ParseErrorKind::InvalidSyntax));
                    }
                    _ if node.is_extra() || !node.is_named() => {}
                    _ => {
                        return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
                    }
                }
                while !cursor.goto_next_sibling() && depth > 0 {
                    let had_parent = cursor.goto_parent();
                    assert!(had_parent);
                    depth -= 1;
                }
            }
            components
        };

        Ok(NestedIdentifier { components })
    }

    pub fn components(&self) -> &[Identifier<'source>] {
        &self.components
    }

    /// Checks if this looks like an identifier prefixed with a type name.
    pub fn maybe_starts_with_type_name(&self) -> bool {
        self.components
            .first()
            .map(|p| p.maybe_type_name())
            .unwrap_or(false)
    }

    /// Splits this to type-name-a-like and the remainder parts.
    ///
    /// If either part is empty, returns None.
    pub fn split_type_name_prefix(&self) -> Option<(Self, Self)> {
        match self.components.iter().position(|p| !p.maybe_type_name()) {
            Some(n) if n > 0 => {
                let (type_parts, rem_parts) = self.components.split_at(n);
                debug_assert!(!type_parts.is_empty());
                debug_assert!(!rem_parts.is_empty());
                Some((
                    NestedIdentifier::new(type_parts),
                    NestedIdentifier::new(rem_parts),
                ))
            }
            _ => None,
        }
    }
}

impl<'source> From<&[&'source str]> for NestedIdentifier<'source> {
    fn from(parts: &[&'source str]) -> Self {
        let components = parts.iter().map(|&s| Identifier::new(s)).collect();
        NestedIdentifier { components }
    }
}

impl fmt::Display for NestedIdentifier<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some((head, tail)) = self.components.split_first() {
            write!(f, "{}", head)?;
            for s in tail {
                write!(f, ".{}", s)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::qml::UiDocument;

    fn extract_type_id(doc: &UiDocument) -> Result<NestedIdentifier, ParseError> {
        let node = doc
            .root_node()
            .child_by_field_name("root")
            .unwrap()
            .child_by_field_name("type_name")
            .unwrap();
        NestedIdentifier::with_cursor(&mut node.walk(), doc.source())
    }

    #[test]
    fn trivial_identifier() {
        let doc = UiDocument::with_source(r"Foo {}".to_owned());
        let id = extract_type_id(&doc).unwrap();
        assert_eq!(id.components(), ["Foo"].map(Identifier::new));
        assert_eq!(id.to_string(), "Foo");
    }

    #[test]
    fn nested_identifier() {
        let doc = UiDocument::with_source(r"Foo.Bar.Baz {}".to_owned());
        let id = extract_type_id(&doc).unwrap();
        assert_eq!(id.components(), ["Foo", "Bar", "Baz"].map(Identifier::new));
        assert_eq!(id.to_string(), "Foo.Bar.Baz");
    }

    #[test]
    fn nested_identifier_with_comments() {
        let doc = UiDocument::with_source(r"Foo. /*Bar.*/ Baz {}".to_owned());
        let id = extract_type_id(&doc).unwrap();
        assert_eq!(id.components(), ["Foo", "Baz"].map(Identifier::new));
        assert_eq!(id.to_string(), "Foo.Baz");
    }

    #[test]
    fn doubled_dots_in_identifier() {
        let doc = UiDocument::with_source(r"Foo..Bar {}".to_owned());
        assert!(extract_type_id(&doc).is_err());
    }
}
