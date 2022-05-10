//! Utility for AST parsing and building.

use super::{ParseError, ParseErrorKind};
use tree_sitter::{Node, TreeCursor};

pub(crate) fn get_child_by_field_name<'tree>(
    node: Node<'tree>,
    name: &'static str,
) -> Result<Node<'tree>, ParseError<'tree>> {
    node.child_by_field_name(name)
        .ok_or_else(|| ParseError::new(node, ParseErrorKind::MissingField(name)))
}

pub(crate) fn node_text<'tree, 'source>(node: Node<'tree>, source: &'source str) -> &'source str {
    node.utf8_text(source.as_bytes())
        .expect("source range must be valid utf-8 string")
}

pub(crate) fn skip_until_named<'tree>(
    cursor: &mut TreeCursor<'tree>,
) -> Result<(), ParseError<'tree>> {
    while cursor.node().is_extra() || !cursor.node().is_named() {
        let node = cursor.node();
        if !cursor.goto_next_sibling() {
            return Err(ParseError::new(node, ParseErrorKind::InvalidSyntax));
        }
    }
    Ok(())
}

pub(crate) fn parse_number<'tree, 'source>(
    node: Node<'tree>,
    source: &'source str,
) -> Result<f64, ParseError<'tree>> {
    if node.kind() != "number" {
        return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
    }
    parse_number_str(node_text(node, source))
        .ok_or_else(|| ParseError::new(node, ParseErrorKind::InvalidSyntax))
}

fn parse_number_str(s: &str) -> Option<f64> {
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#numeric_literals
    // TODO: maybe incomplete
    if let Some((radix, t)) = strip_radix_prefix(s) {
        u32::from_str_radix(t, radix)
            .or_else(|_| {
                let cleaned: String = t.chars().filter(|&c| c != '_').collect();
                u32::from_str_radix(&cleaned, radix)
            })
            .ok()
            .map(|n| n.into())
    } else {
        s.parse().ok()
    }
}

fn strip_radix_prefix(s: &str) -> Option<(u32, &str)> {
    if s.starts_with("0b") || s.starts_with("0B") {
        Some((2, &s[2..]))
    } else if s.starts_with("0o") || s.starts_with("0O") {
        Some((8, &s[2..]))
    } else if s.starts_with("0x") || s.starts_with("0X") {
        Some((16, &s[2..]))
    } else if s.starts_with('0') && s.len() > 1 && s.chars().all(|c| ('0'..='7').contains(&c)) {
        Some((8, &s[1..]))
    } else {
        None
    }
}

pub(crate) fn parse_string<'tree, 'source>(
    node: Node<'tree>,
    source: &'source str,
) -> Result<String, ParseError<'tree>> {
    if node.kind() != "string" {
        return Err(ParseError::new(node, ParseErrorKind::UnexpectedNodeKind));
    }
    let mut decoded = String::with_capacity(node.byte_range().len());
    for n in node.named_children(&mut node.walk()) {
        let s = node_text(n, source);
        match n.kind() {
            "string_fragment" => decoded.push_str(s),
            "escape_sequence" => {
                let c = unescape_char(s)
                    .ok_or_else(|| ParseError::new(n, ParseErrorKind::InvalidSyntax))?;
                decoded.push(c);
            }
            _ => return Err(ParseError::new(n, ParseErrorKind::UnexpectedNodeKind)),
        }
    }
    Ok(decoded)
}

fn unescape_char(escaped: &str) -> Option<char> {
    if !escaped.starts_with('\\') {
        return None;
    }

    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#escape_sequences
    let tail = &escaped[1..];
    if tail.len() == 1 {
        match tail.chars().next().unwrap() {
            '0' => Some('\0'),
            '\'' => Some('\''),
            '\"' => Some('\"'),
            '\\' => Some('\\'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            'v' => Some('\x0b'),
            't' => Some('\t'),
            'b' => Some('\x08'),
            'f' => Some('\x0c'),
            _ => None,
        }
    } else if tail.starts_with("u{") && tail.ends_with('}') {
        char_from_str_radix(&tail[2..tail.len() - 1], 16)
    } else if tail.starts_with('u') && tail.len() == 5 {
        char_from_str_radix(&tail[1..], 16)
    } else if tail.starts_with('x') && tail.len() == 3 {
        char_from_str_radix(&tail[1..], 16)
    } else {
        None
    }
}

fn char_from_str_radix(src: &str, radix: u32) -> Option<char> {
    u32::from_str_radix(src, radix)
        .ok()
        .and_then(char::from_u32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number_literal() {
        assert_eq!(parse_number_str("0"), Some(0.));
        assert_eq!(parse_number_str("123"), Some(123.));
        assert_eq!(parse_number_str("01234567"), Some(0o1234567 as f64));
        assert_eq!(parse_number_str("0o123"), Some(0o123 as f64));
        assert_eq!(parse_number_str("0Xdead"), Some(0xdead as f64));
        assert_eq!(parse_number_str("0e-1"), Some(0.));
        assert_eq!(parse_number_str("0.8"), Some(0.8));
        assert_eq!(parse_number_str("0b0101_1010"), Some(0b0101_1010 as f64));
    }

    #[test]
    fn escape_sequence() {
        assert_eq!(unescape_char(r"\n"), Some('\n'));
        assert_eq!(unescape_char(r"\\"), Some('\\'));
        assert_eq!(unescape_char(r"\0"), Some('\0'));
        assert_eq!(unescape_char(r"\x7f"), Some('\x7f'));
        assert_eq!(unescape_char(r"\u300f"), Some('\u{300f}'));
        assert_eq!(unescape_char(r"\u{12f}"), Some('\u{12f}'));
    }
}
