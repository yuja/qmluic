//! Utility for error reporting.

use crate::qmlast::ParseError;
use std::ops::Range;
use std::slice;

/// Type (or level) of diagnostic message.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiagnosticKind {
    Error,
}

/// Diagnostic message.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    byte_range: Range<usize>,
    message: String,
}

impl Diagnostic {
    pub fn kind(&self) -> DiagnosticKind {
        self.kind
    }

    pub fn start_byte(&self) -> usize {
        self.byte_range.start
    }

    pub fn end_byte(&self) -> usize {
        self.byte_range.end
    }

    pub fn byte_range(&self) -> Range<usize> {
        Range {
            start: self.byte_range.start,
            end: self.byte_range.end,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl From<&ParseError<'_>> for Diagnostic {
    fn from(error: &ParseError) -> Self {
        Diagnostic {
            kind: DiagnosticKind::Error,
            byte_range: error.byte_range(),
            message: error.to_string(),
        }
    }
}

impl From<ParseError<'_>> for Diagnostic {
    fn from(error: ParseError) -> Self {
        Self::from(&error)
    }
}

/// Manages diagnostic messages.
#[derive(Clone, Debug, Default)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Diagnostics::default()
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }

    pub fn iter(&self) -> slice::Iter<'_, Diagnostic> {
        self.diagnostics.iter()
    }

    pub fn push<T>(&mut self, diag: T)
    where
        T: Into<Diagnostic>,
    {
        self.diagnostics.push(diag.into())
    }
}

impl<'a> IntoIterator for &'a Diagnostics {
    type Item = &'a Diagnostic;
    type IntoIter = slice::Iter<'a, Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> Extend<T> for Diagnostics
where
    T: Into<Diagnostic>,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        self.diagnostics.extend(iter.into_iter().map(|d| d.into()))
    }
}
