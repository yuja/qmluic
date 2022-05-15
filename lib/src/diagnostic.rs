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
    /// Creates new diagnostic message of the given `kind`.
    pub fn new<S>(kind: DiagnosticKind, byte_range: Range<usize>, message: S) -> Self
    where
        S: Into<String>,
    {
        Diagnostic {
            kind,
            byte_range,
            message: message.into(),
        }
    }

    /// Creates new error message.
    pub fn error<S>(byte_range: Range<usize>, message: S) -> Self
    where
        S: Into<String>,
    {
        Self::new(DiagnosticKind::Error, byte_range, message)
    }

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
        Self::error(error.byte_range(), error.to_string())
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

    /// Extracts error from the given `result` and pushes it. Returns the success value if any.
    pub fn consume_err<T, E>(&mut self, result: Result<T, E>) -> Option<T>
    where
        E: Into<Diagnostic>,
    {
        match result {
            Ok(x) => Some(x),
            Err(e) => {
                self.push(e);
                None
            }
        }
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
