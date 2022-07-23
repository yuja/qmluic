//! Utility for error reporting.

use crate::qmlast::ParseError;
use camino::{Utf8Path, Utf8PathBuf};
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
    labels: Vec<(Range<usize>, String)>,
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
            labels: Vec::new(),
        }
    }

    /// Creates new error message.
    pub fn error<S>(byte_range: Range<usize>, message: S) -> Self
    where
        S: Into<String>,
    {
        Self::new(DiagnosticKind::Error, byte_range, message)
    }

    /// Builds diagnostic with the given label attached.
    pub fn with_label<S>(mut self, byte_range: Range<usize>, message: S) -> Self
    where
        S: Into<String>,
    {
        self.push_label(byte_range, message);
        self
    }

    /// Builds diagnostic with the given labels attached.
    pub fn with_labels<I, S>(mut self, labels: I) -> Self
    where
        I: IntoIterator<Item = (Range<usize>, S)>,
        S: Into<String>,
    {
        self.extend_labels(labels);
        self
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

    pub fn labels(&self) -> &[(Range<usize>, String)] {
        &self.labels
    }

    /// Add a label to this diagnostic.
    pub fn push_label<S>(&mut self, byte_range: Range<usize>, message: S)
    where
        S: Into<String>,
    {
        self.labels.push((byte_range, message.into()));
    }

    /// Add labels to this diagnostic.
    pub fn extend_labels<I, S>(&mut self, labels: I)
    where
        I: IntoIterator<Item = (Range<usize>, S)>,
        S: Into<String>,
    {
        self.labels
            .extend(labels.into_iter().map(|(r, s)| (r, s.into())));
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

/// Manages per-file store of diagnostic messages.
#[derive(Clone, Debug, Default)]
pub struct ProjectDiagnostics {
    file_diagnostics: Vec<(Utf8PathBuf, Diagnostics)>, // in insertion order
}

impl ProjectDiagnostics {
    pub fn new() -> Self {
        ProjectDiagnostics::default()
    }

    pub fn is_empty(&self) -> bool {
        // push() guarantees that each file store is not empty
        self.file_diagnostics.is_empty()
    }

    pub fn iter(&self) -> slice::Iter<'_, (Utf8PathBuf, Diagnostics)> {
        self.file_diagnostics.iter()
    }

    pub fn diagnostics(&self) -> impl Iterator<Item = (&Utf8Path, &Diagnostic)> {
        self.file_diagnostics
            .iter()
            .flat_map(|(p, ds)| ds.iter().map(|d| (p.as_ref(), d)))
    }

    pub fn push<P>(&mut self, path: P, diagnostics: Diagnostics)
    where
        P: AsRef<Utf8Path>,
    {
        if !diagnostics.is_empty() {
            self.file_diagnostics
                .push((path.as_ref().to_owned(), diagnostics));
        }
    }
}
