use camino::{Utf8Path, Utf8PathBuf};
use codespan_reporting::diagnostic::{Label, LabelStyle, Severity};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use qmluic::diagnostic::{DiagnosticKind, Diagnostics};
use qmluic::qmldoc::{SyntaxError, SyntaxErrorKind, UiDocument};
use std::borrow::Cow;
use std::env;
use std::iter;
use std::path::Path;
use termcolor::{ColorChoice, StandardStream};

pub type ReportableDiagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

pub fn print_syntax_errors(doc: &UiDocument) -> anyhow::Result<()> {
    let errors = doc.collect_syntax_errors();
    print_reportable_diagnostics(doc, make_reportable_syntax_errors(&errors))
}

pub fn print_diagnostics(doc: &UiDocument, diagnostics: &Diagnostics) -> anyhow::Result<()> {
    print_reportable_diagnostics(doc, make_reportable_diagnostics(diagnostics))
}

pub fn make_reportable_syntax_errors<'a>(
    errors: &'a [SyntaxError],
) -> impl Iterator<Item = ReportableDiagnostic> + 'a {
    errors.iter().map(|e| {
        let d = ReportableDiagnostic::error();
        match e.kind() {
            SyntaxErrorKind::Error => d
                .with_message("syntax error")
                .with_labels(vec![Label::primary((), e.byte_range())]),
            _ => d
                .with_message(format!("syntax error: {}", e))
                .with_labels(vec![
                    Label::primary((), e.byte_range()).with_message(e.to_string())
                ]),
        }
    })
}

pub fn make_reportable_diagnostics(
    diagnostics: &Diagnostics,
) -> impl Iterator<Item = ReportableDiagnostic> + '_ {
    diagnostics.iter().map(|diag| {
        let severity = match diag.kind() {
            DiagnosticKind::Error => Severity::Error,
            DiagnosticKind::Warning => Severity::Warning,
        };
        let labels = if diag.labels().is_empty() {
            vec![Label::primary((), diag.byte_range())]
        } else {
            diag.labels()
                .iter()
                .map(|(r, s)| {
                    let k = if r == &diag.byte_range() {
                        LabelStyle::Primary
                    } else {
                        LabelStyle::Secondary
                    };
                    Label::new(k, (), r.clone()).with_message(s)
                })
                .collect()
        };
        ReportableDiagnostic {
            severity,
            code: None,
            message: diag.message().into(),
            labels,
            notes: diag.notes().into(),
        }
    })
}

fn print_reportable_diagnostics<I>(doc: &UiDocument, diagnostics: I) -> anyhow::Result<()>
where
    I: IntoIterator<Item = ReportableDiagnostic>,
{
    let stderr = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    let rel_path = doc.path().map(make_cwd_relative_path);
    let files = SimpleFile::new(
        rel_path.unwrap_or_else(|| Cow::Borrowed("<unknown>".into())),
        doc.source(),
    );
    for d in diagnostics {
        term::emit_to_write_style(&mut stderr.lock(), &config, &files, &d)?;
    }
    Ok(())
}

/// Turns the given `path` into relative path from the `start`.
///
/// Both `path` and `start` are supposed to be absolute paths.
///
/// ```
/// # use camino::Utf8Path;
/// # use std::path::Path;
/// # use qmluic_cli::reporting::*;
/// assert_eq!(make_relative_path(Utf8Path::new("/foo"), Path::new("/foo")),
///            Utf8Path::new("."));
/// assert_eq!(make_relative_path(Utf8Path::new("/foo/bar"), Path::new("/foo")),
///            Utf8Path::new("bar"));
/// assert_eq!(make_relative_path(Utf8Path::new("/foo"), Path::new("/foo/bar")),
///            Utf8Path::new(".."));
/// assert_eq!(make_relative_path(Utf8Path::new("/foo/bar"), Path::new("/foo/foo/baz")),
///            Utf8Path::new("../../bar"));
/// assert_eq!(make_relative_path(Utf8Path::new("/foo/bar"), Path::new("/baz")),
///            Utf8Path::new("../foo/bar"));
/// ```
pub fn make_relative_path(path: &Utf8Path, start: impl AsRef<Path>) -> Cow<'_, Utf8Path> {
    // find common prefix
    for (i, base) in start.as_ref().ancestors().enumerate() {
        if let Ok(p) = path.strip_prefix(base) {
            if i == 0 && p.as_str().is_empty() {
                return Cow::Borrowed(".".into());
            } else if i == 0 {
                return Cow::Borrowed(p);
            } else {
                let mut rel_path = Utf8PathBuf::from_iter(iter::repeat("..").take(i));
                rel_path.push(p);
                return Cow::Owned(rel_path);
            }
        }
    }

    // no way to make it relative (e.g. different Windows drive letter?)
    Cow::Borrowed(path)
}

/// Turns the given `path` into relative path from the current working directory.
pub fn make_cwd_relative_path(path: &Utf8Path) -> Cow<'_, Utf8Path> {
    // qmldir::normalize_path() uses canonicalize(), which disagree with the cwd on Windows.
    if let Ok(cwd) = env::current_dir().and_then(|p| p.canonicalize()) {
        make_relative_path(path, cwd)
    } else {
        Cow::Borrowed(path)
    }
}
