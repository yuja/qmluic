use codespan_reporting::diagnostic::{Label, LabelStyle, Severity};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use qmluic::diagnostic::{DiagnosticKind, Diagnostics};
use qmluic::qmldoc::{SyntaxError, SyntaxErrorKind, UiDocument};
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
        ReportableDiagnostic::new(severity)
            .with_message(diag.message())
            .with_labels(labels)
    })
}

fn print_reportable_diagnostics<I>(doc: &UiDocument, diagnostics: I) -> anyhow::Result<()>
where
    I: IntoIterator<Item = ReportableDiagnostic>,
{
    let stderr = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    let files = SimpleFile::new(
        doc.path().map(|p| p.as_str()).unwrap_or("<unknown>"),
        doc.source(),
    );
    for d in diagnostics {
        term::emit(&mut stderr.lock(), &config, &files, &d)?;
    }
    Ok(())
}
