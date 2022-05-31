use codespan_reporting::diagnostic::{Label, Severity};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use qmluic::diagnostic::{DiagnosticKind, Diagnostics};
use qmluic::qmldoc::UiDocument;
use termcolor::{ColorChoice, StandardStream};

type ReportableDiagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

pub fn print_syntax_errors(doc: &UiDocument) -> anyhow::Result<()> {
    let errors: Vec<_> = doc.collect_syntax_errors();
    print_reportable_diagnostics(
        doc,
        errors.iter().map(|err| {
            let msg = err.to_string();
            ReportableDiagnostic::error()
                .with_message(&msg)
                .with_labels(vec![Label::primary((), err.byte_range()).with_message(&msg)])
        }),
    )
}

pub fn print_diagnostics(doc: &UiDocument, diagnostics: &Diagnostics) -> anyhow::Result<()> {
    print_reportable_diagnostics(
        doc,
        diagnostics.iter().map(|diag| {
            let severity = match diag.kind() {
                DiagnosticKind::Error => Severity::Error,
            };
            ReportableDiagnostic::new(severity)
                .with_message(diag.message())
                .with_labels(vec![
                    Label::primary((), diag.byte_range()).with_message(diag.message())
                ])
        }),
    )
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
