#![forbid(unsafe_code)]

use clap::Parser;
use qmluic::qml;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::process;

#[derive(Parser, Clone, Debug, Eq, PartialEq)]
struct Args {
    /// File to parse
    file: PathBuf,
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let doc = qml::UiDocument::with_source(fs::read_to_string(args.file)?);
    if doc.has_syntax_error() {
        print_syntax_errors(&doc)?;
        process::exit(1);
    }

    Ok(())
}

fn print_syntax_errors(doc: &qml::UiDocument) -> io::Result<()> {
    for e in &doc.collect_syntax_errors::<Vec<_>>() {
        print_parse_error(doc, e)?;
    }
    Ok(())
}

fn print_parse_error(doc: &qml::UiDocument, error: &qml::ParseError) -> io::Result<()> {
    use ariadne::{Color, Label, Report, ReportKind, Source};
    let start_char_index = doc.source()[..error.start_byte()].chars().count();
    let end_char_index = start_char_index + doc.source()[error.byte_range()].chars().count();
    let report = Report::build(ReportKind::Error, (), start_char_index)
        .with_message(error)
        .with_label(Label::new(start_char_index..end_char_index).with_color(Color::Yellow))
        .finish();
    report.eprint(Source::from(doc.source()))
}
