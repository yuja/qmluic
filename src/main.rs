#![forbid(unsafe_code)]

use clap::Parser;
use qmluic::qml;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process;

mod uibuilder;

use self::uibuilder::UiBuilder;

#[derive(Parser, Clone, Debug, Eq, PartialEq)]
struct Args {
    /// File to parse
    file: PathBuf,
}

fn main() -> quick_xml::Result<()> {
    let args = Args::parse();

    let doc = qml::UiDocument::with_source(fs::read_to_string(&args.file)?);
    if doc.has_syntax_error() {
        print_syntax_errors(&doc)?;
        process::exit(1);
    }

    let class_name = class_name_for_path(&args.file).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("cannot deduce class name for {}", args.file.display()),
        )
    })?;

    let stdout = io::stdout();
    let mut builder = UiBuilder::new(stdout.lock(), &doc, class_name);
    builder.build()?;
    if !builder.errors().is_empty() {
        for e in builder.errors() {
            print_parse_error(&doc, &e)?;
        }
        process::exit(1);
    }

    Ok(())
}

fn class_name_for_path(path: &Path) -> Option<&str> {
    path.file_name()
        .and_then(|s| s.to_str())
        .map(|name| name.rsplit_once('.').map(|(n, _)| n).unwrap_or(name))
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
