#![forbid(unsafe_code)]

use clap::Parser;
use qmluic::metatype;
use qmluic::qmlast;
use qmluic::typemap::TypeMap;
use qmluic_cli::UiBuilder;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process;

#[derive(Parser, Clone, Debug, Eq, PartialEq)]
struct Args {
    /// File to parse
    file: PathBuf,
    #[clap(long)]
    /// Qt metatypes.json file to load
    foreign_types: Vec<PathBuf>,
}

fn main() -> quick_xml::Result<()> {
    let args = Args::parse();

    let mut type_map = TypeMap::with_primitive_types();
    type_map.extend(load_metatypes(&args.foreign_types)?);

    let doc = qmlast::UiDocument::read(&args.file)?;
    if doc.has_syntax_error() {
        print_syntax_errors(&doc)?;
        process::exit(1);
    }

    let stdout = io::stdout();
    let mut builder = UiBuilder::new(stdout.lock(), &type_map, &doc);
    builder.build()?;
    if !builder.errors().is_empty() {
        for e in builder.errors() {
            print_parse_error(&doc, &e)?;
        }
        process::exit(1);
    }

    Ok(())
}

fn load_metatypes(paths: &[PathBuf]) -> io::Result<Vec<metatype::Class>> {
    fn load_into(classes: &mut Vec<metatype::Class>, path: &Path) -> io::Result<()> {
        let data = fs::read_to_string(path)?;
        let cs = metatype::extract_classes_from_str(&data).map_err(|e| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("failed to load {}: {}", path.display(), e),
            )
        })?;
        classes.extend(cs);
        Ok(())
    }

    paths.iter().fold(Ok(vec![]), |acc, path| {
        acc.and_then(|mut classes| {
            if path.is_dir() {
                for e in fs::read_dir(path)? {
                    let p = e?.path();
                    if p.extension().map(|e| e == "json").unwrap_or(false) {
                        load_into(&mut classes, &p)?;
                    }
                }
            } else {
                load_into(&mut classes, path)?;
            }
            Ok(classes)
        })
    })
}

fn print_syntax_errors(doc: &qmlast::UiDocument) -> io::Result<()> {
    for e in &doc.collect_syntax_errors::<Vec<_>>() {
        print_parse_error(doc, e)?;
    }
    Ok(())
}

fn print_parse_error(doc: &qmlast::UiDocument, error: &qmlast::ParseError) -> io::Result<()> {
    use ariadne::{Color, Label, Report, ReportKind, Source};
    let start_char_index = doc.source()[..error.start_byte()].chars().count();
    let end_char_index = start_char_index + doc.source()[error.byte_range()].chars().count();
    let report = Report::build(ReportKind::Error, (), start_char_index)
        .with_message(error)
        .with_label(Label::new(start_char_index..end_char_index).with_color(Color::Yellow))
        .finish();
    report.eprint(Source::from(doc.source()))
}
