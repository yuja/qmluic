#![forbid(unsafe_code)]

use anyhow::Context as _;
use clap::Parser;
use qmluic::diagnostic::{Diagnostic, DiagnosticKind, Diagnostics};
use qmluic::metatype;
use qmluic::metatype_tweak;
use qmluic::qmlast;
use qmluic::typemap::TypeMap;
use qmluic::uigen::{self, BuildContext, XmlWriter};
use qmluic_cli::QtPaths;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process;
use thiserror::Error;

#[derive(Parser, Clone, Debug, Eq, PartialEq)]
struct Args {
    /// File to parse
    file: PathBuf,
    #[clap(long)]
    /// Qt metatypes.json file to load (default: QT_INSTALL_LIBS/metatypes)
    foreign_types: Vec<PathBuf>,
}

#[derive(Debug, Error)]
enum CommandError {
    #[error("(see diagnostic messages)")]
    DiagnosticGenerated,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    match generate_ui(&args) {
        Ok(()) => Ok(()),
        Err(CommandError::DiagnosticGenerated) => process::exit(1),
        Err(CommandError::Other(e)) => Err(e),
    }
}

fn generate_ui(args: &Args) -> Result<(), CommandError> {
    let mut type_map = TypeMap::with_primitive_types();
    let mut classes = if args.foreign_types.is_empty() {
        let paths = QtPaths::query().context("failed to query Qt paths")?;
        if let Some(p) = &paths.install_libs {
            load_metatypes(&[p.join("metatypes")])?
        } else {
            eprintln!("Qt metatypes path cannot be detected");
            process::exit(1);
        }
    } else {
        load_metatypes(&args.foreign_types)?
    };
    metatype_tweak::apply_all(&mut classes);
    type_map.extend(classes);

    let doc = qmlast::UiDocument::read(&args.file)
        .with_context(|| format!("failed to load QML source: {}", args.file.display()))?;
    if doc.has_syntax_error() {
        print_syntax_errors(&doc).map_err(anyhow::Error::from)?;
        return Err(CommandError::DiagnosticGenerated);
    }

    let ctx = BuildContext::prepare(&type_map, &doc).map_err(anyhow::Error::from)?;
    let stdout = io::stdout();
    let mut diagnostics = Diagnostics::new();
    let form_opt = uigen::build(&ctx, &doc, &mut diagnostics);
    if form_opt.is_none() || !diagnostics.is_empty() {
        for d in &diagnostics {
            print_diagnostic(&doc, d).map_err(anyhow::Error::from)?;
        }
        return Err(CommandError::DiagnosticGenerated);
    }

    form_opt
        .map(|f| f.serialize_to_xml(&mut XmlWriter::new_with_indent(stdout.lock(), b' ', 1)))
        .unwrap_or(Ok(()))
        .context("failed to write UI XML")?;
    Ok(())
}

fn load_metatypes(paths: &[PathBuf]) -> anyhow::Result<Vec<metatype::Class>> {
    fn load_into(classes: &mut Vec<metatype::Class>, path: &Path) -> io::Result<()> {
        let data = fs::read_to_string(path)?;
        let cs = metatype::extract_classes_from_str(&data)?;
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
        print_diagnostic(doc, &e.into())?;
    }
    Ok(())
}

fn print_diagnostic(doc: &qmlast::UiDocument, diag: &Diagnostic) -> io::Result<()> {
    use ariadne::{Color, Label, Report, ReportKind, Source};
    let kind = match diag.kind() {
        DiagnosticKind::Error => ReportKind::Error,
    };
    let start_char_index = doc.source()[..diag.start_byte()].chars().count();
    let end_char_index = start_char_index + doc.source()[diag.byte_range()].chars().count();
    let report = Report::build(kind, (), start_char_index)
        .with_message(diag.message())
        .with_label(Label::new(start_char_index..end_char_index).with_color(Color::Yellow))
        .finish();
    report.eprint(Source::from(doc.source()))
}
