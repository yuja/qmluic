#![forbid(unsafe_code)]

use anyhow::{anyhow, Context as _};
use clap::{Args, Parser, Subcommand};
use qmluic::diagnostic::{Diagnostic, DiagnosticKind, Diagnostics};
use qmluic::metatype;
use qmluic::metatype_tweak;
use qmluic::qmlast;
use qmluic::typemap::TypeMap;
use qmluic::uigen::{self, BuildContext, XmlWriter};
use qmluic_cli::QtPaths;
use std::fs;
use std::io::{self, BufWriter, Write as _};
use std::path::{Path, PathBuf};
use std::process;
use tempfile::NamedTempFile;
use thiserror::Error;

#[derive(Clone, Debug, Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Clone, Debug, Subcommand)]
enum Command {
    DumpMetatypes(DumpMetatypesArgs),
    GenerateUi(GenerateUiArgs),
}

#[derive(Debug, Error)]
enum CommandError {
    #[error("(see diagnostic messages)")]
    DiagnosticGenerated,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

fn main() -> anyhow::Result<()> {
    pretty_env_logger::init_custom_env("QMLUIC_LOG");
    let cli = Cli::parse();
    match dispatch(&cli) {
        Ok(()) => Ok(()),
        Err(CommandError::DiagnosticGenerated) => process::exit(1),
        Err(CommandError::Other(e)) => Err(e),
    }
}

fn dispatch(cli: &Cli) -> Result<(), CommandError> {
    match &cli.command {
        Command::DumpMetatypes(args) => dump_metatypes(args),
        Command::GenerateUi(args) => generate_ui(args),
    }
}

/// Apply tweaks on QtWidgets metatypes and output modified version
///
/// If --output-qmltypes is specified, qmltyperegistrar will be invoked with the metatypes
/// to generate qmltypes.
#[derive(Args, Clone, Debug)]
struct DumpMetatypesArgs {
    /// Source metatypes file (.json)
    input: PathBuf,
    #[clap(short = 'o', long)]
    /// Write metatypes output to file (e.g. metatypes.json)
    output_metatypes: Option<PathBuf>,
    #[clap(long)]
    /// Write qmltypes output to file (e.g. plugins.qmltypes)
    output_qmltypes: Option<PathBuf>,
}

fn dump_metatypes(args: &DumpMetatypesArgs) -> Result<(), CommandError> {
    let data = fs::read_to_string(&args.input)
        .with_context(|| format!("failed to load metatypes file: {}", args.input.display()))?;
    // TODO: report ignored: https://github.com/dtolnay/serde-ignored ?
    let mut units: Vec<metatype::CompilationUnit> = serde_json::from_str(&data)
        .with_context(|| format!("failed to parse metatypes file: {}", args.input.display()))?;

    for u in units.iter_mut() {
        metatype_tweak::fix_classes(&mut u.classes);
        for c in u.classes.iter_mut() {
            // TODO: maybe restrict to QWidget/QLayout classes?
            c.class_infos
                .push(metatype::ClassInfo::new("QML.Element", "auto"));
        }
    }

    // attached type shouldn't be exported as QML.Element
    units.push(metatype::CompilationUnit {
        classes: metatype_tweak::internal_widgets_classes()
            .into_iter()
            .collect(),
        ..Default::default()
    });

    if let Some(metatypes_path) = &args.output_metatypes {
        with_output_file(metatypes_path, |out| {
            serde_json::to_writer_pretty(BufWriter::new(out), &units)
        })
        .with_context(|| {
            format!(
                "failed to dump metatypes to file: {}",
                metatypes_path.display()
            )
        })?;
        if let Some(qmltypes_path) = &args.output_qmltypes {
            generate_qmltypes(
                &QtPaths::query().context("failed to query Qt paths")?,
                qmltypes_path,
                metatypes_path,
            )?;
        }
    } else if let Some(qmltypes_path) = &args.output_qmltypes {
        let mut metatypes_file =
            NamedTempFile::new().context("failed to create temporary metatypes file")?;
        serde_json::to_writer_pretty(BufWriter::new(&mut metatypes_file), &units)
            .context("failed to dump temporary metatypes")?;
        metatypes_file
            .flush()
            .context("failed to dump temporary metatypes")?;
        generate_qmltypes(
            &QtPaths::query().context("failed to query Qt paths")?,
            qmltypes_path,
            metatypes_file.path(),
        )?;
    } else {
        let stdout = io::stdout();
        serde_json::to_writer_pretty(BufWriter::new(stdout.lock()), &units)
            .context("failed to dump metatypes")?;
    }
    Ok(())
}

fn generate_qmltypes(
    qt_paths: &QtPaths,
    output_qmltypes: &Path,
    source_metatypes: &Path,
) -> Result<(), CommandError> {
    let bin_path = qt_paths
        .install_bins
        .as_ref()
        .ok_or_else(|| anyhow!("qmltyperegistrar path cannot be detected"))?;
    let metatypes_path = qt_paths
        .install_libs
        .as_ref()
        .map(|p| p.join("metatypes"))
        .ok_or_else(|| anyhow!("Qt metatypes path cannot be detected"))?;
    let qt_version = qt_paths
        .version
        .ok_or_else(|| anyhow!("Qt version cannot be detected"))?;
    if output_qmltypes
        .to_str()
        .map(|s| s.starts_with('-'))
        .unwrap_or(true)
    {
        // qmltyperegistrar doesn't support "--" separator
        return Err(CommandError::Other(anyhow!(
            "invalid output file name: {}",
            output_qmltypes.display()
        )));
    }
    let mut cmd = process::Command::new(bin_path.join("qmltyperegistrar"));
    cmd.arg("--import-name")
        .arg("qmluic.QtWidgets")
        .arg("--major-version")
        .arg(qt_version.major.to_string())
        .arg("--foreign-types")
        .arg(metatypes_path.join(format!("qt{}core_metatypes.json", qt_version.major)))
        .arg("--generate-qmltypes")
        .arg(output_qmltypes)
        .arg(source_metatypes)
        .stdout(process::Stdio::null());
    log::info!("executing {cmd:?}");
    let status = cmd.status().context("failed to run qmltyperegistrar")?;
    if status.success() {
        Ok(())
    } else {
        Err(CommandError::Other(anyhow!(
            "qmltyperegistrar exited with {}",
            status.code().unwrap_or(-1)
        )))
    }
}

/// Generate UI XML (.ui) from QML (.qml)
#[derive(Args, Clone, Debug)]
struct GenerateUiArgs {
    /// QML File to parse
    file: PathBuf,
    #[clap(long)]
    /// Qt metatypes.json file to load (default: QT_INSTALL_LIBS/metatypes)
    foreign_types: Vec<PathBuf>,
}

fn generate_ui(args: &GenerateUiArgs) -> Result<(), CommandError> {
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

fn with_output_file<P, F, E>(path: P, f: F) -> anyhow::Result<()>
where
    P: AsRef<Path>,
    F: FnOnce(&mut NamedTempFile) -> Result<(), E>,
    E: std::error::Error + Send + Sync + 'static,
{
    let path = path.as_ref();
    let mut out =
        NamedTempFile::new_in(path.parent().ok_or_else(|| anyhow!("invalid file name"))?)?;
    f(&mut out)?;
    out.persist(path)?;
    Ok(())
}
