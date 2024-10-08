#![forbid(unsafe_code)]

use anyhow::{anyhow, Context as _};
use camino::{Utf8Component, Utf8Path, Utf8PathBuf};
use clap::{Args, Parser, Subcommand};
use notify::{RecursiveMode, Watcher as _};
use once_cell::sync::OnceCell;
use qmluic::diagnostic::{Diagnostics, ProjectDiagnostics};
use qmluic::metatype;
use qmluic::metatype_tweak;
use qmluic::qmldir;
use qmluic::qmldoc::UiDocumentsCache;
use qmluic::qtname::FileNameRules;
use qmluic::typemap::{ModuleData, ModuleId, TypeMap};
use qmluic::uigen::{self, BuildContext, DynamicBindingHandling, XmlWriter};
use qmluic_cli::{reporting, QtPaths, QtVersion, UiViewer};
use std::collections::HashSet;
use std::fs;
use std::io::{self, BufWriter, Write as _};
use std::path::Path;
use std::process;
use std::sync::mpsc;
use tempfile::NamedTempFile;
use thiserror::Error;

#[derive(Clone, Debug, Parser)]
struct Cli {
    #[command(flatten)]
    global_args: GlobalArgs,
    #[command(subcommand)]
    command: Command,
}

#[derive(Args, Clone, Debug)]
struct GlobalArgs {
    /// Command to query Qt installation paths.
    #[arg(long, global = true, default_value = "qmake")]
    qmake: String,
}

#[derive(Clone, Debug, Subcommand)]
enum Command {
    DumpMetatypes(DumpMetatypesArgs),
    GenerateUi(GenerateUiArgs),
    Preview(PreviewArgs),
}

#[derive(Debug, Error)]
enum CommandError {
    #[error("(see diagnostic messages)")]
    DiagnosticGenerated,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

#[derive(Clone, Debug)]
struct CommandHelper {
    global_args: GlobalArgs,
    qt_paths_cache: OnceCell<QtPaths>,
}

impl CommandHelper {
    fn new(global_args: GlobalArgs) -> Self {
        CommandHelper {
            global_args,
            qt_paths_cache: OnceCell::new(),
        }
    }

    pub fn qt_paths(&self) -> anyhow::Result<&QtPaths> {
        self.qt_paths_cache.get_or_try_init(|| {
            QtPaths::query_with(&self.global_args.qmake).with_context(|| {
                format!("failed to query Qt paths with '{}'", self.global_args.qmake)
            })
        })
    }
}

fn main() {
    pretty_env_logger::init_custom_env("QMLUIC_LOG");
    let cli = Cli::parse();
    let helper = CommandHelper::new(cli.global_args);
    match dispatch(&helper, &cli.command) {
        Ok(()) => {}
        Err(CommandError::DiagnosticGenerated) => {
            process::exit(1);
        }
        Err(CommandError::Other(err)) => {
            eprintln!(
                "{}: {}",
                console::style("error").for_stderr().red().bold(),
                console::style(&err).for_stderr().bold()
            );
            for cause in err.chain().skip(1) {
                eprintln!("{}: {}", console::style("cause").for_stderr().blue(), cause);
            }
            process::exit(1);
        }
    }
}

fn dispatch(helper: &CommandHelper, command: &Command) -> Result<(), CommandError> {
    match command {
        Command::DumpMetatypes(args) => dump_metatypes(helper, args),
        Command::GenerateUi(args) => generate_ui(helper, args),
        Command::Preview(args) => preview(helper, args),
    }
}

/// Apply tweaks on QtWidgets metatypes and output modified version
///
/// If --output-qmltypes is specified, qmltyperegistrar will be invoked with the metatypes
/// to generate qmltypes.
#[derive(Args, Clone, Debug)]
struct DumpMetatypesArgs {
    /// Source metatypes file (.json)
    input: Utf8PathBuf,
    #[arg(short = 'o', long)]
    /// Write metatypes output to file (e.g. metatypes.json)
    output_metatypes: Option<Utf8PathBuf>,
    #[arg(long)]
    /// Write qmltypes output to file (e.g. plugins.qmltypes)
    output_qmltypes: Option<Utf8PathBuf>,
}

fn dump_metatypes(helper: &CommandHelper, args: &DumpMetatypesArgs) -> Result<(), CommandError> {
    eprintln!(
        "{} {}",
        console::style("processing").for_stderr().green().bold(),
        &args.input
    );
    let data = fs::read_to_string(&args.input)
        .with_context(|| format!("failed to load metatypes file: {}", args.input))?;
    // TODO: report ignored: https://github.com/dtolnay/serde-ignored ?
    let mut units: Vec<metatype::CompilationUnit> = serde_json::from_str(&data)
        .with_context(|| format!("failed to parse metatypes file: {}", args.input))?;

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
        classes: metatype_tweak::internal_gui_classes().into_iter().collect(),
        ..Default::default()
    });
    units.push(metatype::CompilationUnit {
        classes: metatype_tweak::internal_widgets_classes()
            .into_iter()
            .collect(),
        ..Default::default()
    });

    if let Some(metatypes_path) = &args.output_metatypes {
        let perm = fs::metadata(&args.input)
            .context("failed to get source file permission")?
            .permissions(); // assume this is the default file permissions
        with_output_file(metatypes_path, perm, |out| {
            serde_json::to_writer_pretty(BufWriter::new(out), &units)
        })
        .with_context(|| format!("failed to dump metatypes to file: {metatypes_path}"))?;
        if let Some(qmltypes_path) = &args.output_qmltypes {
            generate_qmltypes(helper.qt_paths()?, qmltypes_path, metatypes_path)?;
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
            helper.qt_paths()?,
            qmltypes_path,
            metatypes_file
                .path()
                .try_into()
                .expect("temporary file name should be valid utf-8"),
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
    output_qmltypes: &Utf8Path,
    source_metatypes: &Utf8Path,
) -> Result<(), CommandError> {
    let bin_path = match qt_paths {
        QtPaths {
            version: Some(QtVersion { major: 6, .. }),
            install_libexecs: Some(p),
            ..
        } => p,
        QtPaths {
            version: Some(QtVersion { major: 5, .. }),
            install_bins: Some(p),
            ..
        } => p,
        _ => return Err(anyhow!("qmltyperegistrar path cannot be detected").into()),
    };
    let metatypes_path = qt_paths
        .install_libs
        .as_ref()
        .map(|p| p.join("metatypes"))
        .ok_or_else(|| anyhow!("Qt metatypes path cannot be detected"))?;
    let qt_version = qt_paths
        .version
        .ok_or_else(|| anyhow!("Qt version cannot be detected"))?;
    if output_qmltypes.as_str().starts_with('-') {
        // qmltyperegistrar doesn't support "--" separator
        return Err(CommandError::Other(anyhow!(
            "invalid output file name: {output_qmltypes}"
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
    log::debug!("executing {cmd:?}");
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
    /// QML files to parse
    #[arg(required = true)]
    sources: Vec<Utf8PathBuf>,
    /// Create output files in the given directory.
    ///
    /// If specified, the source file paths must be relative and not contain "..".
    #[arg(short = 'O', long)]
    output_directory: Option<Utf8PathBuf>,
    #[arg(long)]
    /// Qt metatypes.json files/directories to load
    ///
    /// By default, qt${major}core/gui/widgets_*.json will be loaded from the
    /// QT_INSTALL_LIBS/metatypes directory.
    foreign_types: Vec<Utf8PathBuf>,
    /// Do not generate C++ code to set up dynamic bindings
    #[arg(long)]
    no_dynamic_binding: bool,
    /// Do not convert output file names to lowercase
    #[arg(long)]
    no_lowercase_file_name: bool,
}

fn generate_ui(helper: &CommandHelper, args: &GenerateUiArgs) -> Result<(), CommandError> {
    if args.output_directory.is_some()
        && args.sources.iter().any(|p| {
            !p.components()
                .all(|c| matches!(c, Utf8Component::CurDir | Utf8Component::Normal(_)))
        })
    {
        return Err(CommandError::Other(anyhow!(
            "source file paths must be relative if --output-directory is specified",
        )));
    }

    let mut type_map = load_type_map(helper, &args.foreign_types)?;
    let mut docs_cache = UiDocumentsCache::new();
    let mut project_diagnostics = ProjectDiagnostics::new();
    qmldir::populate_directories(
        &mut type_map,
        &mut docs_cache,
        &args.sources,
        &mut project_diagnostics,
    )
    .map_err(anyhow::Error::from)?;
    if !project_diagnostics.is_empty() {
        // for the specified sources, more detailed diagnostics will be emitted later
        let sources: HashSet<_> = args.sources.iter().map(qmldir::normalize_path).collect();
        for (p, ds) in project_diagnostics
            .iter()
            .filter(|(p, _)| !sources.contains(p))
        {
            reporting::print_diagnostics(docs_cache.get(p).unwrap(), ds)?;
        }
    }

    let file_name_rules = FileNameRules {
        lowercase: !args.no_lowercase_file_name,
        ..Default::default()
    };
    let dynamic_binding_handling = if args.no_dynamic_binding {
        DynamicBindingHandling::Reject
    } else {
        DynamicBindingHandling::Generate
    };
    let ctx = BuildContext::prepare(&type_map, file_name_rules, dynamic_binding_handling)
        .map_err(anyhow::Error::from)?;
    for p in &args.sources {
        generate_ui_file(&ctx, &docs_cache, p, args.output_directory.as_deref())?;
    }
    Ok(())
}

fn generate_ui_file(
    ctx: &BuildContext,
    docs_cache: &UiDocumentsCache,
    source: &Utf8Path,
    output_directory: Option<&Utf8Path>,
) -> Result<(), CommandError> {
    eprintln!(
        "{} {}",
        console::style("processing").for_stderr().green().bold(),
        source
    );
    let doc = docs_cache
        .get(source)
        .ok_or_else(|| anyhow!("QML source not loaded (bad file suffix?): {source}"))?;
    if doc.has_syntax_error() {
        reporting::print_syntax_errors(doc)?;
        return Err(CommandError::DiagnosticGenerated);
    }

    log::debug!("building ui for {source:?}");
    let mut diagnostics = Diagnostics::new();
    let (form, ui_support_opt) = match uigen::build(ctx, doc, &mut diagnostics) {
        Some(x) if !diagnostics.has_error() => {
            reporting::print_diagnostics(doc, &diagnostics)?; // emit warnings if any
            x
        }
        _ => {
            reporting::print_diagnostics(doc, &diagnostics)?;
            return Err(CommandError::DiagnosticGenerated);
        }
    };

    let (ui_out_path, ui_support_h_out_path) = {
        let ui_path =
            source.with_file_name(ctx.file_name_rules.type_name_to_ui_name(doc.type_name()));
        let ui_support_h_path = source.with_file_name(
            ctx.file_name_rules
                .type_name_to_ui_support_cxx_header_name(doc.type_name()),
        );
        if let Some(dir) = output_directory {
            (dir.join(ui_path), dir.join(ui_support_h_path))
        } else {
            (ui_path, ui_support_h_path)
        }
    };
    let perm = fs::metadata(source)
        .context("failed to get source file permission")?
        .permissions(); // assume this is the default file permissions

    let mut ui_serialized = Vec::new();
    form.serialize_to_xml(&mut XmlWriter::new_with_indent(&mut ui_serialized, b' ', 1))
        .context("failed to serializee UI XML")?;
    // do not touch the output file if unchanged so the subsequent build steps wouldn't run
    if !fs::read(&ui_out_path)
        .map(|d| d == ui_serialized)
        .unwrap_or(false)
    {
        with_output_file(&ui_out_path, perm.clone(), |out| {
            out.write_all(&ui_serialized)
        })
        .context("failed to write UI XML")?;
    }

    if let Some(ui_support) = ui_support_opt {
        let mut h_data = Vec::new();
        ui_support
            .write_header(&mut h_data)
            .context("failed to generate UI support header")?;
        if !fs::read(&ui_support_h_out_path)
            .map(|d| d == h_data)
            .unwrap_or(false)
        {
            with_output_file(&ui_support_h_out_path, perm, |out| out.write_all(&h_data))
                .context("failed to write UI support header")?;
        }
    }

    Ok(())
}

/// Preview UI QML (.qml)
///
/// The previewer tries to render the UI QML document even it had syntax/semantic errors.
#[derive(Args, Clone, Debug)]
struct PreviewArgs {
    /// QML file to load
    source: Utf8PathBuf,
    #[arg(long)]
    /// Qt metatypes.json files/directories to load
    ///
    /// By default, qt${major}core/gui/widgets_*.json will be loaded from the
    /// QT_INSTALL_LIBS/metatypes directory.
    foreign_types: Vec<Utf8PathBuf>,
}

fn preview(helper: &CommandHelper, args: &PreviewArgs) -> Result<(), CommandError> {
    let mut type_map = load_type_map(helper, &args.foreign_types)?;
    let mut docs_cache = UiDocumentsCache::new();
    let mut project_diagnostics = ProjectDiagnostics::new();
    qmldir::populate_directories(
        &mut type_map,
        &mut docs_cache,
        [&args.source],
        &mut project_diagnostics,
    )
    .map_err(anyhow::Error::from)?;
    for (p, ds) in project_diagnostics.iter() {
        reporting::print_diagnostics(docs_cache.get(p).unwrap(), ds)?;
    }

    let mut viewer = UiViewer::spawn()?;
    let file_name_rules = FileNameRules::default();
    let ctx = BuildContext::prepare(&type_map, file_name_rules, DynamicBindingHandling::Omit)
        .map_err(anyhow::Error::from)?;
    preview_file(&mut viewer, &ctx, &mut docs_cache, &args.source)?;

    #[derive(Debug)]
    enum Message {
        Filesystem(notify::Result<notify::Event>),
        Interrupted,
    }

    let (tx, rx) = mpsc::channel();
    let tx2 = tx.clone();
    let mut watcher = notify::recommended_watcher(move |r| {
        let _ = tx.send(Message::Filesystem(r));
    })
    .context("failed to create file watcher")?;
    ctrlc::set_handler(move || {
        let _ = tx2.send(Message::Interrupted);
    })
    .context("failed to register signal handler")?;
    let canonical_doc_path = args
        .source
        .canonicalize()
        .context("failed to get canonical doc path")?;
    // watches the parent directory as the doc file may be recreated
    let watch_base_path = canonical_doc_path
        .parent()
        .expect("file path should have parent");
    watcher
        .watch(watch_base_path, RecursiveMode::NonRecursive)
        .with_context(|| format!("failed to watch {:?}", watch_base_path))?;
    while let Ok(m) = rx.recv() {
        log::trace!("message: {m:?}");
        match m {
            Message::Filesystem(Ok(ev)) => {
                use notify::EventKind;
                if matches!(ev.kind, EventKind::Create(_) | EventKind::Modify(_))
                    && ev.paths.contains(&canonical_doc_path)
                {
                    // TODO: refresh populated qmldirs as needed
                    docs_cache.remove(&args.source);
                    preview_file(&mut viewer, &ctx, &mut docs_cache, &args.source)?;
                }
            }
            Message::Filesystem(Err(_)) => {}
            Message::Interrupted => break,
        }
    }

    // TODO: quit on previewer window closed
    viewer.wait()?;
    Ok(())
}

fn preview_file(
    viewer: &mut UiViewer,
    ctx: &BuildContext,
    docs_cache: &mut UiDocumentsCache,
    source: &Utf8Path,
) -> Result<(), CommandError> {
    eprintln!(
        "{} {}",
        console::style("processing").for_stderr().green().bold(),
        source
    );
    let doc = docs_cache
        .read(source)
        .with_context(|| format!("failed to load QML document: {source}"))?;
    if doc.has_syntax_error() {
        reporting::print_syntax_errors(doc)?;
    }

    log::debug!("building ui for {:?}", source);
    let mut diagnostics = Diagnostics::new();
    let maybe_form = uigen::build(ctx, doc, &mut diagnostics);
    reporting::print_diagnostics(doc, &diagnostics)?;
    if let Some((form, None)) = maybe_form {
        let mut data = Vec::new();
        form.serialize_to_xml(&mut XmlWriter::new(&mut data))
            .context("failed to serialize UI to XML")?;
        viewer.write_ui_data(&data)?;
    }
    Ok(())
}

fn load_type_map(
    helper: &CommandHelper,
    foreign_type_paths: &[Utf8PathBuf],
) -> anyhow::Result<TypeMap> {
    let mut type_map = TypeMap::with_primitive_types();
    let mut classes = if foreign_type_paths.is_empty() {
        load_metatypes(&find_installed_metatype_files(helper.qt_paths()?)?)?
    } else {
        load_metatypes(foreign_type_paths)?
    };

    metatype_tweak::apply_all(&mut classes);
    let mut module_data = ModuleData::with_builtins();
    module_data.extend(classes);
    type_map.insert_module(ModuleId::Named("qmluic.QtWidgets"), module_data);
    Ok(type_map)
}

fn load_metatypes(paths: &[Utf8PathBuf]) -> anyhow::Result<Vec<metatype::Class>> {
    fn load_into(classes: &mut Vec<metatype::Class>, path: &Utf8Path) -> io::Result<()> {
        log::trace!("loading metatypes file {path:?}");
        let data = fs::read_to_string(path)?;
        let cs = metatype::extract_classes_from_str(&data)?;
        classes.extend(cs);
        Ok(())
    }

    log::debug!("loading metatypes from {paths:?}");
    paths.iter().try_fold(vec![], |mut classes, path| {
        if path.is_dir() {
            for r in path.read_dir_utf8()? {
                let e = r?;
                let p = e.path();
                if p.as_str().ends_with(".json") {
                    load_into(&mut classes, p)?;
                }
            }
        } else {
            load_into(&mut classes, path)?;
        }
        Ok(classes)
    })
}

fn find_installed_metatype_files(paths: &QtPaths) -> anyhow::Result<Vec<Utf8PathBuf>> {
    // Recent versions of Qt 6 appear to install metatypes under the ARCHDATA
    // directory. Old Qt 6 and Qt 5 install them to LIBS.
    let base_path = [paths.install_archdata.as_ref(), paths.install_libs.as_ref()]
        .iter()
        .flatten()
        .map(|p| p.join("metatypes"))
        .find(|p| p.exists())
        .ok_or_else(|| anyhow!("Qt metatypes path cannot be detected"))?;
    let major = paths
        .version
        .as_ref()
        .map(|v| v.major)
        .ok_or_else(|| anyhow!("Qt metatypes version cannot be detected"))?;
    let prefixes = [
        format!("qt{major}core_"),
        format!("qt{major}gui_"),
        format!("qt{major}widgets_"),
    ];

    let mut file_paths = Vec::new();
    for r in base_path.read_dir_utf8()? {
        let e = r?;
        let p = e.path();
        if p.file_name()
            .map(|s| prefixes.iter().any(|p| s.starts_with(p)) && s.ends_with(".json"))
            .unwrap_or(false)
        {
            file_paths.push(p.to_owned());
        }
    }
    Ok(file_paths)
}

fn with_output_file<P, F, E>(path: P, perm: fs::Permissions, f: F) -> anyhow::Result<()>
where
    P: AsRef<Path>,
    F: FnOnce(&mut NamedTempFile) -> Result<(), E>,
    E: std::error::Error + Send + Sync + 'static,
{
    let path = path.as_ref();
    let dir = path.parent().ok_or_else(|| anyhow!("invalid file name"))?;
    fs::create_dir_all(dir).context("failed to create output directory")?;
    let mut out = NamedTempFile::new_in(dir)?;
    f(&mut out)?;
    out.as_file().set_permissions(perm)?;
    out.persist(path)?;
    Ok(())
}
