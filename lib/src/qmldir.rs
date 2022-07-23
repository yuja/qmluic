//! QML module/directory handling.

use crate::diagnostic::{Diagnostic, Diagnostics, ProjectDiagnostics};
use crate::qmlast::{UiImportSource, UiObjectDefinition, UiProgram};
use crate::qmldoc::{UiDocument, UiDocumentsCache};
use crate::typemap::{ModuleData, ModuleId, QmlComponentData, TypeMap};
use camino::{Utf8Path, Utf8PathBuf};
use std::borrow::Cow;
use std::io;
use thiserror::Error;

/// Populates directory modules reachable from the given `src_paths`.
///
/// The `src_paths` element may point to either a QML file or a directory containing QML files.
pub fn populate_directories<I>(
    type_map: &mut TypeMap,
    docs_cache: &mut UiDocumentsCache,
    src_paths: I,
    project_diagnostics: &mut ProjectDiagnostics,
) -> Result<(), PopulateError>
where
    I: IntoIterator,
    I::Item: AsRef<Utf8Path>,
{
    let mut pending_dirs: Vec<_> = src_paths
        .into_iter()
        .map(|p| {
            let p = p.as_ref();
            if p.is_file() {
                normalize_path(p.parent().expect("file path should have parent"))
            } else {
                normalize_path(p)
            }
        })
        .collect();

    log::debug!("populating directories from {pending_dirs:?}");
    while let Some(base_dir) = pending_dirs.pop() {
        if type_map.contains_module(ModuleId::Directory(Cow::Borrowed(base_dir.as_ref()))) {
            continue; // already visited
        }

        log::trace!("processing directory {base_dir:?}");
        let mut base_module_data = ModuleData::default();
        for entry in base_dir
            .read_dir_utf8()
            .map_err(|e| PopulateError::ReadDir(base_dir.clone(), e))?
        {
            let entry = entry.map_err(|e| PopulateError::ReadDir(base_dir.clone(), e))?;
            if !is_qml_file(entry.path()) {
                continue;
            }

            log::trace!("processing file {:?}", entry.path());
            let doc = docs_cache
                .read(entry.path())
                .map_err(|e| PopulateError::ReadUiDocument(entry.path().to_owned(), e))?;

            let mut diagnostics = Diagnostics::new();
            if let Some(data) = make_doc_component_data(doc, &base_dir, &mut diagnostics) {
                for id in data.imports() {
                    match id {
                        ModuleId::Builtins | ModuleId::Named(_) => {}
                        ModuleId::Directory(dir) if !type_map.contains_module(id) => {
                            pending_dirs.push(dir.clone().into_owned());
                        }
                        ModuleId::Directory(_) => {} // already visited
                    }
                }
                base_module_data.push_qml_component(data);
            }
            project_diagnostics.push(entry.path(), diagnostics);
        }

        type_map.insert_module(ModuleId::Directory(base_dir.into()), base_module_data);
    }

    Ok(())
}

fn make_doc_component_data(
    doc: &UiDocument,
    doc_base_dir: &Utf8Path,
    diagnostics: &mut Diagnostics,
) -> Option<QmlComponentData> {
    let program = diagnostics.consume_err(UiProgram::from_node(doc.root_node(), doc.source()))?;
    let obj = diagnostics.consume_err(UiObjectDefinition::from_node(
        program.root_object_node(),
        doc.source(),
    ))?;

    let mut data =
        QmlComponentData::with_super(doc.type_name(), obj.type_name().to_string(doc.source()));

    // QML files in the base directory should be available by default
    data.import_module(ModuleId::Directory(doc_base_dir.to_owned().into()));

    for imp in program.imports() {
        if imp.alias().is_some() {
            diagnostics.push(Diagnostic::error(
                imp.node().byte_range(),
                "aliased import is not supported",
            ));
            continue;
        }
        if imp.version().is_some() {
            diagnostics.push(Diagnostic::warning(
                imp.node().byte_range(),
                "import version is ignored",
            ));
        }
        match imp.source() {
            UiImportSource::Identifier(x) => {
                let s = x.to_string(doc.source());
                data.import_module(ModuleId::Named(Cow::Owned(s.into())));
            }
            UiImportSource::String(x) => {
                let dir = doc_base_dir.join(&x);
                if dir.is_dir() {
                    data.import_module(ModuleId::Directory(normalize_path(dir).into()));
                } else {
                    // confine error so the population loop wouldn't fail with e.g. ENOENT
                    diagnostics.push(Diagnostic::error(
                        imp.node().byte_range(), // TODO: on imp.source() node
                        format!("source path is not a directory: {dir}"),
                    ));
                }
            }
        }
    }

    // TODO: properties, functions, etc. if we add support for those

    Some(data)
}

fn is_qml_file(path: &Utf8Path) -> bool {
    path.is_file()
        && path
            .extension()
            .map(|s| s.eq_ignore_ascii_case("qml"))
            .unwrap_or(false)
}

pub fn normalize_path<P>(path: P) -> Utf8PathBuf
where
    P: AsRef<Utf8Path>,
{
    // TODO: normalize per BuildContext rule?, which wouldn't fail
    let path = path.as_ref();
    if path.as_str().is_empty() {
        // Path("foo").parent() returns "", not "."
        // https://github.com/rust-lang/rust/issues/36861
        Utf8Path::new(".").canonicalize_utf8()
    } else {
        path.canonicalize_utf8()
    }
    .unwrap_or_else(|_| path.to_owned())
}

/// Error occurred while populating directory modules.
#[derive(Debug, Error)]
pub enum PopulateError {
    #[error("failed to read module directory '{0}': {1}")]
    ReadDir(Utf8PathBuf, #[source] io::Error),
    #[error("failed to read QML document '{0}': {1}")]
    ReadUiDocument(Utf8PathBuf, #[source] io::Error),
}
