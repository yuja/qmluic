//! QML module/directory handling.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{UiImportSource, UiObjectDefinition, UiProgram};
use crate::qmldoc::UiDocument;
use crate::typemap::{ModuleData, ModuleId, QmlComponentData, TypeMap};
use camino::{Utf8Path, Utf8PathBuf};
use std::borrow::Cow;
use std::collections::HashMap;
use std::io;
use thiserror::Error;

/// Populates directory modules reachable from the given `src_paths`.
///
/// The `src_paths` element may point to either a QML file or a directory containing QML files.
pub fn populate_directories<I>(
    type_map: &mut TypeMap,
    doc_cache: &mut HashMap<Utf8PathBuf, UiDocument>,
    src_paths: I,
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

    while let Some(base_dir) = pending_dirs.pop() {
        if type_map.contains_module(ModuleId::Directory(Cow::Borrowed(base_dir.as_ref()))) {
            continue; // already visited
        }

        let mut base_module_data = ModuleData::default();
        for entry in base_dir
            .read_dir_utf8()
            .map_err(|e| PopulateError::ReadDir(base_dir.clone(), e))?
        {
            let entry = entry.map_err(|e| PopulateError::ReadDir(base_dir.clone(), e))?;
            if !is_qml_file(entry.path()) {
                continue;
            }

            // TODO: extract UiDocumentCache struct?
            use std::collections::hash_map::Entry;
            let doc = match doc_cache.entry(entry.path().to_owned()) {
                Entry::Occupied(e) => e.into_mut(),
                Entry::Vacant(e) => {
                    let doc = UiDocument::read(entry.path())
                        .map_err(|e| PopulateError::ReadUiDocument(entry.path().to_owned(), e))?;
                    e.insert(doc)
                }
            };

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
            // TODO: pass diagnostic messages to caller
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

    let mut data = QmlComponentData::with_super(
        doc.type_name().expect("doc must be read from file"),
        obj.type_name().to_string(doc.source()),
    );

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
        // TODO: warn that version field is ignored
        let id = match imp.source() {
            UiImportSource::Identifier(x) => {
                ModuleId::Named(Cow::Owned(x.to_string(doc.source()).into()))
            }
            UiImportSource::String(x) => {
                ModuleId::Directory(normalize_path(doc_base_dir.join(&x)).into())
            }
        };
        data.import_module(id);
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
    path.canonicalize_utf8().unwrap_or_else(|_| path.to_owned())
}

/// Error occurred while populating directory modules.
#[derive(Debug, Error)]
pub enum PopulateError {
    #[error("failed to read module directory '{0}': {1}")]
    ReadDir(Utf8PathBuf, #[source] io::Error),
    #[error("failed to read QML document '{0}': {1}")]
    ReadUiDocument(Utf8PathBuf, #[source] io::Error),
}
