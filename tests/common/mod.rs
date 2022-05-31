use camino::Utf8Path;
use codespan_reporting::diagnostic::{Label, Severity};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use qmluic::diagnostic::{DiagnosticKind, Diagnostics, ProjectDiagnostics};
use qmluic::metatype;
use qmluic::metatype_tweak;
use qmluic::qmldir;
use qmluic::qmldoc::{UiDocument, UiDocumentsCache};
use qmluic::typemap::{ModuleData, ModuleId, TypeMap};
use qmluic::uigen::{self, BuildContext, XmlWriter};
use std::fs;
use std::str;
use termcolor::NoColor;

pub fn translate_file(path: impl AsRef<Utf8Path>) -> Result<String, String> {
    let doc = UiDocument::read(path).unwrap();
    translate_doc(&doc)
}

pub fn translate_str(source: impl Into<String>) -> Result<String, String> {
    let doc = UiDocument::parse(source, None);
    translate_doc(&doc)
}

fn translate_doc(doc: &UiDocument) -> Result<String, String> {
    assert!(!doc.has_syntax_error());
    let mut type_map = TypeMap::with_primitive_types();
    let mut classes = load_metatypes();
    metatype_tweak::apply_all(&mut classes);
    let mut module_data = ModuleData::with_builtins();
    module_data.extend(classes);
    type_map.insert_module(ModuleId::Named("qmluic.QtWidgets".into()), module_data);

    if let Some(p) = doc.path() {
        let mut docs_cache = UiDocumentsCache::new();
        let mut project_diagnostics = ProjectDiagnostics::new();
        qmldir::populate_directories(
            &mut type_map,
            &mut docs_cache,
            [p],
            &mut project_diagnostics,
        )
        .unwrap();
    }

    let ctx = BuildContext::prepare(&type_map).unwrap();
    let mut diagnostics = Diagnostics::new();
    let form = match uigen::build(&ctx, doc, &mut diagnostics) {
        Some(form) if diagnostics.is_empty() => form,
        _ => return Err(format_diagnostics(doc, &diagnostics)),
    };
    let mut buf = Vec::new();
    form.serialize_to_xml(&mut XmlWriter::new_with_indent(&mut buf, b' ', 1))
        .unwrap();
    Ok(String::from_utf8(buf).unwrap())
}

fn load_metatypes() -> Vec<metatype::Class> {
    let paths = [
        "contrib/metatypes/qt5core_metatypes.json",
        "contrib/metatypes/qt5gui_metatypes.json",
        "contrib/metatypes/qt5widgets_metatypes.json",
    ];
    paths
        .iter()
        .flat_map(|p| {
            let data = fs::read_to_string(p).unwrap();
            metatype::extract_classes_from_str(&data).unwrap()
        })
        .collect()
}

fn format_diagnostics(doc: &UiDocument, diagnostics: &Diagnostics) -> String {
    let mut buf = Vec::new();
    let mut writer = NoColor::new(&mut buf);
    let config = term::Config {
        display_style: term::DisplayStyle::Short,
        ..Default::default()
    };
    let files = SimpleFile::new(doc.type_name().unwrap_or("<unknown>"), doc.source());

    for diag in diagnostics {
        let severity = match diag.kind() {
            DiagnosticKind::Error => Severity::Error,
        };
        let cdiag = codespan_reporting::diagnostic::Diagnostic::new(severity)
            .with_message(diag.message())
            .with_labels(vec![
                Label::primary((), diag.byte_range()).with_message(diag.message())
            ]);
        term::emit(&mut writer, &config, &files, &cdiag).unwrap();
    }
    str::from_utf8(&buf).unwrap().trim_end().to_owned()
}
