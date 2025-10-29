use assert_cmd::Command;
use camino::Utf8Path;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use qmluic::diagnostic::{Diagnostics, ProjectDiagnostics};
use qmluic::metatype;
use qmluic::metatype_tweak;
use qmluic::qmldir;
use qmluic::qmldoc::{UiDocument, UiDocumentsCache};
use qmluic::qtname::FileNameRules;
use qmluic::typemap::{ModuleData, ModuleId, TypeMap};
use qmluic::uigen::{self, BuildContext, DynamicBindingHandling, XmlWriter};
use qmluic_cli::reporting::{self, ReportableDiagnostic};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::str;
use tempfile::TempDir;

pub struct TestEnv {
    temp_dir: TempDir,
}

impl TestEnv {
    pub fn prepare() -> Self {
        let temp_dir = TempDir::new().unwrap();
        TestEnv { temp_dir }
    }

    pub fn base_path(&self) -> &Path {
        self.temp_dir.path()
    }

    pub fn join(&self, path: impl AsRef<Path>) -> PathBuf {
        self.base_path().join(path)
    }

    pub fn create_dir_all(&self, path: impl AsRef<Path>) {
        fs::create_dir_all(self.join(path)).unwrap();
    }

    pub fn write_dedent(&self, path: impl AsRef<Path>, data: impl AsRef<str>) {
        let full_path = self.join(path);
        fs::create_dir_all(full_path.parent().unwrap()).unwrap();
        fs::write(&full_path, dedent(data)).unwrap();
    }

    pub fn read_to_string(&self, path: impl AsRef<Path>) -> String {
        fs::read_to_string(self.join(path)).unwrap()
    }

    pub fn generate_ui_cmd(&self, args: impl IntoIterator<Item = impl AsRef<OsStr>>) -> Command {
        let mut cmd = assert_cmd::cargo::cargo_bin_cmd!("qmluic");
        cmd.current_dir(self.base_path())
            .env("NO_COLOR", "")
            .arg("generate-ui")
            .arg("--foreign-types")
            .arg(Path::new("contrib/metatypes").canonicalize().unwrap())
            .args(args);
        cmd
    }
}

pub fn dedent(data: impl AsRef<str>) -> String {
    let data = data.as_ref();
    let mut leader: String = data
        .chars()
        .take_while(|&c| c == '\n' || c == ' ')
        .collect();
    let data = &data[leader.len()..];
    if !leader.starts_with('\n') {
        leader.insert(0, '\n');
    }
    assert_eq!(leader.chars().filter(|&c| c == '\n').count(), 1);
    data.replace(&leader, "\n")
}

pub fn parse_doc(source: impl AsRef<str>) -> UiDocument {
    UiDocument::parse(dedent(source), "MyType", None)
}

pub fn translate_file(path: impl AsRef<Utf8Path>) -> Result<(String, String), String> {
    let doc = UiDocument::read(path).unwrap();
    translate_doc(&doc, DynamicBindingHandling::Generate)
}

pub fn translate_str(source: impl AsRef<str>) -> Result<String, String> {
    let doc = UiDocument::parse(dedent(source), "MyType", None);
    let (ui_xml, _) = translate_doc(&doc, DynamicBindingHandling::Reject)?;
    Ok(ui_xml)
}

pub fn translate_doc(
    doc: &UiDocument,
    dynamic_binding_handling: DynamicBindingHandling,
) -> Result<(String, String), String> {
    if doc.has_syntax_error() {
        return Err(format_reportable_diagnostics(
            doc,
            reporting::make_reportable_syntax_errors(&doc.collect_syntax_errors()),
        ));
    }

    let mut type_map = TypeMap::with_primitive_types();
    let mut classes = load_metatypes();
    metatype_tweak::apply_all(&mut classes);
    let mut module_data = ModuleData::with_builtins();
    module_data.extend(classes);
    type_map.insert_module(ModuleId::Named("qmluic.QtWidgets"), module_data);

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

    let ctx = BuildContext::prepare(
        &type_map,
        FileNameRules::default(),
        dynamic_binding_handling,
    )
    .unwrap();
    let mut diagnostics = Diagnostics::new();
    let (form, ui_support_opt) = match uigen::build(&ctx, doc, &mut diagnostics) {
        Some(x) if diagnostics.is_empty() => x,
        _ => {
            // may be only warnings, but we do want to test warning outputs
            return Err(format_reportable_diagnostics(
                doc,
                reporting::make_reportable_diagnostics(&diagnostics),
            ));
        }
    };
    let mut form_buf = Vec::new();
    form.serialize_to_xml(&mut XmlWriter::new_with_indent(&mut form_buf, b' ', 1))
        .unwrap();
    let mut ui_support_buf = Vec::new();
    if let Some(ui_support) = ui_support_opt {
        ui_support.write_header(&mut ui_support_buf).unwrap();
    }
    Ok((
        String::from_utf8(form_buf).unwrap(),
        String::from_utf8(ui_support_buf).unwrap(),
    ))
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

fn format_reportable_diagnostics(
    doc: &UiDocument,
    diagnostics: impl IntoIterator<Item = ReportableDiagnostic>,
) -> String {
    let mut buf = String::new();
    let config = term::Config::default();
    let files = SimpleFile::new(
        doc.path()
            .and_then(|p| p.file_name())
            .unwrap_or("<unknown>"),
        doc.source(),
    );
    for d in diagnostics {
        term::emit_to_string(&mut buf, &config, &files, &d).unwrap();
    }
    buf
}
