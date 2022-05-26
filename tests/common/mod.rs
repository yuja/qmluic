use qmluic::diagnostic::Diagnostics;
use qmluic::metatype;
use qmluic::metatype_tweak;
use qmluic::qmlast::UiDocument;
use qmluic::typemap::TypeMap;
use qmluic::uigen::{self, BuildContext, XmlWriter};
use std::fs;
use std::path::Path;

pub fn translate_file(path: impl AsRef<Path>) -> Result<String, Diagnostics> {
    let doc = UiDocument::read(path).unwrap();
    translate_doc(&doc)
}

pub fn translate_str(source: impl Into<String>) -> Result<String, Diagnostics> {
    let doc = UiDocument::parse(source, None);
    translate_doc(&doc)
}

fn translate_doc(doc: &UiDocument) -> Result<String, Diagnostics> {
    assert!(!doc.has_syntax_error());
    let mut type_map = TypeMap::with_primitive_types();
    let mut classes = load_metatypes();
    metatype_tweak::apply_all(&mut classes);
    type_map.extend(classes);
    let ctx = BuildContext::prepare(&type_map, doc).unwrap();
    let mut diagnostics = Diagnostics::new();
    let form = match uigen::build(&ctx, doc, &mut diagnostics) {
        Some(form) if diagnostics.is_empty() => form,
        _ => return Err(diagnostics),
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
