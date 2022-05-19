use qmluic::diagnostic::Diagnostics;
use qmluic::metatype;
use qmluic::qmlast::UiDocument;
use qmluic::typemap::TypeMap;
use qmluic::uigen::XmlWriter;
use qmluic_cli::UiBuilder;
use std::fs;
use std::path::Path;

#[test]
fn test_translate_example() {
    insta::assert_snapshot!(translate_file("examples/SettingsDialog.qml",));
    insta::assert_snapshot!(translate_file("examples/MainWindow.qml"));
    insta::assert_snapshot!(translate_file("examples/VariousLayouts.qml"));
}

fn translate_file(path: impl AsRef<Path>) -> String {
    let mut type_map = TypeMap::with_primitive_types();
    type_map.extend(load_metatypes());
    let doc = UiDocument::read(path).unwrap();
    let mut diagnostics = Diagnostics::new();
    let form = UiBuilder::new(&type_map, &doc, &mut diagnostics)
        .build()
        .unwrap();
    let mut buf = Vec::new();
    form.serialize_to_xml(&mut XmlWriter::new_with_indent(&mut buf, b' ', 1))
        .unwrap();
    String::from_utf8(buf).unwrap()
}

fn load_metatypes() -> Vec<metatype::Class> {
    let paths = [
        "contrib/metatypes/qmluic_metatypes.json",
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
