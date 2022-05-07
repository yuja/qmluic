use qmluic::metatype;
use qmluic::qmlast::UiDocument;
use qmluic::typemap::TypeMap;
use qmluic_cli::UiBuilder;
use std::fs;
use std::path::Path;

#[test]
fn test_translate_example() {
    insta::assert_snapshot!(translate_file(
        "examples/SettingsDialog.qml",
        "SettingsDialog"
    ));
    insta::assert_snapshot!(translate_file("examples/MainWindow.qml", "MainWindow"));
}

fn translate_file(path: impl AsRef<Path>, class_name: impl AsRef<str>) -> String {
    let mut type_map = TypeMap::with_primitive_types();
    type_map.extend(load_metatypes());
    let doc = UiDocument::read(path).unwrap();
    let mut buf = Vec::new();
    UiBuilder::new(&mut buf, &type_map, &doc, class_name)
        .build()
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
