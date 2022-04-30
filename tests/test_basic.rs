use qmluic::qml::UiDocument;
use qmluic_cli::UiBuilder;
use std::fs;
use std::path::Path;

#[test]
fn test_translate_example() {
    insta::assert_snapshot!(translate_file(
        "examples/SettingsDialog.qml",
        "SettingsDialog"
    ));
}

fn translate_file(path: impl AsRef<Path>, class_name: impl AsRef<str>) -> String {
    let doc = UiDocument::with_source(fs::read_to_string(path).unwrap());
    let mut buf = Vec::new();
    UiBuilder::new(&mut buf, &doc, class_name).build().unwrap();
    String::from_utf8(buf).unwrap()
}
