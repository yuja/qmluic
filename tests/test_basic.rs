pub mod common;

#[test]
fn test_translate_example() {
    insta::assert_snapshot!(common::translate_file("examples/SettingsDialog.qml").unwrap());
    insta::assert_snapshot!(common::translate_file("examples/MainWindow.qml").unwrap());
    insta::assert_snapshot!(common::translate_file("examples/VariousLayouts.qml").unwrap());
    insta::assert_snapshot!(common::translate_file("examples/LayoutFlow.qml").unwrap());
    insta::assert_snapshot!(common::translate_file("examples/GadgetProperties.qml").unwrap());
    insta::assert_snapshot!(common::translate_file("examples/thg/HgEmailDialog.qml").unwrap());
}
