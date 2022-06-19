use camino::Utf8PathBuf;
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

#[test]
fn test_translate_item_views_example() {
    insta::assert_snapshot!(common::translate_file("examples/ItemViews.qml").unwrap());
}

#[test]
fn test_translate_static_item_model_example() {
    insta::assert_snapshot!(common::translate_file("examples/StaticItemModel.qml").unwrap());
}

#[test]
fn test_translate_customwidget_example() {
    let base = Utf8PathBuf::from("examples/customwidget");
    insta::assert_snapshot!(common::translate_file(base.join("MainDialog.qml")).unwrap());
    insta::assert_snapshot!(common::translate_file(base.join("SettingsForm.qml")).unwrap());
    insta::assert_snapshot!(
        common::translate_file(base.join("common/MyDialogButtonBox.qml")).unwrap()
    );
}
