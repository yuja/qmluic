pub mod common;

#[test]
fn test_gadget_properties() {
    insta::assert_snapshot!(common::translate_file("examples/GadgetProperties.qml").unwrap());
}

#[test]
fn test_item_views() {
    insta::assert_snapshot!(common::translate_file("examples/ItemViews.qml").unwrap());
}

#[test]
fn test_layout_flow() {
    insta::assert_snapshot!(common::translate_file("examples/LayoutFlow.qml").unwrap());
}

#[test]
fn test_main_window() {
    insta::assert_snapshot!(common::translate_file("examples/MainWindow.qml").unwrap());
}

#[test]
fn test_settings_dialog() {
    insta::assert_snapshot!(common::translate_file("examples/SettingsDialog.qml").unwrap());
}

#[test]
fn test_static_item_model() {
    insta::assert_snapshot!(common::translate_file("examples/StaticItemModel.qml").unwrap());
}

#[test]
fn test_various_layouts() {
    insta::assert_snapshot!(common::translate_file("examples/VariousLayouts.qml").unwrap());
}

#[test]
fn test_customwidget_common_my_dialog_button_box() {
    insta::assert_snapshot!(common::translate_file(
        "examples/customwidget/common/MyDialogButtonBox.qml"
    )
    .unwrap());
}

#[test]
fn test_customwidget_main_dialog() {
    insta::assert_snapshot!(
        common::translate_file("examples/customwidget/MainDialog.qml").unwrap()
    );
}

#[test]
fn test_customwidget_settings_form() {
    insta::assert_snapshot!(
        common::translate_file("examples/customwidget/SettingsForm.qml").unwrap()
    );
}

#[test]
fn test_thg_hg_email_dialog() {
    insta::assert_snapshot!(common::translate_file("examples/thg/HgEmailDialog.qml").unwrap());
}
