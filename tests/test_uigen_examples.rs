pub mod common;

#[test]
fn test_binding_loop() {
    let (ui_xml, ui_support_h) = common::translate_file("examples/BindingLoop.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_hg_email_dialog() {
    let (ui_xml, ui_support_h) = common::translate_file("examples/HgEmailDialog.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_item_views() {
    let (ui_xml, ui_support_h) = common::translate_file("examples/ItemViews.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_layout_flow() {
    let (ui_xml, ui_support_h) = common::translate_file("examples/LayoutFlow.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_main_window() {
    let (ui_xml, ui_support_h) = common::translate_file("examples/MainWindow.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_settings_dialog() {
    let (ui_xml, ui_support_h) = common::translate_file("examples/SettingsDialog.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_static_item_model() {
    let (ui_xml, ui_support_h) = common::translate_file("examples/StaticItemModel.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_various_layouts() {
    let (ui_xml, ui_support_h) = common::translate_file("examples/VariousLayouts.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_customwidget_common_my_dialog_button_box() {
    let (ui_xml, ui_support_h) =
        common::translate_file("examples/customwidget/common/MyDialogButtonBox.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_customwidget_main_dialog() {
    let (ui_xml, ui_support_h) =
        common::translate_file("examples/customwidget/MainDialog.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_customwidget_settings_form() {
    let (ui_xml, ui_support_h) =
        common::translate_file("examples/customwidget/SettingsForm.qml").unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}
