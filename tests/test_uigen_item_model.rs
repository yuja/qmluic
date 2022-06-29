pub mod common;

#[test]
fn test_string_list_as_combo_box_item() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QComboBox { model: ["foo", qsTr("bar")] }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QComboBox" name="comboBox">
      <item>
       <property name="text">
        <string notr="true">foo</string>
       </property>
      </item>
      <item>
       <property name="text">
        <string>bar</string>
       </property>
      </item>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_empty_list_as_combo_box_item() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QComboBox { model: [] }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QComboBox" name="comboBox">
     </widget>
    </ui>
    "###);
}

#[test]
fn test_string_list_as_list_widget_item() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QListWidget { model: ["foo"] }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QListWidget" name="listWidget">
      <item>
       <property name="text">
        <string notr="true">foo</string>
       </property>
      </item>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_string_list_as_list_view_item() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QListView { model: ["foo"] }
    "###).unwrap_err(), @"<unknown>:2:13: error: not a writable property");
}
