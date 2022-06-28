pub mod common;

#[test]
fn test_reserved_word_workaround() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QPushButton { default_: true }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QPushButton">
      <property name="default">
       <bool>true</bool>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_width_is_readonly() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget { width: 100 }
    "###).unwrap_err(), @"<unknown>:2:11: error: not a writable property");
}

#[test]
fn test_object_property_binding_unsupported() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
         QCheckBox { id: source }
         QWidget { visible: source.checked }
    }
    "###).unwrap_err(), @"<unknown>:4:32: error: unsupported object property resolution");
}

#[test]
fn test_self_property_binding_unsupported() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QCheckBox { checked: enabled }
    "###).unwrap_err(), @"<unknown>:2:22: error: undefined reference");
}
