use qmluic::uigen::DynamicBindingHandling;

pub mod common;

#[test]
fn test_root_must_be_widget() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QVBoxLayout {}
    "###).unwrap_err(), @r###"
    error: class 'QVBoxLayout' is not a QWidget
      ┌─ <unknown>:2:1
      │
    2 │ QVBoxLayout {}
      │ ^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_reserved_word_workaround() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QPushButton { default_: true }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QPushButton" name="pushButton">
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
    "###).unwrap_err(), @r###"
    error: not a writable property
      ┌─ <unknown>:2:11
      │
    2 │ QWidget { width: 100 }
      │           ^^^^^^^^^^
    "###);
}

#[test]
fn test_dynamic_width_is_readonly() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
            width: spinBox.value
            QSpinBox { id: spinBox }
        }
        "###,
    );
    insta::assert_snapshot!(
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap_err(),
        @r###"
    error: not a writable property
      ┌─ <unknown>:3:5
      │
    3 │     width: spinBox.value
      │     ^^^^^^^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_unobservable_property() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
             id: root
             QLabel { text: "width: %1".arg(root.width) }
        }
        "###,
    );
    insta::assert_snapshot!(
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap_err(),
        @r###"
    error: unobservable property: width
      ┌─ <unknown>:4:37
      │
    4 │      QLabel { text: "width: %1".arg(root.width) }
      │                                     ^^^^
    "###);
}

#[test]
fn test_assign_double() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QDoubleSpinBox { value: 1.0 }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QDoubleSpinBox" name="doubleSpinBox">
      <property name="value">
       <number>1</number>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_assign_qreal() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QDial { notchTarget: 3.7 }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QDial" name="dial">
      <property name="notchTarget">
       <number>3.7</number>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_assign_float_to_int() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QSpinBox { value: 1.0 }
    "###).unwrap_err(), @r###"
    error: expression type mismatch (expected: int, actual: double)
      ┌─ <unknown>:2:19
      │
    2 │ QSpinBox { value: 1.0 }
      │                   ^^^
    "###);
}

#[test]
fn test_object_property_binding() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
             QCheckBox { id: source }
             QWidget { visible: source.checked }
        }
        "###,
    );
    let (ui_xml, ui_support_h) =
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_object_property_binding_to_root() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
             windowTitle: edit.text
             QLineEdit { id: edit }
        }
        "###,
    );
    let (ui_xml, ui_support_h) =
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_xml);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_object_property_binding_unsupported() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
         QCheckBox { id: source }
         QWidget { visible: source.checked }
    }
    "###).unwrap_err(), @r###"
    error: unsupported dynamic binding
      ┌─ <unknown>:4:25
      │
    4 │      QWidget { visible: source.checked }
      │                         ^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_self_property_binding_unsupported() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QCheckBox { checked: enabled }
    "###).unwrap_err(), @r###"
    error: undefined reference
      ┌─ <unknown>:2:22
      │
    2 │ QCheckBox { checked: enabled }
      │                      ^^^^^^^
    "###);
}

#[test]
fn test_dynamic_binding_type_mismatch() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
         windowTitle: source.checked
         QCheckBox { id: source }
    }
    "###).unwrap_err(), @r###"
    error: expression type mismatch (expected: QString, actual: bool)
      ┌─ <unknown>:3:19
      │
    3 │      windowTitle: source.checked
      │                   ^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_omit_dynamic_binding() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
             QCheckBox { id: source }
             QWidget { visible: source.checked }
        }
        "###,
    );
    let (ui_xml, _) = common::translate_doc(&doc, DynamicBindingHandling::Omit).unwrap();
    insta::assert_snapshot!(ui_xml, @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QWidget" name="widget1">
      <widget class="QCheckBox" name="source">
      </widget>
      <widget class="QWidget" name="widget">
      </widget>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_deduplicate_dynamic_binding_senders() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
             windowTitle: combo.currentIndex === 0 ? "-" : combo.currentText
             QComboBox { id: combo }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_deduplicate_dynamic_binding_connections() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
             windowTitle: edit.text + edit.text
             QLineEdit { id: edit }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_deduplicate_dynamic_binding_sender_receiver() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QLineEdit {
            id: edit
            enabled: edit.text === ""
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_ternary_expression_type_mismatch() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
         windowTitle: source.checked ? 1 : "whatever"
         QCheckBox { id: source }
    }
    "###).unwrap_err(), @r###"
    error: operation 'ternary' on incompatible types: integer and QString
      ┌─ <unknown>:3:19
      │
    3 │      windowTitle: source.checked ? 1 : "whatever"
      │                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_qstring_arg() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
            QLabel { text: "dynamic %1".arg(combo.currentIndex) }
            QLabel { text: "static %1".arg(1) }
            QComboBox { id: combo }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_unique_binding_method_name() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
            id: main
            windowTitle: edit1.text  // "main" + "WindowTitle"
            QGroupBox {
                id: mainWindow
                title: edit2.text  // "mainWindow" + "Title"
            }
            QLineEdit { id: edit1 }
            QLineEdit { id: edit2 }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}
