use qmluic::uigen::DynamicBindingHandling;

pub mod common;

#[test]
fn test_unknown_signal() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        onWhatever: ;
    }
    "###).unwrap_err(), @r###"
    error: unknown signal of class 'QWidget': whatever
      ┌─ <unknown>:3:5
      │
    3 │     onWhatever: ;
      │     ^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_invalid_binding_map_callback() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QDialog {
        onAccepted.whatever: ;
    }
    "###).unwrap_err(), @r###"
    error: signal callback cannot be a map
      ┌─ <unknown>:3:16
      │
    3 │     onAccepted.whatever: ;
      │                ^^^^^^^^
    "###);
}

#[test]
fn test_callback_without_dynamic_binding() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QDialog {
        onAccepted: ;
    }
    "###).unwrap_err(), @r###"
    error: signal callback cannot be translated without dynamic binding
      ┌─ <unknown>:3:5
      │
    3 │     onAccepted: ;
      │     ^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_root_connection() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
            onAccepted: ;
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_non_root_connection() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
            id: root
            QDialogButtonBox { onAccepted: root.accept() }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_default_argument_deduction() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
            id: root
            // clicked(bool checked = false)
            QPushButton { onClicked: root.close() }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_take_signal_parameter() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
            QLineEdit {
                id: edit
                onCursorPositionChanged: function(oldPos: int, newPos: int) {
                    label.text = "old: %1, new: %2".arg(oldPos).arg(newPos);
                }
            }
            QLabel { id: label }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_too_many_callback_parameters() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialogButtonBox {
            onClicked: (a: QAbstractButton, b: int) => {}
        }
        "###,
    );
    insta::assert_snapshot!(
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap_err(), @r###"
    error: too many callback arguments (expected: 0..1, actual: 2)
      ┌─ <unknown>:3:37
      │
    3 │     onClicked: (a: QAbstractButton, b: int) => {}
      │                                     ^^^^^^
    "###);
}

#[test]
fn test_incompatible_callback_parameters() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialogButtonBox {
            onClicked: (a: QPushButton) => {}
        }
        "###,
    );
    insta::assert_snapshot!(
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap_err(), @r###"
    error: incompatible callback arguments (expected: QAbstractButton*, actual: QPushButton*)
      ┌─ <unknown>:3:17
      │
    3 │     onClicked: (a: QPushButton) => {}
      │                 ^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_upcast_callback_parameters() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialogButtonBox {
            onClicked: (a: QWidget) => {}
        }
        "###,
    );
    assert!(common::translate_doc(&doc, DynamicBindingHandling::Generate).is_ok());
}

#[test]
fn test_redefine_callback_parameters() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QLineEdit {
            onCursorPositionChanged: (pos: int, pos: int) => {}
        }
        "###,
    );
    insta::assert_snapshot!(
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap_err(), @r###"
    error: redefinition of parameter: pos
      ┌─ <unknown>:3:41
      │
    3 │     onCursorPositionChanged: (pos: int, pos: int) => {}
      │                                         ^^^
    "###);
}

#[test]
fn test_callback_parameter_without_type_annotation() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QLineEdit {
            onCursorPositionChanged: (pos) => {}
        }
        "###,
    );
    insta::assert_snapshot!(
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap_err(), @r###"
    error: function parameter must have type annotation
      ┌─ <unknown>:3:31
      │
    3 │     onCursorPositionChanged: (pos) => {}
      │                               ^^^
    "###);
}

#[test]
fn test_callback_parameter_of_void_type() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QLineEdit {
            onCursorPositionChanged: (pos: void) => {}
        }
        "###,
    );
    insta::assert_snapshot!(
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap_err(), @r###"
    error: operation 'function parameter' on unsupported type: void
      ┌─ <unknown>:3:31
      │
    3 │     onCursorPositionChanged: (pos: void) => {}
      │                               ^^^^^^^^^
    "###);
}

#[test]
fn test_method_call_bad_arg_count() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QDialog {
        id: root
        QDialogButtonBox { onAccepted: root.accept(0) }
    }
    "###).unwrap_err(), @r###"
    error: invalid argument: expects (), but got (integer)
      ┌─ <unknown>:4:36
      │
    4 │     QDialogButtonBox { onAccepted: root.accept(0) }
      │                                    ^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_method_call_bad_arg_type() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QDialog {
        id: root
        QDialogButtonBox { onAccepted: root.done("whatever") }
    }
    "###).unwrap_err(), @r###"
    error: invalid argument: expects (int), but got (QString)
      ┌─ <unknown>:4:36
      │
    4 │     QDialogButtonBox { onAccepted: root.done("whatever") }
      │                                    ^^^^^^^^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_implicit_this_method_call() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
            QPushButton { onClicked: hide() }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_explicit_this_method_call() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
            QPushButton { onClicked: this.hide() }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_property_assignment() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
            id: root
            QPushButton { onClicked: root.windowTitle = "clicked" }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_property_assignment_incompatible_type() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QDialog {
        id: root
        QPushButton { onClicked: root.windowTitle = 1 }
    }
    "###).unwrap_err(), @r###"
    error: operation '=' on incompatible types: QString and integer
      ┌─ <unknown>:4:30
      │
    4 │     QPushButton { onClicked: root.windowTitle = 1 }
      │                              ^^^^^^^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_property_assignment_readonly() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QDialog {
        id: root
        QPushButton { onClicked: root.width = 1 }
    }
    "###).unwrap_err(), @r###"
    error: not a writable property
      ┌─ <unknown>:4:30
      │
    4 │     QPushButton { onClicked: root.width = 1 }
      │                              ^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_multiple_statements() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
            id: root
            QPushButton {
                onClicked: {
                    root.showMaximized();
                    root.raise();
                }
            }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_let_object() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QPushButton {
            id: button
            onClicked: {
                let b = button;
                b.text = "clicked";
            }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_if_else_complete() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QCheckBox {
            text: if (checked) { "checked" } else { "unchecked" }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_variant_cast() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QComboBox {
            onCurrentTextChanged: {
                windowTitle = currentData as QString;
            }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}
