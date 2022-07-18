use qmluic::uigen::DynamicBindingHandling;

pub mod common;

#[test]
fn test_unknown_signal() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        onWhatever: 0
    }
    "###).unwrap_err(), @r###"
    error: unknown signal of class 'QWidget': whatever
      ┌─ <unknown>:3:5
      │
    3 │     onWhatever: 0
      │     ^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_invalid_binding_map_callback() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QDialog {
        onAccepted.whatever: 0
    }
    "###).unwrap_err(), @r###"
    error: signal callback cannot be a map
      ┌─ <unknown>:3:16
      │
    3 │     onAccepted.whatever: 0
      │                ^^^^^^^^
    "###);
}

#[test]
fn test_callback_without_dynamic_binding() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QDialog {
        onAccepted: 0
    }
    "###).unwrap_err(), @r###"
    error: signal callback cannot be translated without dynamic binding
      ┌─ <unknown>:3:5
      │
    3 │     onAccepted: 0
      │     ^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_root_connection() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QDialog {
            onAccepted: 0
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
