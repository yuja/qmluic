use qmluic::uigen::DynamicBindingHandling;

pub mod common;

#[test]
fn test_root_must_be_widget() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QVBoxLayout {}
    "###).unwrap_err(), @"<unknown>:2:1: error: class 'QVBoxLayout' is not a QWidget");
}

#[test]
fn test_reserved_word_workaround() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QPushButton { default_: true }
    "###).unwrap(), @r###"
    <ui version="4.0">
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
    "###).unwrap_err(), @"<unknown>:4:25: error: unsupported dynamic binding");
}

#[test]
fn test_self_property_binding_unsupported() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QCheckBox { checked: enabled }
    "###).unwrap_err(), @"<unknown>:2:22: error: undefined reference");
}

#[test]
fn test_dynamic_binding_type_mismatch() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
         windowTitle: source.checked
         QCheckBox { id: source }
    }
    "###).unwrap_err(), @"<unknown>:3:19: error: expression type mismatch (expected: QString, actual: bool)");
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
    insta::assert_snapshot!(common::translate_doc(&doc, DynamicBindingHandling::Omit).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QWidget" name="widget1">
      <widget class="QCheckBox" name="source">
      </widget>
      <widget class="QWidget" name="widget">
      </widget>
     </widget>
    </ui>
    "###);
}
