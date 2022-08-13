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
fn test_duplicated_id() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        QWidget { id: foo }
        QWidget { id: foo }
    }
    "###).unwrap_err(), @r###"
    error: duplicated object id: foo
      ┌─ <unknown>:4:19
      │
    3 │     QWidget { id: foo }
      │                   --- id is first defined here
    4 │     QWidget { id: foo }
      │                   ^^^ duplicated id is defined here
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
fn test_incompatible_return_type() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
            windowTitle: {
                switch (edit.text) {
                case "hello":
                    return "world";
                }
            }
            QLineEdit { id: edit }
        }
        "###,
    );
    insta::assert_snapshot!(
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap_err(), @r###"
    error: cannot deduce return type from 'QString' and 'void'
      ┌─ <unknown>:8:6
      │
    6 │             return "world";
      │                    ------- type: QString
    7 │         }
    8 │     }
      │      ^ type: void
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
fn test_implicit_this_property_binding() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QCheckBox { checked: windowTitle === "" }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_explicit_this_property_binding() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QCheckBox { checked: this.windowTitle === "" }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_indirect_property_binding() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
            QCheckBox { id: source }
            QWidget {
                visible: {
                    let w = source;
                    w.checked
                }
            }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_dynamic_property_source() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
            windowTitle: (source.checked ? edit1 : edit2).text
            QCheckBox { id: source }
            QLineEdit { id: edit1 }
            QLineEdit { id: edit2 }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_dynamic_binding_type_mismatch() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
             windowTitle: source.checked
             QCheckBox { id: source }
        }
        "###,
    );
    insta::assert_snapshot!(
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap_err(), @r###"
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
fn test_dynamic_binding_with_static_cast() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
            QDoubleSpinBox { id: edit }
            QComboBox { currentIndex: edit.value as int }
        }
        "###,
    );
    let (_, ui_support_h) = common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_integer_arithmetic_type_mismatch() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
         windowTitle: "sum: %1".arg(edit1.value + edit2.value)
         QSpinBox { id: edit1 }
         QDoubleSpinBox { id: edit2 }
    }
    "###).unwrap_err(), @r###"
    error: operation '+' on incompatible types: int and double
      ┌─ <unknown>:3:33
      │
    3 │      windowTitle: "sum: %1".arg(edit1.value + edit2.value)
      │                                 -----------   ----------- type: double
      │                                 │              
      │                                 type: int
      │
      = use (expr as double) or (expr as int) for numeric cast
    "###);
}

#[test]
fn test_object_comparison_type_mismatch() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
         visible: combo !== edit
         QComboBox { id: combo }
         QLineEdit { id: edit }
    }
    "###).unwrap_err(), @r###"
    error: operation '!=' on incompatible types: QComboBox* and QLineEdit*
      ┌─ <unknown>:3:15
      │
    3 │      visible: combo !== edit
      │               -----     ---- type: QLineEdit*
      │               │          
      │               type: QComboBox*
      │
      = use (expr as QWidget) to upcast to base class
    "###);
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
      ┌─ <unknown>:3:36
      │
    3 │      windowTitle: source.checked ? 1 : "whatever"
      │                                    -   ---------- type: QString
      │                                    │    
      │                                    type: integer
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
fn test_nullable_property_sources() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QLabel {
            text: {
                let w = check.checked ? edit : null;
                w !== null ? w.text : ""
            }
            QCheckBox { id: check }
            QLineEdit { id: edit }
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

#[test]
fn test_dynamic_tab_widget_attached() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QTabWidget {
            QWidget {
                id: tab
                QTabWidget.title: tab.windowTitle
            }
        }
        "###,
    );
    insta::assert_snapshot!(
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap_err(), @r###"
    error: unused or unsupported dynamic binding to attached property
      ┌─ <unknown>:5:9
      │
    5 │         QTabWidget.title: tab.windowTitle
      │         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_static_actions_list() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QMenu {
        actions: [act, menu.menuAction(), sep]
        QAction { id: act }
        QMenu { id: menu }
        QAction { id: sep; separator: true }
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QMenu" name="menu1">
      <addaction name="act"/>
      <addaction name="menu"/>
      <addaction name="separator"/>
      <action name="act">
      </action>
      <widget class="QMenu" name="menu">
      </widget>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_actions_list_incompatible_type() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QMenu {
        actions: [
            act1,
            act2,
            menu,
        ]
        QAction { id: act1 }
        QAction { id: act2 }
        QMenu { id: menu }
    }
    "###).unwrap_err(), @r###"
    error: incompatible array element types at index 2: QAction* and QMenu*
      ┌─ <unknown>:5:9
      │
    5 │         act2,
      │         ---- type: QAction*
    6 │         menu,
      │         ---- type: QMenu*
      │
      = call .menuAction() to obtain QAction* associated with menu
    "###);
}

#[test]
fn test_action_separator_false() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QMenu {
        QAction { separator: false }
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QMenu" name="menu">
      <addaction name="action"/>
      <action name="action">
      </action>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_action_seaparator_dynamic() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
            QCheckBox { id: check }
            QAction { separator: check.checked }
        }
        "###,
    );
    let (ui_xml, ui_support_h) =
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_xml, @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QWidget" name="widget">
      <addaction name="action"/>
      <widget class="QCheckBox" name="check">
      </widget>
      <action name="action">
      </action>
     </widget>
    </ui>
    "###);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_action_seaparator_with_other_properties() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
            QAction { separator: true; text: "whatever" }
        }
        "###,
    );
    let (ui_xml, ui_support_h) =
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_xml, @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QWidget" name="widget">
      <addaction name="action"/>
      <action name="action">
       <property name="text">
        <string notr="true">whatever</string>
       </property>
      </action>
     </widget>
    </ui>
    "###);
    insta::assert_snapshot!(ui_support_h);
}

#[test]
fn test_action_seaparator_with_callback() {
    let doc = common::parse_doc(
        r###"
        import qmluic.QtWidgets
        QWidget {
            QAction { separator: true; onHovered: {} }
        }
        "###,
    );
    let (ui_xml, ui_support_h) =
        common::translate_doc(&doc, DynamicBindingHandling::Generate).unwrap();
    insta::assert_snapshot!(ui_xml, @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QWidget" name="widget">
      <addaction name="action"/>
      <action name="action">
      </action>
     </widget>
    </ui>
    "###);
    insta::assert_snapshot!(ui_support_h);
}
