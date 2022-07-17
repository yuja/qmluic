pub mod common;

#[test]
fn test_unknown_attached_type() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        Whatever.property: 1
    }
    "###).unwrap_err(), @r###"
    error: unknown attaching type: Whatever
      ┌─ <unknown>:3:5
      │
    3 │     Whatever.property: 1
      │     ^^^^^^^^
    "###);
}

#[test]
fn test_no_attached_type() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        QWidget.property: 1
    }
    "###).unwrap_err(), @r###"
    error: no attached type for type: QWidget
      ┌─ <unknown>:3:5
      │
    3 │     QWidget.property: 1
      │     ^^^^^^^
    "###);
}

#[test]
fn test_enum_as_attached_type() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        Qt.Alignment.property: 1
    }
    "###).unwrap_err(), @r###"
    error: invalid attaching type: Qt::Alignment
      ┌─ <unknown>:3:5
      │
    3 │     Qt.Alignment.property: 1
      │     ^^^^^^^^^^^^
    "###);
}

#[test]
fn test_unused_attached() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        QTabWidget.title: "Hello"
    }
    "###).unwrap_err(), @r###"
    error: unused or unsupported dynamic binding to attached property
      ┌─ <unknown>:3:5
      │
    3 │     QTabWidget.title: "Hello"
      │     ^^^^^^^^^^^^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_partially_used_layout_attached() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        QVBoxLayout {
            QWidget {
                QLayout.columnStretch: 1
                QLayout.rowStretch: 2
            }
        }
    }
    "###).unwrap_err(), @r###"
    error: unused or unsupported dynamic binding to attached property
      ┌─ <unknown>:5:13
      │
    5 │             QLayout.columnStretch: 1
      │             ^^^^^^^^^^^^^^^^^^^^^^^^
    "###);
}

#[test]
fn test_tab_widget() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QTabWidget {
        QWidget {
            QTabWidget.title: "Hello"
            QTabWidget.toolTip: "Hello world!"
        }
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QTabWidget" name="tabWidget">
      <widget class="QWidget" name="widget">
       <attribute name="title">
        <string notr="true">Hello</string>
       </attribute>
       <attribute name="toolTip">
        <string notr="true">Hello world!</string>
       </attribute>
      </widget>
     </widget>
    </ui>
    "###);
}
