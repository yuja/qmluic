pub mod common;

#[test]
fn test_pixmap() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QLabel { pixmap: ":/a.png" }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QLabel">
      <property name="pixmap">
       <pixmap>:/a.png</pixmap>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_unpaired_size_policy() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        sizePolicy.horizontalPolicy: QSizePolicy.Expanding
    }
    "###).unwrap_err(), @"<unknown>:4:9: error: both horizontal and vertical policies must be specified");
}
