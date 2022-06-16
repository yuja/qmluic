pub mod common;

#[test]
fn test_cursor() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget { cursor: Qt.IBeamCursor }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QWidget">
      <property name="cursor">
       <cursorShape>IBeamCursor</cursorShape>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_icon_themed() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QToolButton { icon.name: "edit-copy" }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QToolButton">
      <property name="icon">
       <iconset theme="edit-copy">
       </iconset>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_icon_state_pixmaps() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QToolButton {
        icon.normalOff: "normal-off.png"
        icon.normalOn: "normal-on.png"
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QToolButton">
      <property name="icon">
       <iconset>
        <normaloff>normal-off.png</normaloff>
        <normalon>normal-on.png</normalon>
       </iconset>
      </property>
     </widget>
    </ui>
    "###);
}

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
    "###).unwrap_err(), @"<unknown>:3:5: error: both horizontal and vertical policies must be specified");
}
