pub mod common;

#[test]
fn test_brush() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QGraphicsView {
        backgroundBrush.color: "#123abc"
        backgroundBrush.style: Qt.Dense4Pattern
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QGraphicsView">
      <property name="backgroundBrush">
       <brush brushstyle="Dense4Pattern">
        <color>
         <blue>188</blue>
         <green>58</green>
         <red>18</red>
        </color>
       </brush>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_brush_solid_color() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QGraphicsView {
        backgroundBrush: "#80123abc"
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QGraphicsView">
      <property name="backgroundBrush">
       <brush brushstyle="SolidPattern">
        <color alpha="128">
         <blue>188</blue>
         <green>58</green>
         <red>18</red>
        </color>
       </brush>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_color() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QColorDialog { currentColor: "#123abc" }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QColorDialog">
      <property name="currentColor">
       <color>
        <blue>188</blue>
        <green>58</green>
        <red>18</red>
       </color>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_color_alpha() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QColorDialog { currentColor: "#80123abc" }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QColorDialog">
      <property name="currentColor">
       <color alpha="128">
        <blue>188</blue>
        <green>58</green>
        <red>18</red>
       </color>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_invalid_color() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QColorDialog { currentColor: "#wtf" }
    "###).unwrap_err(), @"<unknown>:2:30: error: invalid hex color");
}

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
