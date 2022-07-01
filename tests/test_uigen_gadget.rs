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
     <class>MyType</class>
     <widget class="QGraphicsView" name="graphicsView">
      <property name="backgroundBrush">
       <brush brushstyle="Dense4Pattern">
        <color alpha="255">
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
     <class>MyType</class>
     <widget class="QGraphicsView" name="graphicsView">
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
     <class>MyType</class>
     <widget class="QColorDialog" name="colorDialog">
      <property name="currentColor">
       <color alpha="255">
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
     <class>MyType</class>
     <widget class="QColorDialog" name="colorDialog">
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
     <class>MyType</class>
     <widget class="QWidget" name="widget">
      <property name="cursor">
       <cursorShape>IBeamCursor</cursorShape>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_font() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QLabel {
        font.family: "Monospace"
        font.pointSize: 9
        font.weight: 75
        font.italic: false
        font.bold: true
        font.strikeout: true
        font.styleStrategy: QFont.PreferDefault
        font.kerning: true
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QLabel" name="label">
      <property name="font">
       <font>
        <bold>true</bold>
        <family notr="true">Monospace</family>
        <italic>false</italic>
        <kerning>true</kerning>
        <pointsize>9</pointsize>
        <strikeout>true</strikeout>
        <stylestrategy>PreferDefault</stylestrategy>
        <weight>75</weight>
       </font>
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
     <class>MyType</class>
     <widget class="QToolButton" name="toolButton">
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
     <class>MyType</class>
     <widget class="QToolButton" name="toolButton">
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
fn test_palette_color_group() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        palette.active { window: "black"; windowText: "white" }
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QWidget" name="widget">
      <property name="palette">
       <palette>
        <active>
         <colorrole role="Window">
          <brush brushstyle="SolidPattern">
           <color alpha="255">
            <blue>0</blue>
            <green>0</green>
            <red>0</red>
           </color>
          </brush>
         </colorrole>
         <colorrole role="WindowText">
          <brush brushstyle="SolidPattern">
           <color alpha="255">
            <blue>255</blue>
            <green>255</green>
            <red>255</red>
           </color>
          </brush>
         </colorrole>
        </active>
        <disabled>
        </disabled>
        <inactive>
        </inactive>
       </palette>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_palette_default_role() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        palette.window: "black"
        palette.disabled.window: "gray"
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QWidget" name="widget">
      <property name="palette">
       <palette>
        <active>
         <colorrole role="Window">
          <brush brushstyle="SolidPattern">
           <color alpha="255">
            <blue>0</blue>
            <green>0</green>
            <red>0</red>
           </color>
          </brush>
         </colorrole>
        </active>
        <disabled>
         <colorrole role="Window">
          <brush brushstyle="SolidPattern">
           <color alpha="255">
            <blue>128</blue>
            <green>128</green>
            <red>128</red>
           </color>
          </brush>
         </colorrole>
        </disabled>
        <inactive>
         <colorrole role="Window">
          <brush brushstyle="SolidPattern">
           <color alpha="255">
            <blue>0</blue>
            <green>0</green>
            <red>0</red>
           </color>
          </brush>
         </colorrole>
        </inactive>
       </palette>
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
     <class>MyType</class>
     <widget class="QLabel" name="label">
      <property name="pixmap">
       <pixmap>:/a.png</pixmap>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_size_policy() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        sizePolicy.horizontalPolicy: QSizePolicy.Preferred
        sizePolicy.verticalPolicy: QSizePolicy.Expanding
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QWidget" name="widget">
      <property name="sizePolicy">
       <sizepolicy hsizetype="Preferred" vsizetype="Expanding">
       </sizepolicy>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_size_policy_with_stretch() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        sizePolicy.horizontalPolicy: QSizePolicy.Preferred
        sizePolicy.verticalPolicy: QSizePolicy.Expanding
        sizePolicy.horizontalStretch: 2
        sizePolicy.verticalStretch: 1
    }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QWidget" name="widget">
      <property name="sizePolicy">
       <sizepolicy hsizetype="Preferred" vsizetype="Expanding">
        <horstretch>2</horstretch>
        <verstretch>1</verstretch>
       </sizepolicy>
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

#[test]
fn test_stretch_without_size_policy() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        sizePolicy.horizontalStretch: 2
    }
    "###).unwrap_err(), @"<unknown>:3:5: error: cannot specify stretch without horizontal and vertical policies");
}

#[test]
fn test_string_list_tr() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QTextBrowser { searchPaths: [qsTr("a"), qsTr("b")] }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QTextBrowser" name="textBrowser">
      <property name="searchPaths">
       <stringlist>
        <string>a</string>
        <string>b</string>
       </stringlist>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_string_list_notr() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QTextBrowser { searchPaths: ["a", "b"] }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QTextBrowser" name="textBrowser">
      <property name="searchPaths">
       <stringlist notr="true">
        <string>a</string>
        <string>b</string>
       </stringlist>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_string_list_empty() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QTextBrowser { searchPaths: [] }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <class>MyType</class>
     <widget class="QTextBrowser" name="textBrowser">
      <property name="searchPaths">
       <stringlist notr="true">
       </stringlist>
      </property>
     </widget>
    </ui>
    "###);
}

#[test]
fn test_string_list_mixed() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QTextBrowser { searchPaths: [qsTr("a"), "b"] }
    "###).unwrap_err(), @"<unknown>:2:29: error: cannot mix bare and translatable strings");
}
