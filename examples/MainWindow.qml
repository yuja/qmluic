// MainWindow.qml

// <ui version="4.0">
//  <class>MainWindow</class>

import qmluic.QtWidgets

//  <widget class="QMainWindow" name="MainWindow">
QMainWindow {
    //   <property name="geometry">
    //    <rect>
    //     <x>0</x>
    //     <y>0</y>
    //     <width>800</width>
    //     <height>600</height>
    //    </rect>
    //   </property>
    geometry { x: 0; y: 0; width: 800; height: 600 }

    //   <property name="windowTitle">
    //    <string>MainWindow</string>
    //   </property>
    windowTitle: qsTr("MainWindow")

    //   <widget class="QWidget" name="centralwidget"/>
    QWidget { id: centralwidget }

    //   <widget class="QMenuBar" name="menubar">
    QMenuBar {
        id: menubar
        //    <property name="geometry">
        //     <rect>
        //      <x>0</x>
        //      <y>0</y>
        //      <width>800</width>
        //      <height>20</height>
        //     </rect>
        //    </property>
        geometry { x: 0; y: 0; width: 800; height: 20 }

        //    <widget class="QMenu" name="menu_File">
        QMenu {
            id: menu_File

            //     <property name="title">
            //      <string>&amp;File</string>
            //     </property>
            title: qsTr("&File")

            //     <addaction name="action_Open"/>
            //     <addaction name="separator"/>
            actions: [
                action_Open,
                separator,  // TODO
            ]

            //    </widget>
        }

        //    <addaction name="menu_File"/>
        actions: [
            menu_File,
        ]

        //   </widget>
    }

    //   <widget class="QStatusBar" name="statusbar"/>
    QStatusBar { id: statusbar }

    //   <action name="action_Open">
    QAction {
        id: action_Open

        //    <property name="text">
        //     <string>&amp;Open</string>
        //    </property>
        text: qsTr("&Open")

        //   </action>
    }

    //   <action name="actionFoo">
    QAction {
        id: actionFoo

        //    <property name="text">
        //     <string>Foo</string>
        //    </property>
        text: qsTr("Foo")

        //   </action>
    }

    //  </widget>
}

//  <resources/>
//  <connections/>
// TODO

// </ui>
