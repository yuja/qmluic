// <ui version="4.0">
//  <class>VariousLayouts</class>

import qmluic.QtWidgets

//  <widget class="QWidget" name="VariousLayouts">
QWidget {
    //   <property name="geometry">
    //    <rect>
    //     <x>0</x>
    //     <y>0</y>
    //     <width>400</width>
    //     <height>300</height>
    //    </rect>
    //   </property>
    geometry { x: 0; y: 0; width: 400; height: 300 }

    //   <property name="windowTitle">
    //    <string>Various Layouts</string>
    //   </property>
    windowTitle: qsTr("Various Layouts")

    //   <layout class="QFormLayout" name="formLayout">
    QFormLayout {
        id: formLayout

        //    <item row="0" column="0">
        //     <widget class="QLabel" name="verticalLabel">
        QLabel {
            id: verticalLabel

            QLayout.row: 0
            QLayout.column: 0

            //      <property name="text">
            //       <string>Vertical</string>
            //      </property>
            text: qsTr("Vertical")

            //     </widget>
            //    </item>
        }

        //    <item row="0" column="1">
        //     <layout class="QVBoxLayout" name="verticalLayout">
        QVBoxLayout {
            id: verticalLayout

            QLayout.row: 0
            QLayout.column: 1

            //      <property name="spacing">
            //       <number>4</number>
            //      </property>
            spacing: 4

            //      <property name="leftMargin">
            //       <number>1</number>
            //      </property>
            //      <property name="topMargin">
            //       <number>2</number>
            //      </property>
            //      <property name="rightMargin">
            //       <number>3</number>
            //      </property>
            //      <property name="bottomMargin">
            //       <number>4</number>
            //      </property>
            // TODO: contentsMargins { left: 1; top: 2; right: 3; bottom: 4 }

            //      <item>
            //       <widget class="QRadioButton" name="radioButton">
            //        <property name="text">
            //         <string>RadioButton</string>
            //        </property>
            //       </widget>
            //      </item>
            QRadioButton { id: radioButton; text: qsTr("RadioButton") }

            //      <item>
            //       <widget class="QRadioButton" name="radioButton_2">
            //        <property name="text">
            //         <string>RadioButton</string>
            //        </property>
            //       </widget>
            //      </item>
            QRadioButton { id: radioButton_2; text: qsTr("RadioButton") }

            //      <item>
            //       <widget class="QCheckBox" name="checkBox">
            //        <property name="text">
            //         <string>CheckBox</string>
            //        </property>
            //       </widget>
            //      </item>
            QCheckBox { id: checkBox; text: qsTr("CheckBox") }

            //     </layout>
            //    </item>
        }

        //    <item row="1" column="0">
        //     <widget class="QLabel" name="horizontalLabel">
        QLabel {
            id: horizontalLabel

            QLayout.row: 1
            QLayout.column: 0

            //      <property name="text">
            //       <string>Horizontal</string>
            //      </property>
            text: qsTr("Horizontal")

            //     </widget>
            //    </item>
        }

        //    <item row="1" column="1">
        //     <layout class="QHBoxLayout" name="horizontalLayout" stretch="2,3">
        QHBoxLayout {
            id: horizontalLayout

            QLayout.row: 1
            QLayout.column: 1

            //      <item>
            //       <widget class="QPlainTextEdit" name="plainTextEdit"/>
            //      </item>
            QPlainTextEdit {
                id: plainTextEdit
                QLayout.columnStretch: 2
            }

            //      <item>
            //       <widget class="QPlainTextEdit" name="plainTextEdit_2"/>
            //      </item>
            QPlainTextEdit {
                id: plainTextEdit_2
                QLayout.columnStretch: 3
            }

            //     </layout>
            //    </item>
        }

        //    <item row="2" column="0">
        //     <widget class="QLabel" name="gridLabel">
        QLabel {
            id: gridLabel

            QLayout.row: 2
            QLayout.column: 0

            //      <property name="text">
            //       <string>Grid</string>
            //      </property>
            text: qsTr("Grid")

            //     </widget>
            //    </item>
        }

        //    <item row="2" column="1">
        //     <layout class="QGridLayout" name="gridLayout" rowstretch="1,2" columnstretch="3,4,5" rowminimumheight="20,20" columnminimumwidth="50,50,50">
        QGridLayout {
            id: gridLayout

            QLayout.row: 2
            QLayout.column: 1

            //      <property name="sizeConstraint">
            //       <enum>QLayout::SetFixedSize</enum>
            //      </property>
            sizeConstraint: QLayout.SetFixedSize

            //      <property name="leftMargin">
            //       <number>2</number>
            //      </property>
            //      <property name="topMargin">
            //       <number>2</number>
            //      </property>
            //      <property name="rightMargin">
            //       <number>2</number>
            //      </property>
            //      <property name="bottomMargin">
            //       <number>2</number>
            //      </property>
            // TODO: contentsMargins { left: 2; top: 2; right: 2; bottom: 2 }

            //      <property name="horizontalSpacing">
            //       <number>10</number>
            //      </property>
            // TODO: horizontalSpacing: 10

            //      <property name="verticalSpacing">
            //       <number>4</number>
            //      </property>
            // TODO: verticalSpacing: 4

            //      <item row="0" column="0">
            //       <widget class="QPushButton" name="pushButton">
            //        <property name="text">
            //         <string>PushButton</string>
            //        </property>
            //       </widget>
            //      </item>
            QPushButton {
                id: pushButton
                QLayout.row: 0
                QLayout.column: 0
                QLayout.rowStretch: 1
                QLayout.columnStretch: 3
                QLayout.rowMinimumHeight: 20
                QLayout.columnMinimumWidth: 50
                text: qsTr("PushButton")
            }

            //      <item row="1" column="0">
            //       <widget class="QPushButton" name="pushButton_2">
            //        <property name="text">
            //         <string>PushButton</string>
            //        </property>
            //       </widget>
            //      </item>
            QPushButton {
                id: pushButton_2
                QLayout.row: 1
                QLayout.column: 0
                QLayout.rowStretch: 2
                QLayout.columnStretch: 3
                QLayout.rowMinimumHeight: 20
                QLayout.columnMinimumWidth: 50
                text: qsTr("PushButton")
            }

            //      <item row="0" column="1" rowspan="2" alignment="Qt::AlignTop">
            //       <widget class="QPushButton" name="pushButton_3">
            //        <property name="text">
            //         <string>RowSpan
            // Button</string>
            //        </property>
            //       </widget>
            //      </item>
            QPushButton {
                id: pushButton_3
                QLayout.row: 0
                QLayout.column: 1
                QLayout.rowSpan: 2
                QLayout.alignment: Qt.AlignTop
                QLayout.rowStretch: 1
                QLayout.columnStretch: 4
                QLayout.columnMinimumWidth: 50
                text: qsTr("RowSpan\nButton")
            }

            //      <item row="0" column="2">
            //       <widget class="QPushButton" name="pushButton_4">
            //        <property name="text">
            //         <string>PushButton</string>
            //        </property>
            //       </widget>
            //      </item>
            QPushButton {
                id: pushButton_4
                QLayout.row: 0
                QLayout.column: 2
                QLayout.rowStretch: 1
                QLayout.columnStretch: 5
                QLayout.rowMinimumHeight: 20
                QLayout.columnMinimumWidth: 50
                text: qsTr("PushButton")
            }

            //     </layout>
            //    </item>
        }

        //   </layout>
    }

    //  </widget>
}

//  <resources/>
//  <connections/>
// </ui>
