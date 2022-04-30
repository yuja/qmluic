// <ui version="4.0">
//  <class>SettingsDialog</class>

import qmluic.QtWidgets

//  <widget class="QDialog" name="SettingsDialog">
QDialog {
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
    //    <string>Settings</string>
    //   </property>
    windowTitle: qsTr("Settings")

    //   <layout class="QVBoxLayout" name="verticalLayout">
    QVBoxLayout {
        id: verticalLayout

        //    <item>
        //     <layout class="QFormLayout" name="formLayout">
        QFormLayout {
            id: formLayout

            //      <item row="0" column="0">
            //       <widget class="QLabel" name="hgExecutableLabel">
            QLabel {
                id: hgExecutableLabel

                QLayoutItem.row: 0
                QLayoutItem.column: 0

                //        <property name="text">
                //         <string>Hg executable:</string>
                //        </property>
                text: qsTr("Hg executable:")

                //       </widget>
                //      </item>
            }

            //      <item row="0" column="1">
            //       <layout class="QHBoxLayout" name="hgExecutableLayout">
            QHBoxLayout {
                id: hgExecutableLayout

                QLayoutItem.row: 0
                QLayoutItem.column: 1

                //        <item>
                //         <widget class="QLineEdit" name="hgExecutableEdit"/>
                QLineEdit { id: hgExecutableEdit }
                //        </item>

                //        <item>
                //         <widget class="QToolButton" name="hgExecutableBrowseButton">
                QToolButton {
                    id: hgExecutableBrowseButton

                    //          <property name="text">
                    //           <string>...</string>
                    //          </property>
                    text: qsTr("...")

                    //         </widget>
                    //        </item>
                }

                //       </layout>
                //      </item>
            }

            //      <item row="1" column="0">
            //       <widget class="QLabel" name="textFontLabel">
            QLabel {
                id: textFontLabel

                QLayoutItem.row: 1
                QLayoutItem.column: 0

                //        <property name="text">
                //         <string>Text font:</string>
                //        </property>
                text: qsTr("Text font:")

                //       </widget>
                //      </item>
            }

            //      <item row="1" column="1">
            //       <layout class="QHBoxLayout" name="textFontLayout">
            QHBoxLayout {
                id: textFontLayout

                QLayoutItem.row: 1
                QLayoutItem.column: 1

                //        <item>
                //         <widget class="QFontComboBox" name="textFontFamilyEdit"/>
                QFontComboBox { id: textFontFamilyEdit }
                //        </item>

                //        <item>
                //         <widget class="QSpinBox" name="textFontSizeEdit">
                QSpinBox {
                    id: textFontSizeEdit

                    //          <property name="alignment">
                    //           <set>Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter</set>
                    //          </property>
                    alignment: Qt.AlignRight | Qt.AlignTrailing | Qt.AlignVCenter

                    //          <property name="maximum">
                    //           <number>999</number>
                    //          </property>
                    maximum: 999

                    //         </widget>
                    //        </item>
                }

                //        <item>
                //         <widget class="QLabel" name="textFontSizeLabel">
                QLabel {
                    id: textFontSizeLabel

                    //          <property name="text">
                    //           <string>pt</string>
                    //          </property>
                    text: qsTr("pt")

                    //         </widget>
                    //        </item>
                }

                //       </layout>
                //      </item>
            }
            //     </layout>
            //    </item>
        }

        //    <item>
        //     <spacer name="verticalSpacer">
        QSpacerItem {
            id: verticalSpacer

            //      <property name="orientation">
            //       <enum>Qt::Vertical</enum>
            //      </property>
            orientation: Qt.Vertical

            //      <property name="sizeHint" stdset="0">
            //       <size>
            //        <width>20</width>
            //        <height>40</height>
            //       </size>
            //      </property>
            sizeHint { width: 20; height: 40 }

            //     </spacer>
            //    </item>
        }

        //    <item>
        //     <widget class="QDialogButtonBox" name="buttonBox">
        QDialogButtonBox {
            id: buttonBox

            //      <property name="orientation">
            //       <enum>Qt::Horizontal</enum>
            //      </property>
            orientation: Qt.Horizontal

            //      <property name="standardButtons">
            //       <set>QDialogButtonBox::Cancel|QDialogButtonBox::Ok</set>
            //      </property>
            standardButtons: QDialogButtonBox.Cancel | QDialogButtonBox.Ok

            //     </widget>
            //    </item>
        }

        //   </layout>
    }
}
