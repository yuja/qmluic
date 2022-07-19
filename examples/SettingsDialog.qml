import qmluic.QtWidgets

QDialog {
    id: root

    geometry { x: 0; y: 0; width: 400; height: 300 }
    windowTitle: qsTr("Settings")

    QVBoxLayout {
        QFormLayout {
            QLabel { text: qsTr("Hg executable:") }
            QHBoxLayout {
                QLineEdit { id: hgExecutableEdit }
                QToolButton {
                    id: hgExecutableBrowseButton
                    text: qsTr("...")
                }
            }

            QLabel { text: qsTr("Text font:") }
            QHBoxLayout {
                QFontComboBox {
                    id: textFontFamilyEdit
                    sizePolicy {
                        horizontalPolicy: QSizePolicy.Expanding
                        verticalPolicy: QSizePolicy.Fixed
                        horizontalStretch: 0
                        verticalStretch: 0
                    }
                }

                QSpinBox {
                    id: textFontSizeEdit
                    alignment: Qt.AlignRight | Qt.AlignTrailing | Qt.AlignVCenter
                    maximum: 999
                }

                QLabel { text: qsTr("pt") }
            }
        }

        QSpacerItem {
            orientation: Qt.Vertical
            sizeHint { width: 20; height: 40 }
        }

        QDialogButtonBox {
            id: buttonBox
            orientation: Qt.Horizontal
            standardButtons: QDialogButtonBox.Cancel | QDialogButtonBox.Ok
            // C++ signal slot connections:
            onAccepted: root.accept()
            onRejected: root.reject()
        }
    }
}
