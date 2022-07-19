import qmluic.QtWidgets

QDialog {
    id: root

    geometry { x: 0; y: 0; width: 400; height: 300 }
    windowTitle: qsTr("Settings")

    QVBoxLayout {
        id: verticalLayout

        QFormLayout {
            id: formLayout

            QLabel {
                id: hgExecutableLabel
                QLayout.row: 0
                QLayout.column: 0
                text: qsTr("Hg executable:")
            }

            QHBoxLayout {
                id: hgExecutableLayout
                QLayout.row: 0
                QLayout.column: 1
                QLineEdit { id: hgExecutableEdit }
                QToolButton {
                    id: hgExecutableBrowseButton
                    text: qsTr("...")
                }
            }

            QLabel {
                id: textFontLabel
                QLayout.row: 1
                QLayout.column: 0
                text: qsTr("Text font:")
            }

            QHBoxLayout {
                id: textFontLayout
                QLayout.row: 1
                QLayout.column: 1

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

                QLabel {
                    id: textFontSizeLabel
                    text: qsTr("pt")
                }
            }
        }

        QSpacerItem {
            id: verticalSpacer
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
