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
                    value: 9
                    maximum: 999
                }

                QLabel { text: qsTr("pt") }
            }

            QLabel { text: qsTr("Text alignment:") }
            QComboBox {
                id: textAlignmentEdit
                model: [
                    "Left",
                    "Right",
                    "Center",
                    "Justify",
                ]
            }

            QFrame {
                QLayout.column: 1
                frameShape: QFrame.StyledPanel
                frameShadow: QFrame.Plain
                QVBoxLayout {
                    QLabel {
                        text: "The quick brown fox jumps over the lazy dog."
                        font: {
                            let font = textFontFamilyEdit.currentFont;
                            font.pointSize = textFontSizeEdit.value;
                            return font;
                        }
                        alignment: {
                            switch (textAlignmentEdit.currentIndex) {
                            case 0:
                                return Qt.AlignLeft;
                            case 1:
                                return Qt.AlignRight;
                            case 2:
                                return Qt.AlignHCenter;
                            case 3:
                                return Qt.AlignJustify;
                            default:
                                return Qt.AlignLeft;
                            }
                        }
                        wordWrap: true
                    }
                }
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
