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

            QLabel { text: qsTr("Alternate font:") }
            QHBoxLayout {
                QFontComboBox {
                    id: altFontFamilyEdit
                    sizePolicy {
                        horizontalPolicy: QSizePolicy.Expanding
                        verticalPolicy: QSizePolicy.Fixed
                        horizontalStretch: 0
                        verticalStretch: 0
                    }
                }

                QSpinBox {
                    id: altFontSizeEdit
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
                    QHBoxLayout {
                        QRadioButton {
                            id: textFontRadio
                            text: qsTr("Text font")
                            checked: true
                        }

                        QRadioButton {
                            text: qsTr("Alternate font")
                        }

                        QSpacerItem { orientation: Qt.Horizontal }
                    }

                    QLabel {
                        text: "The quick brown fox jumps over the lazy dog."
                        font: {
                            let fontEdit = textFontRadio.checked ? textFontFamilyEdit
                                                                 : altFontFamilyEdit;
                            let sizeEdit = [altFontSizeEdit, textFontSizeEdit][textFontRadio.checked as int];
                            let font = fontEdit.currentFont;
                            font.pointSize = Math.max(sizeEdit.value, 1);
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

                    // This is another way to bind font properties. Since QFont may have
                    // multiple families, it's probably better to overwrite font.pointSize
                    // instead of setting properties separately. See the example above.
                    QLabel {
                        text: "The quick brown fox jumps over the lazy dog."
                        font.family: (textFontRadio.checked ? textFontFamilyEdit
                                                            : altFontFamilyEdit).currentFont.family
                        font.pointSize: {
                            let value = (textFontRadio.checked ? textFontSizeEdit
                                                               : altFontSizeEdit).value;
                            Math.max(value, 1)
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
