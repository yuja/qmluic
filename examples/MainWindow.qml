import qmluic.QtWidgets

// This is a main window example which is somewhat functional. Selecting the source
// combo box will update the source/form views.
//
// Menu/toolbar actions are all dummy but for the quit action.
QMainWindow {
    id: root

    // Since the width/height properties are readonly, you need to set the geometry instead.
    // You can also set the size property, but apparently uic generates a slightly better
    // code for geometry. You can omit 'x: 0; y: 0;' part.
    geometry { x: 0; y: 0; width: 800; height: 600 }

    windowTitle: {
        let mainPage = fileNameEdit.currentIndex === 0;
        qsTr("Qmluic Example: %1").arg(
                mainPage ? qsTr("*** Select Source ***") : fileNameEdit.currentText)
    }

    // QAction and QMenu instances will be added automatically unless 'actions'
    // is explicitly set.
    actions: []

    // uic will pick a widget as the central widget if it is not QMenuBar, QToolBar,
    // QDockWidget, nor QStatusBar.
    QWidget {
        id: centralwidget

        QVBoxLayout {
            QFormLayout {
                QLabel { text: qsTr("&Source"); buddy: fileNameEdit }
                QComboBox {
                    id: fileNameEdit
                    model: [
                        "MainWindow.qml",
                        "BindingLoop.qml",
                        "HgEmailDialog.qml",
                        "ItemViews.qml",
                        "LayoutFlow.qml",
                        "SettingsDialog.qml",
                        "StaticItemModel.qml",
                        "VariousLayouts.qml",
                    ]
                    onTextHighlighted: function(s: QString) {
                        statusbar.showMessage(s, 1000);
                    }
                }
            }

            QSplitter {
                sizePolicy.horizontalPolicy: QSizePolicy.Expanding
                sizePolicy.verticalPolicy: QSizePolicy.Expanding

                QPlainTextEdit {
                    id: sourceEdit
                    font.family: "Monospace"
                    lineWrapMode: QPlainTextEdit.NoWrap
                    readOnly: true
                    textInteractionFlags: Qt.TextSelectableByMouse | Qt.TextSelectableByKeyboard
                }

                QStackedWidget {
                    id: formStack

                    // Dynamic binding is handled by the generated UiSupport code.
                    // Basically, this binding will be mapped to the following connection:
                    //
                    // connect(fileNameEdit, &QComboBox::currentIndexChanged,
                    //         []() { formStack->setCurrentIndex(fileNameEdit->currentIndex()); });
                    currentIndex: fileNameEdit.currentIndex

                    QWidget {}  // placeholder for "MainWindow.qml"
                    BindingLoop {}
                    HgEmailDialog {}
                    ItemViews {}
                    LayoutFlow {}
                    SettingsDialog {}
                    StaticItemModel {}
                    VariousLayouts {}
                }
            }
        }
    }

    QMenuBar {
        id: menubar

        QMenu {
            title: qsTr("&File")
            actions: [
                action_Open,
                whateverSeparator,
                action_Quit,
            ]
        }

        QMenu {
            title: qsTr("&Edit")
            actions: [
                action_Undo,
                action_Redo,
            ]
        }

        QMenu {
            title: qsTr("&Help")

            // Since 'actions' isn't set, QAction and QMenu instances will be added to
            // the menu automatically.

            QAction {
                id: action_About
                icon.normalOff: "app.png"
                text: qsTr("&About")
            }

            QAction { separator: true }

            QMenu {
                title: qsTr("&Developer tools")

                QAction {
                    text: qsTr("&Console")
                }
            }
        }
    }

    QToolBar {
        actions: [
            action_Open,
            whateverSeparator,
            action_Undo,
            action_Redo,
            whateverSeparator,
            action_About,
        ]
    }

    QStatusBar { id: statusbar }

    QAction {
        id: action_Open
        icon.name: "document-open"
        text: qsTr("&Open")
        // QUiLoader nor designer doesn't support StandardKey enum, but uic
        // can generate a valid code (because it is just an enum property.)
        shortcut: QKeySequence.Open
    }

    QAction {
        id: action_Quit
        text: qsTr("&Quit")
        shortcut: "Ctrl+X, Ctrl+C"
        onTriggered: root.close()
    }

    QAction {
        id: action_Undo
        icon.name: "edit-undo"
        text: qsTr("&Undo")
    }

    QAction {
        id: action_Redo
        icon.name: "edit-redo"
        text: qsTr("&Redo")
        enabled: false
    }

    QAction {
        id: actionFoo
        text: qsTr("Foo")
    }

    // Action representing a menu/toolbar separator.
    QAction {
        id: whateverSeparator
        separator: true
    }
}
