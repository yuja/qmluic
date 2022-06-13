import qmluic.QtWidgets

QMainWindow {
    geometry { x: 0; y: 0; width: 800; height: 600 }
    windowTitle: qsTr("MainWindow")

    // QAction and QMenu instances will be added automatically unless 'actions'
    // is explicitly set.
    actions: []

    QWidget { id: centralwidget }

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

            QAction {
                id: action_About
                text: qsTr("&About")
            }

            QActionSeparator {}

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
        text: qsTr("&Open")
        // QUiLoader nor designer doesn't support StandardKey enum, but uic
        // can generate a valid code (because it is just an enum property.)
        shortcut: QKeySequence.Open
    }

    QAction {
        id: action_Quit
        text: qsTr("&Quit")
        shortcut: "Ctrl+X, Ctrl+C"
    }

    QAction {
        id: action_Undo
        text: qsTr("&Undo")
    }

    QAction {
        id: action_Redo
        text: qsTr("&Redo")
        enabled: false
    }

    QAction {
        id: actionFoo
        text: qsTr("Foo")
    }

    // Pseudo action representing a menu/toolbar separator.
    QActionSeparator {
        id: whateverSeparator
    }
}
