import qmluic.QtWidgets

QMainWindow {
    geometry { x: 0; y: 0; width: 800; height: 600 }
    windowTitle: qsTr("MainWindow")
    actions: []

    QWidget { id: centralwidget }

    QMenuBar {
        id: menubar
        actions: [
            menu_File,
            menu_Edit,
        ]

        QMenu {
            id: menu_File
            title: qsTr("&File")
            actions: [
                action_Open,
                separator,
                action_Quit,
            ]
        }

        QMenu {
            id: menu_Edit
            title: qsTr("&Edit")
            actions: [
                action_Undo,
                action_Redo,
            ]
        }
    }

    QToolBar {
        actions: [
            action_Open,
            separator,
            action_Undo,
            action_Redo,
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

    // TODO: find better way to represent a menu separator
    QAction {
        id: separator
    }
}
