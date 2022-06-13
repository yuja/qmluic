import qmluic.QtWidgets

QMainWindow {
    geometry { x: 0; y: 0; width: 800; height: 600 }
    windowTitle: qsTr("MainWindow")

    QWidget { id: centralwidget }

    QMenuBar {
        id: menubar
        geometry { x: 0; y: 0; width: 800; height: 20 }
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
    }

    QAction {
        id: action_Quit
        text: qsTr("&Quit")
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
