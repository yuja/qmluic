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
        ]

        QMenu {
            id: menu_File
            title: qsTr("&File")
            actions: [
                action_Open,
                separator,
            ]
        }
    }

    QStatusBar { id: statusbar }

    QAction {
        id: action_Open
        text: qsTr("&Open")
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
