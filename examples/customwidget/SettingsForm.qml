import qmluic.QtWidgets

QWidget {
    QFormLayout {
        QLabel { text: qsTr("Path") }
        QHBoxLayout {
            QLineEdit { id: pathEdit }
            QToolButton { id: pathBrowseButton; text: "..." }
        }
    }
}
