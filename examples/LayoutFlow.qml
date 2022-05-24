import qmluic.QtWidgets

QWidget {
    QFormLayout {
        QLabel { text: "LeftToRight" }

        QGridLayout {
            QLabel { text: "row: 0, column: 0" }
            QLabel { text: "row: 0, column: 1" }
            QLabel { text: "row: 0, column: 2" }

            QLabel { QLayout.row: 1; text: "row: 1, column: 0" }
            QLabel { text: "row: 1, column: 1" }
        }
    }
}
