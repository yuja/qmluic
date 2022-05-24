import qmluic.QtWidgets

QWidget {
    QFormLayout {
        QLabel { text: "LeftToRight" }
        QGridLayout {
            flow: QGridLayout.LeftToRight  // default
            columns: 3

            QLabel { text: "row: 0, column: 0" }
            QLabel { text: "row: 0, column: 1" }
            QLabel { text: "row: 0, column: 2" }

            QLabel { text: "row: 1, column: 0" }
            QLabel { text: "row: 1, column: 1" }

            QLabel { QLayout.row: 2; text: "row: 2, column: 0" }
        }

        QLabel { text: "TopToBottom" }
        QGridLayout {
            flow: QGridLayout.TopToBottom
            rows: 2

            QLabel { text: "row: 0, column: 0" }
            QLabel { text: "row: 1, column: 0" }

            QLabel { text: "row: 0, column: 1" }
            QLabel { text: "row: 1, column: 1" }
        }
    }
}
