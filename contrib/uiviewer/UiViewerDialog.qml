import qmluic.QtWidgets

QDialog {
    QVBoxLayout {
        QCheckBox {
            id: separateWindowCheck
            text: qsTr("Show in separate window")
        }

        QFrame {
            frameShape: QFrame.StyledPanel
            frameShadow: QFrame.Plain

            QVBoxLayout {
                id: contentLayout
                contentsMargins { left: 0; top: 0; right: 0; bottom: 0 }
            }
        }
    }
}
