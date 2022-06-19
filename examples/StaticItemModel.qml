import qmluic.QtWidgets

// As of now, only string list can be assigned as a static item model. We'll probably
// want to support icons as well, but the syntax isn't determined yet.
//
// It's unlikely for qmluic to support more complex static models like tables and nested
// tree because such kind of models will be constructed and managed by C++ business logic.
QWidget {
    QFormLayout {
        QLabel { text: "QComboBox" }
        QComboBox {
            currentIndex: 1
            model: [
                qsTr("Foo"),
                qsTr("Bar"),
                qsTr("Baz"),
            ]
        }

        QLabel { text: "QListWidget" }
        QListWidget {
            model: [
                qsTr("Foo"),
                qsTr("Bar"),
                qsTr("Baz"),
            ]
        }
    }
}
