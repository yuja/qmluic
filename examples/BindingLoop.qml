import qmluic.QtWidgets

// This should trigger assertion "binding loop detected". Under release build,
// it would SEGV because of stack overflow.
QWidget {
    QVBoxLayout {
        QPushButton {
            id: trig
            checkable: true
            font.pointSize: 20
            text: qsTr("Click to crash")
        }

        // Add 33 bindings to make sure that the guard bitset can support
        // more than sizeof(quint32) * 8 bindings.
        QGridLayout {
            columns: 8
            QCheckBox { id: check0; enabled: false; checked: check32.checked }
            QCheckBox { id: check1; enabled: false; checked: check0.checked }
            QCheckBox { id: check2; enabled: false; checked: check1.checked }
            QCheckBox { id: check3; enabled: false; checked: check2.checked }
            QCheckBox { id: check4; enabled: false; checked: check3.checked }
            QCheckBox { id: check5; enabled: false; checked: check4.checked }
            QCheckBox { id: check6; enabled: false; checked: check5.checked }
            QCheckBox { id: check7; enabled: false; checked: check6.checked }
            QCheckBox { id: check8; enabled: false; checked: check7.checked }
            QCheckBox { id: check9; enabled: false; checked: check8.checked }
            QCheckBox { id: check10; enabled: false; checked: check9.checked }
            QCheckBox { id: check11; enabled: false; checked: check10.checked }
            QCheckBox { id: check12; enabled: false; checked: check11.checked }
            QCheckBox { id: check13; enabled: false; checked: check12.checked }
            QCheckBox { id: check14; enabled: false; checked: check13.checked }
            QCheckBox { id: check15; enabled: false; checked: check14.checked }
            QCheckBox { id: check16; enabled: false; checked: check15.checked }
            QCheckBox { id: check17; enabled: false; checked: check16.checked }
            QCheckBox { id: check18; enabled: false; checked: check17.checked }
            QCheckBox { id: check19; enabled: false; checked: check18.checked }
            QCheckBox { id: check20; enabled: false; checked: check19.checked }
            QCheckBox { id: check21; enabled: false; checked: check20.checked }
            QCheckBox { id: check22; enabled: false; checked: check21.checked }
            QCheckBox { id: check23; enabled: false; checked: check22.checked }
            QCheckBox { id: check24; enabled: false; checked: check23.checked }
            QCheckBox { id: check25; enabled: false; checked: check24.checked }
            QCheckBox { id: check26; enabled: false; checked: check25.checked }
            QCheckBox { id: check27; enabled: false; checked: check26.checked }
            QCheckBox { id: check28; enabled: false; checked: check27.checked }
            QCheckBox { id: check29; enabled: false; checked: check28.checked }
            QCheckBox { id: check30; enabled: false; checked: check29.checked }
            QCheckBox { id: check31; enabled: false; checked: check30.checked }
            QCheckBox { id: check32; enabled: false; checked: check31.checked ^ trig.checked }
        }

        QSpacerItem { orientation: Qt.Vertical }
    }
}
