import qmluic.QtWidgets
import "common"

QDialog {
    QVBoxLayout {
        SettingsForm {
            id: settingsForm
        }

        MyDialogButtonBox {
            id: buttonBox
        }
    }
}
