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

    // TODO:
    // <customwidgets>
    //  <customwidget>
    //   <class>SettingsForm</class>
    //   <extends>QWidget</extends>
    //   <header>SettingsForm.h</header>
    //  </customwidget>
    //  <customwidget>
    //   <class>MyDialogButtonBox</class>
    //   <extends>QDialogButtonBox</extends>
    //   <header>common/MyDialogButtonBox.h</header>
    //  </customwidget>
    // </customwidgets>
}
