qmluic
======

Write QtWidgets UI in QML. **(EXPERIMENTAL)**

Usage
-----

`qmluic generate-ui` command translates `.qml` file to `.ui` XML file, which
can then be processed by Qt User Interface Compiler `uic` command.

```
$ qmluic generate-ui SettingsDialog.qml
$ uic settingsdialog.ui -o ui_settingsdialog.h
```

`qmluic generate-ui` loads type information from the `QT_INSTALL_LIBS/metatypes`
directory by default. Specify `--foreign-types` option to load metatype.json
files from the other directory or files.

Example QML file
----------------

(see examples/*.qml)

```qml
// qmluic hosts all classes under this module. This also helps Qt Creator
// find type stubs for completion.
import qmluic.QtWidgets

// Top-level widget
QDialog {
    windowTitle: qsTr("Settings")

    QFormLayout {
        QLabel { text: qsTr("Path") }
        QHBoxLayout {
            QLineEdit { id: pathEdit }
            QToolButton {
                id: pathBrowseButton
                text: "..."
            }
        }
        // ...
    }
}
```
