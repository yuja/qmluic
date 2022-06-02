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

Tested with Qt 5.15 on Debian sid. `qmluic` should run with Qt 6 in principle,
but there may be silly bugs.

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

Only constant expressions are allowed in QML. You can concatenate string
literals, but not translated strings, for example.

```qml
QWidget {
    // this is okay
    QLabel { text: qsTr("The quick fox jumps over " + "the lazy dog.") }

    // this is not
    QLabel { text: qsTr("The quick fox jumps over ") + qsTr("the lazy dog.") }
}
```

Dynamic property bindings are unsupported at all.

```qml
QWidget {
    QCheckBox {
        id: check
        text: qsTr("Check")
    }

    QFrame {
        enabled: check.checked // not work
    }
}
```

Debugging
---------

Debug logging can be enabled by `QMLUIC_LOG` environment variable.

```
$ QMLUIC_LOG=trace cargo run -- generate-ui SettingsDialog.qml
```

https://docs.rs/env_logger/latest/env_logger/

Major TODOs
-----------

- [ ] Support more gadget types: QColor, QPalette, ...
- [ ] Load type stubs from qmldir/plugins.qmltypes instead of metatypes.json
- [ ] Better type resolution, namespace support
- [ ] Build integration (maybe cmake?)
- [ ] Support signal-slot connections (`on<Signal>` syntax)
- [ ] Support dynamic property bindings
