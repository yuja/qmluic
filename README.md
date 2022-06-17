qmluic - Write QtWidgets UI in QML
==================================

*A .qml-to-.ui transpiler. No dynamic binding support at the moment.*

Usage
-----

`qmluic generate-ui` command translates `.qml` file to `.ui` XML file, which
can then be processed by Qt User Interface Compiler `uic` command.

```
$ qmluic generate-ui SettingsDialog.qml
$ uic settingsdialog.ui -o ui_settingsdialog.h
```

By default, `qmluic generate-ui` loads type information from the
`QT_INSTALL_LIBS/metatypes` directory. Use `--foreign-types` option to load
metatype.json files from the other directory or files.

`qmluic` is tested with Qt 5.15 on Debian sid. It should run with Qt 6 in
principle, but there may be silly bugs.

### Code Completion

You can leverage the excellent Qt Creator's QML editor. You just need to add
[`contrib/imports`](contrib/imports) to the `QML_IMPORT_PATH` so the creator
can find our type stubs.

See the following examples:

* [contrib/uiviewer/CMakeLists.txt](contrib/uiviewer/CMakeLists.txt)
* [examples/examples.qmlproject](examples/examples.qmlproject)

### Live Preview

`qmluic preview` command starts filesystem watcher, and updates the preview
window to reflect the source QML file changes. It does not support multi-file
QML sources yet.

```
$ qmluic preview SettingsDialog.qml
```

The preview window is a separate Qt C++ application, which needs to be built
in addition to the standard `cargo build --workspace` command.
See [Makefile](Makefile) for the build instructions (or just run `make release`
on Linux.)

The previewer might not work on Windows because of the stdio use. Patches are
welcome.

Example QML file
----------------

(see [examples/*.qml](examples))

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

Debugging
---------

Debug logging can be enabled by `QMLUIC_LOG` environment variable.

```
$ QMLUIC_LOG=trace cargo run -- generate-ui SettingsDialog.qml
```

https://docs.rs/env_logger/latest/env_logger/

Major TODOs
-----------

- [ ] Support more gadget types: QPalette, ...
- [ ] Load type stubs from qmldir/plugins.qmltypes instead of metatypes.json
- [ ] Better type resolution, namespace support
- [ ] Build integration (maybe cmake?)
- [ ] Live preview triggered by lsp
- [ ] Live preview for multi-document file
- [ ] Support signal-slot connections (`on<Signal>` syntax)
- [ ] Support dynamic property bindings

Comparison to DeclarativeWidgets
--------------------------------

[DeclarativeWidgets](https://github.com/KDAB/DeclarativeWidgets) exports
QtWidgets classes to the QML world. You can write widget application in a way
you would do for QtQuick applications. You can fully leverage property
bindings, etc. The downside is the runtime dependency. And I guess it would
be a bit tedious to "control" a widget from C++ side.

`qmluic` is merely a .ui file generator. You can't write expressive property
bindings (though I'm thinking of adding a C++ codegen pass for trivial
expressions.) You can (and need to) implement C++ counterpart for each QML UI.
