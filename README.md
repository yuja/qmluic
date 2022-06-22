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

`qmluic` is tested with Qt 5.15 and 6.2 on Debian sid.

### CMake

There's a basic helper to integrate `qmluic generate-ui` in the build step.
Please make sure to not enable `CMAKE_AUTOUIC`, which conflicts with the .ui
generation step.

```cmake
find_package(Qt6 REQUIRED COMPONENTS Widgets)
find_package(Qmluic REQUIRED)

# DO NOT ENABLE: set(CMAKE_AUTOUIC ON)

add_executable(myapp
  main.cpp
  ...
)

qmluic_target_qml_sources(myapp
  MyDialog.qml
  ...
)
```

See [examples/CMakeLists.txt](examples/CMakeLists.txt) for details.

#### Optional CMake

If you want to optionally enable the qmluic integration, try this:

```cmake
find_package(Qt6 REQUIRED COMPONENTS Widgets)
find_package(Qmluic QUIET)

if(Qmluic_FOUND)
  qmluic_target_qml_sources(myapp
    MyDialog.qml
    ...
    # Put the generated .ui in the source directory so they will be committed.
    OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
else()
  set(CMAKE_AUTOUIC ON)
  target_sources(myapp PRIVATE
    MyDialog.qml
    mydialog.ui
    ...
  )
endif()
```

### Code Completion

You can leverage the excellent Qt Creator's QML editor. You just need to add
[`contrib/imports`](contrib/imports) to the `QML_IMPORT_PATH` so the creator
can find our type stubs.

See the following examples:

* [examples/CMakeLists.txt](examples/CMakeLists.txt)
* [examples/examples.qmlproject](examples/examples.qmlproject)

### Live Preview

`qmluic preview` command starts filesystem watcher, and updates the preview
window to reflect the source QML file changes. It does not support multi-file
QML sources yet.

```
$ qmluic preview SettingsDialog.qml
```

The preview window is a separate Qt C++ application, which needs to be built
in addition to the standard `cargo build --workspace` command. Use the
top-level [CMakeLists.txt](CMakeLists.txt) to build and install the binaries
(or run `make local` on Linux for in-place usage.)

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

- [ ] Load type stubs from qmldir/plugins.qmltypes instead of metatypes.json
- [ ] Better type resolution, namespace support
- [ ] Live preview triggered by lsp
- [ ] Live preview for multi-document file
- [ ] Better support for static `QComboBox`/`QListWidget` items
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
