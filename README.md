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
`QT_INSTALL_LIBS/metatypes` directory. Use `--qmake` or `--foreign-types`
option to load metatype.json files from the other directory or files.

`qmluic` is tested with Qt 5.15 and 6.2 on Debian sid.

### CMake

There's a basic helper to integrate `qmluic generate-ui` in the build step.
Please make sure to not enable `CMAKE_AUTOUIC`, which conflicts with the .ui
generation step.

```cmake
find_package(Qt6 REQUIRED COMPONENTS Widgets)
find_package(Qmluic REQUIRED)

# DO NOT ENABLE: set(CMAKE_AUTOUIC ON)

# Help Qt Creator find qmluic type stub
set(QML_IMPORT_PATH ${QMLUIC_QML_IMPORT_PATH} CACHE STRING "" FORCE)

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

If you want to optionally enable the qmluic integration, copy
[cmake/QmluicShim.cmake](cmake/QmluicShim.cmake) to your project tree and
load it if qmluic not found:

```cmake
find_package(Qt6 REQUIRED COMPONENTS Widgets)
find_package(Qmluic QUIET)
if(Qmluic_FOUND)
  set(QML_IMPORT_PATH ${QMLUIC_QML_IMPORT_PATH} CACHE STRING "" FORCE)
else()
  include("${CMAKE_SOURCE_DIR}/cmake/QmluicShim.cmake")
endif()

qmluic_target_qml_sources(myapp
  MyDialog.qml
  ...
  # Put the generated .ui in the source directory so they will be committed.
  OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
```

### Code Completion

You can leverage the excellent Qt Creator's QML editor. You just need to add
`share/qmluic/imports` to the `QML_IMPORT_PATH` so the creator can find our
type stubs.

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

Building
--------

Requirements:

- Rust
- Cargo

Optional requirements for previewer and type stubs:

- CMake
- Qt 5.15 or 6.2+

If you have all requirements installed, use CMake (or GNU Make) to build
and install everything.

If you just need to build the `qmluic` frontend, simply run
`cargo build --release --workspace`.

See [Makefile](Makefile), [CMakeLists.txt](CMakeLists.txt), and
[build.yml](.github/workflows/build.yml) for more details.

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
