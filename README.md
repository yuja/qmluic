qmluic - Write QtWidgets UI in QML
==================================

*A .qml-to-.ui transpiler.*

Write UI in QML:
```qml
import qmluic.QtWidgets

QDialog {
    windowTitle: qsTr("Hello")
    QVBoxLayout {
        QLabel { text: qsTr("Hello world!") }
    }
}
```

Run live preview and polish the UI:
```
$ qmluic preview HelloDialog.qml
```

Transpile to `*.ui`/`ui_*.h`/`uisupport_*.h`:
```
$ qmluic generate-ui HelloDialog.qml
$ uic hellodialog.ui -o ui_hellodialog.h
```

See [examples/](examples/) directory for details.

Usage
-----

`qmluic generate-ui` command translates `.qml` file to `.ui` XML file and
`uisupport_*.h` C++ code. `.ui` can then be processed by Qt User Interface
Compiler `uic` command. See [Dynamic Binding](#dynamic-binding) for
`uisupport_*.h`.

By default, `qmluic generate-ui` loads type information from the
`QT_INSTALL_LIBS/metatypes` directory. Use `--qmake` or `--foreign-types`
option to load metatype.json files from the other directory or files.

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
$ qmluic preview HelloDialog.qml
```

The previewer might not work on Windows because of the stdio use. Patches are
welcome.

### Dynamic Binding

(To turn off this feature, set `--no-dynamic-binding` or `NO_DYNAMIC_BINDING`
option in CMake.)

`qmluic generate-ui` generates a `uisupport_*.h` file in addition to `*.ui`,
which sets up signal/slot connections for the dynamic binding expressions.

```qml
import qmluic.QtWidgets

QDialog {
    id: root
    QVBoxLayout {
        QCheckBox { id: checkBox; checked: true }
        QLabel { id: label; visible: checkBox.checked; text: qsTr("Checked") }
    }
}
```

In this example, `visible: checkBox.checked` is conceptually translated to the
following code:

```c++
void UiSupport::MainWindow::setup() {
    connect(checkBox, &QAbstractButton::toggled, root,
            [this]() { label->setVisible(checkBox->isChecked()); });
}
```

(The generated code would be more verbose since QML/JS expression is first
transformed to basic intermediate representation.)

Syntax of supported binding expressions is quite limited right now. See
the "Major TODOs" section for details.

Building
--------

Requirements:

- Rust
- Cargo

Optional requirements for previewer and type stubs:

- CMake
- Qt 5.15 or 6.2+

If you have all requirements installed, use Cargo and CMake to build/install
the binaries and data. There's a GNU Make wrapper to automate the build steps.

```
$ make release install
```

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
- [x] Support signal-slot connections (`on<Signal>` syntax)
- [ ] Improve support for dynamic property bindings / signal callbacks
  - [x] ternary operator
  - [ ] `.toString()`
  - [x] `QString::arg()`
  - [ ] integer/string as bool
  - [x] integer/floating point mess
  - [x] implicit/explicit `this` property
  - [x] method/slot call
  - [x] property setter
  - [ ] gadget types
  - [x] cycle detection
  - [x] `if` statement
  - [ ] `switch` statement
  - [ ] `return` statement
  - [ ] `let` binding
  - [x] multiple statements
  - [ ] signal parameters: `on<Signal>: function(...) {}`
- [ ] export helper functions from C++

Comparison to DeclarativeWidgets
--------------------------------

[DeclarativeWidgets](https://github.com/KDAB/DeclarativeWidgets) exports
QtWidgets classes to the QML world. You can write widget application in a way
you would do for QtQuick applications. You can fully leverage property
bindings, etc. The downside is the runtime dependency. And I guess it would
be a bit tedious to "control" a widget from C++ side.

`qmluic` is merely a .ui file generator. You can't write expressive property
bindings. You can (and need to) implement C++ counterpart for each QML UI.
