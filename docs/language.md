Supported QML/JS syntax
=======================

`qmluic` supports a tiny subset of QML and JavaScript/TypeScript syntax. The
language is syntactically compatible with QML/JS so you can use Qt Creator
for editing, but the semantics is diverged from JavaScript.

Notable differences are:

* No implicit type conversion (except for object upcasting.)
* Integer and floating point are distinct types.
* Integer division returns an integer.

Bindings
--------

Constant expression will be embed in the `.ui` XML file.

```qml
windowTitle: qsTr("Hello world!")
```

If the right-hand expression isn't constant, it will be automatically
re-evaluated when the underlying value changes.

```qml
QComboBox { id: edit }
QStackedWidget { id: stack; currentIndex: edit.currentIndex }
```

Under the hood, this will be translated to the following signal/slot
connection.

```c++
connect(edit, &QComboBox::currentIndexChanged, root,
        []() { stack->setCurrentIndex(edit->currentIndex()); });
```

Callbacks
---------

Signal callback can be attached by `on<Signal>`.

```qml
QDialogButtonBox {
    onAccepted: root.accept()
    onRejected: root.reject()
}
```

In order to take signal arguments, wrap the callback statements with
`function() { ... }`. Unlike QtQuick/QML, type annotation is mandatory.

```qml
onToggled: function(checked: bool) {
}
```

Attached properties
-------------------

TODO: [metatype_tweak.rs](../lib/src/metatype_tweak.rs)

Types
-----

In addition to `Q_OBJECT`/`Q_GADGET` classes and `Q_ENUM`s, the following
types are defined.

* `bool`
* `double` / `qreal`
* `int`
* `uint`
* `QBrush`
* `QColor`
* `QCursor`
* `QIcon`
* `QPalette`
* `QString`
* `QStringList`
* `QVariant`
* `void`

Not all types are usable in dynamic expression context. For example, there's
no `QColor` constructor yet.

`QObject`-derived objects are passed by pointer.

Operators
---------

* Unary arithmetic: `+`, `-`
* Unary bitwise: `~`
* Unary logical: `!`
* Binary arithmetic: `+`, `-`, `*`, `/`, `%`
* Binary bitwise: `&`, `^`, `|`
* Shift: `>>`, `<<`
* Binary logical: `&&`, `||`
* Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
* Type cast: `<value> as <type>`
  * numeric cast amongst `int` / `uint` / `double`
  * `enum` to integer
  * `bool` to integer
  * discarding result: `<value> as void`
  * TODO: `QVariant` cast will be added later
  * TODO: `qobject_cast` for downcasting will be added later
* Ternary: `... ? ... : ...`

Operands are statically type checked. `<string> + <int>` is invalid for
example.

Strict comparison operators, `===` and `!==`, are aliased to `==` and `!=`
respectively. There are no lax comparison operators.

Builtin functions and methods
-----------------------------

* `qsTr()`
* `QString::arg()`

Statements
----------

* `let` / `const` (type annotation or initial value is required)
* `if` / `else`
* `switch` / `case` / `default` / `break`
  (case expression is tested by `==` operator)
* `return`

No loop statement is available.
