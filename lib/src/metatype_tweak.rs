//! Modifications on Qt metatypes data.

use crate::metatype::{Class, ClassInfo, Enum, Method, Property, SuperClassSpecifier};

/// Applies all modifications on the given `classes` data.
pub fn apply_all(classes: &mut Vec<Class>) {
    fix_classes(classes);
    classes.extend(internal_core_classes());
    classes.extend(internal_gui_classes());
    classes.extend(internal_widgets_classes());
}

/// Updates existing class meta data.
pub fn fix_classes(classes: &mut [Class]) {
    for cls in classes.iter_mut() {
        match cls.qualified_class_name.as_ref() {
            "QAbstractItemView" => fix_abstract_item_view(cls),
            "QAction" => fix_action(cls),
            "QComboBox" => fix_combo_box(cls),
            "QFont" => fix_font(cls),
            "QGridLayout" => fix_grid_layout(cls),
            "QLabel" => fix_label(cls),
            "QLayout" => fix_layout(cls),
            "QMenu" => fix_menu(cls),
            "QPushButton" => fix_push_button(cls),
            "QSizePolicy" => fix_size_policy(cls),
            "QTabWidget" => fix_tab_widget(cls),
            "QTableView" => fix_table_view(cls),
            "QTreeView" => fix_tree_view(cls),
            "QWidget" => fix_widget(cls),
            _ => {}
        }
    }
}

fn fix_abstract_item_view(cls: &mut Class) {
    cls.properties.extend([
        // handled by uigen
        Property::new("model", "QAbstractItemModel*"),
    ])
}

fn fix_action(cls: &mut Class) {
    cls.properties.push(Property {
        name: "separator".to_owned(),
        r#type: "bool".to_owned(),
        read: Some("isSeparator".to_owned()),
        write: Some("setSeparator".to_owned()),
        ..Default::default()
    });
}

fn fix_combo_box(cls: &mut Class) {
    cls.properties.extend([
        // handled by uigen
        Property::new("model", "QAbstractItemModel*"),
    ])
}

fn fix_font(cls: &mut Class) {
    cls.properties.extend([
        Property {
            name: "family".to_owned(),
            r#type: "QString".to_owned(),
            read: Some("family".to_owned()),
            write: Some("setFamily".to_owned()),
            ..Default::default()
        },
        Property {
            name: "pointSize".to_owned(),
            r#type: "int".to_owned(),
            read: Some("pointSize".to_owned()),
            write: Some("setPointSize".to_owned()),
            ..Default::default()
        },
        Property {
            name: "weight".to_owned(),
            r#type: "int".to_owned(), // TODO: enum QFont::Weight on Qt 6
            read: Some("weight".to_owned()),
            write: Some("setWeight".to_owned()),
            ..Default::default()
        },
        Property {
            name: "italic".to_owned(),
            r#type: "bool".to_owned(),
            read: Some("italic".to_owned()),
            write: Some("setItalic".to_owned()),
            ..Default::default()
        },
        Property {
            name: "bold".to_owned(),
            r#type: "bool".to_owned(),
            read: Some("bold".to_owned()),
            write: Some("setBold".to_owned()),
            ..Default::default()
        },
        Property {
            name: "underline".to_owned(),
            r#type: "bool".to_owned(),
            read: Some("underline".to_owned()),
            write: Some("setUnderline".to_owned()),
            ..Default::default()
        },
        Property {
            name: "strikeout".to_owned(), // follows QML name
            r#type: "bool".to_owned(),
            read: Some("strikeOut".to_owned()),
            write: Some("setStrikeOut".to_owned()),
            ..Default::default()
        },
        Property {
            name: "styleStrategy".to_owned(),
            r#type: "QFont::StyleStrategy".to_owned(),
            read: Some("styleStrategy".to_owned()),
            write: Some("setStyleStrategy".to_owned()),
            ..Default::default()
        },
        Property {
            name: "kerning".to_owned(),
            r#type: "bool".to_owned(),
            read: Some("kerning".to_owned()),
            write: Some("setKerning".to_owned()),
            ..Default::default()
        },
    ]);
}

fn fix_grid_layout(cls: &mut Class) {
    cls.enums.extend([
        Enum::with_values("Flow", ["LeftToRight", "TopToBottom"]), // handled by uigen
    ]);
    cls.properties.extend([
        // declared as QDOC_PROPERTY()
        Property {
            name: "horizontalSpacing".to_owned(),
            r#type: "int".to_owned(),
            read: Some("horizontalSpacing".to_owned()),
            write: Some("setHorizontalSpacing".to_owned()),
            ..Default::default()
        },
        Property {
            name: "verticalSpacing".to_owned(),
            r#type: "int".to_owned(),
            read: Some("verticalSpacing".to_owned()),
            write: Some("setVerticalSpacing".to_owned()),
            ..Default::default()
        },
        // handled by uigen
        Property::new("columns", "int"),
        Property::new("rows", "int"),
        Property::new("flow", "QGridLayout::Flow"),
    ]);
}

fn fix_label(cls: &mut Class) {
    cls.properties.push(Property {
        name: "buddy".to_owned(),
        r#type: "QWidget*".to_owned(),
        read: Some("buddy".to_owned()),
        write: Some("setBuddy".to_owned()),
        ..Default::default()
    });
}

fn fix_layout(cls: &mut Class) {
    cls.class_infos
        .push(ClassInfo::new("QML.Attached", "QLayoutAttached"));
    if !cls.properties.iter().any(|p| p.name == "contentsMargins") {
        // Qt 5
        cls.properties.push(Property {
            name: "contentsMargins".to_owned(),
            r#type: "QMargins".to_owned(),
            read: Some("contentsMargins".to_owned()),
            write: Some("setContentsMargins".to_owned()),
            ..Default::default()
        });
    }
}

fn fix_menu(cls: &mut Class) {
    // for static evaluation pass to obtain QAction from menu object id
    cls.methods.push(Method {
        name: "menuAction".to_owned(),
        return_type: "QAction*".to_owned(),
        ..Default::default()
    });
}

fn fix_push_button(cls: &mut Class) {
    if let Some(p) = cls.properties.iter_mut().find(|p| p.name == "default") {
        p.name += "_"; // reserved world
    }
}

fn fix_size_policy(cls: &mut Class) {
    cls.properties.extend([
        Property {
            name: "horizontalPolicy".to_owned(),
            r#type: "QSizePolicy::Policy".to_owned(),
            read: Some("horizontalPolicy".to_owned()),
            write: Some("setHorizontalPolicy".to_owned()),
            ..Default::default()
        },
        Property {
            name: "horizontalStretch".to_owned(),
            r#type: "int".to_owned(),
            read: Some("horizontalStretch".to_owned()),
            write: Some("setHorizontalStretch".to_owned()),
            ..Default::default()
        },
        Property {
            name: "verticalPolicy".to_owned(),
            r#type: "QSizePolicy::Policy".to_owned(),
            read: Some("verticalPolicy".to_owned()),
            write: Some("setVerticalPolicy".to_owned()),
            ..Default::default()
        },
        Property {
            name: "verticalStretch".to_owned(),
            r#type: "int".to_owned(),
            read: Some("verticalStretch".to_owned()),
            write: Some("setVerticalStretch".to_owned()),
            ..Default::default()
        },
    ]);
}

fn fix_tab_widget(cls: &mut Class) {
    cls.class_infos
        .push(ClassInfo::new("QML.Attached", "QTabWidgetAttached"));
}

fn fix_table_view(cls: &mut Class) {
    cls.properties.extend([
        Property::new("horizontalHeader", "QHeaderView*"), // handled by uigen
        Property::new("verticalHeader", "QHeaderView*"),   // handled by uigen
    ]);
}

fn fix_tree_view(cls: &mut Class) {
    cls.properties.extend([
        Property::new("header", "QHeaderView*"), // handled by uigen
    ]);
}

fn fix_widget(cls: &mut Class) {
    cls.properties.extend([
        Property::new("actions", "QList<QAction*>"), // handled by uigen
    ]);
}

/// Creates meta data for QtCore classes which are internally required, but not defined
/// in the Qt metatypes.json.
pub fn internal_core_classes() -> impl IntoIterator<Item = Class> {
    [
        Class {
            class_name: "QMargins".to_owned(),
            qualified_class_name: "QMargins".to_owned(),
            properties: vec![
                Property::new_final("left", "int"),
                Property::new_final("top", "int"),
                Property::new_final("right", "int"),
                Property::new_final("bottom", "int"),
            ],
            ..Default::default()
        },
        Class {
            class_name: "QRect".to_owned(),
            qualified_class_name: "QRect".to_owned(),
            properties: vec![
                Property::new_final("x", "int"),
                Property::new_final("y", "int"),
                Property::new_final("width", "int"),
                Property::new_final("height", "int"),
            ],
            ..Default::default()
        },
        Class {
            class_name: "QSize".to_owned(),
            qualified_class_name: "QSize".to_owned(),
            properties: vec![
                Property::new_final("width", "int"),
                Property::new_final("height", "int"),
            ],
            ..Default::default()
        },
    ]
}

/// Creates meta data for QtGui classes which are internally required, but not defined
/// in the Qt metatypes.json.
pub fn internal_gui_classes() -> impl IntoIterator<Item = Class> {
    [
        Class {
            class_name: "QBrush".to_owned(),
            qualified_class_name: "QBrush".to_owned(),
            class_infos: vec![ClassInfo::new("QML.Element", "auto")],
            gadget: true, // mark as "uncreatable" object in .qmltypes
            properties: vec![
                Property::new("color", "QColor"),
                Property::new("style", "Qt::BrushStyle"),
            ],
            ..Default::default()
        },
        Class {
            class_name: "QColor".to_owned(),
            qualified_class_name: "QColor".to_owned(),
            class_infos: vec![ClassInfo::new("QML.Element", "auto")],
            gadget: true, // mark as "uncreatable" object in .qmltypes
            properties: vec![
                // TODO: r, g, b: qreal vs int
            ],
            ..Default::default()
        },
        Class {
            class_name: "QCursor".to_owned(),
            qualified_class_name: "QCursor".to_owned(),
            ..Default::default()
        },
        Class {
            class_name: "QIcon".to_owned(),
            qualified_class_name: "QIcon".to_owned(),
            class_infos: vec![ClassInfo::new("QML.Element", "auto")],
            gadget: true, // mark as "uncreatable" object in .qmltypes
            properties: vec![
                // follows QtQuick.Controls naming:
                Property::new("name", "QString"),
                // follows uic naming:
                Property::new("normalOff", "QPixmap"),
                Property::new("normalOn", "QPixmap"),
                Property::new("disabledOff", "QPixmap"),
                Property::new("disabledOn", "QPixmap"),
                Property::new("activeOff", "QPixmap"),
                Property::new("activeOn", "QPixmap"),
                Property::new("selectedOff", "QPixmap"),
                Property::new("selectedOn", "QPixmap"),
            ],
            ..Default::default()
        },
        Class::new("QPaintDevice"), // super class of QWidget
        Class {
            class_name: "QPalette".to_owned(),
            qualified_class_name: "QPalette".to_owned(),
            super_classes: vec![SuperClassSpecifier::public("QPaletteColorGroup")],
            class_infos: vec![ClassInfo::new("QML.Element", "auto")],
            gadget: true, // mark as "uncreatable" object in .qmltypes
            properties: vec![
                // modeled after QQuickPalette
                Property::new("active", "QPaletteColorGroup"),
                Property::new("disabled", "QPaletteColorGroup"),
                Property::new("inactive", "QPaletteColorGroup"),
            ],
            ..Default::default()
        },
        Class {
            class_name: "QPaletteColorGroup".to_owned(),
            qualified_class_name: "QPaletteColorGroup".to_owned(),
            class_infos: vec![ClassInfo::new("QML.Element", "auto")],
            gadget: true, // mark as "uncreatable" object in .qmltypes
            properties: vec![
                // modeled after QQuickColorGroup
                Property::new("active", "QBrush"),
                Property::new("alternateBase", "QBrush"),
                Property::new("base", "QBrush"),
                Property::new("brightText", "QBrush"),
                Property::new("button", "QBrush"),
                Property::new("buttonText", "QBrush"),
                Property::new("dark", "QBrush"),
                Property::new("highlight", "QBrush"),
                Property::new("highlightedText", "QBrush"),
                Property::new("light", "QBrush"),
                Property::new("link", "QBrush"),
                Property::new("linkVisited", "QBrush"),
                Property::new("mid", "QBrush"),
                Property::new("midlight", "QBrush"),
                Property::new("placeholderText", "QBrush"),
                Property::new("shadow", "QBrush"),
                Property::new("text", "QBrush"),
                Property::new("toolTipBase", "QBrush"),
                Property::new("toolTipText", "QBrush"),
                Property::new("window", "QBrush"),
                Property::new("windowText", "QBrush"),
            ],
            ..Default::default()
        },
        Class {
            class_name: "QPixmap".to_owned(),
            qualified_class_name: "QPixmap".to_owned(),
            ..Default::default()
        },
    ]
}

/// Creates meta data for QtWidgets classes which are internally required, but not defined
/// in the Qt metatypes.json.
pub fn internal_widgets_classes() -> impl IntoIterator<Item = Class> {
    [
        Class {
            class_name: "QLayoutAttached".to_owned(),
            qualified_class_name: "QLayoutAttached".to_owned(),
            object: true,
            properties: vec![
                Property::new("alignment", "Qt::Alignment"),
                Property::new("column", "int"),
                Property::new("columnMinimumWidth", "int"),
                Property::new("columnSpan", "int"),
                Property::new("columnStretch", "int"),
                Property::new("row", "int"),
                Property::new("rowMinimumHeight", "int"),
                Property::new("rowSpan", "int"),
                Property::new("rowStretch", "int"),
            ],
            ..Default::default()
        },
        Class::new("QLayoutItem"), // super class of QLayout, QSpacerItem, and QWidgetItem
        Class {
            class_name: "QSpacerItem".to_owned(),
            qualified_class_name: "QSpacerItem".to_owned(),
            class_infos: vec![ClassInfo::new("QML.Element", "auto")],
            object: true, // mark as "creatable" object in .qmltypes
            properties: vec![
                // follows uic names
                Property::new("orientation", "Qt::Orientation"),
                Property::new("sizeHint", "QSize"),
                Property::new("sizeType", "QSizePolicy"),
            ],
            ..Default::default()
        },
        Class {
            class_name: "QTabWidgetAttached".to_owned(),
            qualified_class_name: "QTabWidgetAttached".to_owned(),
            object: true,
            properties: vec![
                Property::new("icon", "QIcon"),
                Property::new("title", "QString"),
                Property::new("toolTip", "QString"),
                Property::new("whatsThis", "QString"),
            ],
            ..Default::default()
        },
    ]
}
