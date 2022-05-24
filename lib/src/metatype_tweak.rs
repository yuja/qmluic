//! Modifications on Qt metatypes data.

use crate::metatype::{Class, ClassInfo, Enum, Property};

/// Applies all modifications on the given `classes` data.
pub fn apply_all(classes: &mut Vec<Class>) {
    fix_classes(classes);
    classes.extend(internal_core_classes());
    classes.extend(internal_widgets_classes());
}

/// Updates existing class meta data.
pub fn fix_classes(classes: &mut [Class]) {
    for cls in classes.iter_mut() {
        match cls.qualified_class_name.as_ref() {
            "QGridLayout" => fix_grid_layout(cls),
            "QLayout" => fix_layout(cls),
            "QSizePolicy" => fix_size_policy(cls),
            "QWidget" => fix_widget(cls),
            _ => {}
        }
    }
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
    ]
}
