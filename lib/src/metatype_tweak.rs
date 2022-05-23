//! Modifications on Qt metatypes data.

use crate::metatype::{Class, Property};

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
            _ => {}
        }
    }
}

fn fix_grid_layout(cls: &mut Class) {
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
            class_name: "QSpacerItem".to_owned(),
            qualified_class_name: "QSpacerItem".to_owned(),
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
