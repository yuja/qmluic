//! Modifications on Qt metatypes data.

use crate::metatype::{Class, Property};

/// Applies all modifications on the given `classes` data.
pub fn apply_all(classes: &mut Vec<Class>) {
    classes.extend(internal_classes());
}

/// Creates meta data for classes which are internally required, but not defined in
/// the Qt metatypes.json.
pub fn internal_classes() -> impl IntoIterator<Item = Class> {
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
        Class {
            class_name: "QSpacerItem".to_owned(),
            qualified_class_name: "QSpacerItem".to_owned(),
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
