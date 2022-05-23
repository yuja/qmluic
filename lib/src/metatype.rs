//! Qt metatypes.json data types.

use serde::{Deserialize, Serialize};

// TODO: Symbols are stored as owned strings, but we'll eventually want to intern them,
// where we can probably remove these temporary owned strings at all.

/// C++ access specifier.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum AccessSpecifier {
    Private,
    Protected,
    Public,
}

/// Class metadata.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Class {
    pub class_name: String,
    pub qualified_class_name: String,
    #[serde(default)]
    pub super_classes: Vec<SuperClassSpecifier>,
    #[serde(default)]
    pub class_infos: Vec<ClassInfo>,
    #[serde(default)]
    pub interfaces: Vec<Vec<Interface>>, // no idea why this is nested array
    #[serde(default)]
    pub object: bool,
    #[serde(default)]
    pub gadget: bool,
    #[serde(default)]
    pub namespace: bool,

    #[serde(default)]
    pub enums: Vec<Enum>,
    #[serde(default)]
    pub properties: Vec<Property>,
    #[serde(default)]
    pub constructors: Vec<Method>, // only QObject appears to define this
    #[serde(default)]
    pub signals: Vec<Method>,
    #[serde(default)]
    pub slots: Vec<Method>,
    #[serde(default)]
    pub methods: Vec<Method>,
}

impl Class {
    /// Creates class metadata of object type.
    pub fn new<S>(name: S) -> Self
    where
        S: Into<String>,
    {
        let name: String = name.into();
        Class {
            class_name: unqualify_name(&name).to_owned(),
            qualified_class_name: name,
            object: true,
            ..Default::default()
        }
    }

    /// Creates class metadata of gadget type.
    pub fn new_gadget<S>(name: S) -> Self
    where
        S: Into<String>,
    {
        let name: String = name.into();
        Class {
            class_name: unqualify_name(&name).to_owned(),
            qualified_class_name: name,
            gadget: true,
            ..Default::default()
        }
    }

    /// Creates class metadata of object type with public super classes.
    pub fn with_supers<S, I>(name: S, supers: I) -> Self
    where
        S: Into<String>,
        I: IntoIterator,
        I::Item: Into<String>,
    {
        let name: String = name.into();
        let super_classes = supers
            .into_iter()
            .map(SuperClassSpecifier::public)
            .collect();
        Class {
            class_name: unqualify_name(&name).to_owned(),
            qualified_class_name: name,
            object: true,
            super_classes,
            ..Default::default()
        }
    }
}

fn unqualify_name(name: &str) -> &str {
    name.rsplit_once("::").map(|(_, n)| n).unwrap_or(name)
}

/// Super class reference with the access specifier.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SuperClassSpecifier {
    pub name: String,
    pub access: AccessSpecifier,
}

impl SuperClassSpecifier {
    /// Creates public super class specifier.
    pub fn public<S>(name: S) -> Self
    where
        S: Into<String>,
    {
        SuperClassSpecifier {
            name: name.into(),
            access: AccessSpecifier::Public,
        }
    }
}

/// Extra class metadata. (e.g. `""QML.Element"`" name)
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ClassInfo {
    pub name: String,
    pub value: String,
}

impl ClassInfo {
    /// Creates class info pair.
    pub fn new<S, T>(name: S, value: T) -> Self
    where
        S: Into<String>,
        T: Into<String>,
    {
        ClassInfo {
            name: name.into(),
            value: value.into(),
        }
    }
}

/// Qt plugin interface identifier. (see `Q_DECLARE_INTERFACE()`)
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Interface {
    pub class_name: String,
    pub id: String,
}

/// Enum (and flag) metadata.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Enum {
    pub name: String,
    pub alias: Option<String>,
    pub is_class: bool,
    pub is_flag: bool,
    pub values: Vec<String>,
}

impl Enum {
    /// Creates enum metadata.
    pub fn new<S>(name: S) -> Self
    where
        S: Into<String>,
    {
        Enum {
            name: name.into(),
            ..Default::default()
        }
    }

    /// Creates flag metadata.
    pub fn new_flag<S, T>(name: S, alias: T) -> Self
    where
        S: Into<String>,
        T: Into<String>,
    {
        Enum {
            name: name.into(),
            alias: Some(alias.into()),
            is_flag: true,
            ..Default::default()
        }
    }

    /// Creates enum metadata with values.
    pub fn with_values<S, I>(name: S, values: I) -> Self
    where
        S: Into<String>,
        I: IntoIterator,
        I::Item: Into<String>,
    {
        Enum {
            name: name.into(),
            values: values.into_iter().map(|n| n.into()).collect(),
            ..Default::default()
        }
    }
}

/// Property metadata.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Property {
    pub name: String,
    pub r#type: String,

    /// See `Q_PRIVATE_PROPERTY()`.
    pub private_class: Option<String>,

    pub member: Option<String>,
    pub read: Option<String>,
    pub write: Option<String>,
    pub reset: Option<String>,
    pub notify: Option<String>,
    pub bindable: Option<String>,

    #[serde(default)]
    pub revision: u32,
    pub designable: bool,
    pub scriptable: bool,
    pub stored: bool,
    pub user: bool,
    pub constant: bool,
    pub r#final: bool,
    pub required: bool,

    /// Property index in the current meta object. (new in Qt 6)
    pub index: Option<i32>,
}

impl Property {
    /// Creates property metadata.
    pub fn new<S, T>(name: S, type_name: T) -> Self
    where
        S: Into<String>,
        T: Into<String>,
    {
        Property {
            name: name.into(),
            r#type: type_name.into(),
            ..Default::default()
        }
    }

    /// Creates property metadata of final.
    pub fn new_final<S, T>(name: S, type_name: T) -> Self
    where
        S: Into<String>,
        T: Into<String>,
    {
        Property {
            name: name.into(),
            r#type: type_name.into(),
            r#final: true,
            ..Default::default()
        }
    }
}

impl Default for Property {
    fn default() -> Self {
        Self {
            name: String::new(),
            r#type: String::new(),
            private_class: None,
            member: None,
            read: None,
            write: None,
            reset: None,
            notify: None,
            bindable: None,
            revision: 0,
            designable: true,
            scriptable: true,
            stored: true,
            user: false,
            constant: false,
            r#final: false,
            required: false,
            index: None,
        }
    }
}

/// Signal, slot, or callable function metadata.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Method {
    pub name: String,
    pub access: AccessSpecifier,
    pub return_type: String,
    #[serde(default)]
    pub arguments: Vec<Argument>,
    #[serde(default)]
    pub revision: u32,
}

impl Default for Method {
    fn default() -> Self {
        Self {
            name: String::new(),
            access: AccessSpecifier::Public,
            return_type: String::new(),
            arguments: vec![],
            revision: 0,
        }
    }
}

/// Signal, slot, or callable function argument.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Argument {
    pub name: Option<String>,
    pub r#type: String,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CompilationUnit {
    #[serde(default)]
    pub classes: Vec<Class>,
    pub input_file: Option<String>,
    #[serde(default)]
    pub output_revision: i32,
}

/// Collects all classes from metatypes.json document.
///
/// The JSON document should consist of either a compilation unit object `{"classes": ...}`
/// or an array of such objects `[{"classes": ...}, ...]`.
pub fn extract_classes_from_str(json_data: &str) -> serde_json::Result<Vec<Class>> {
    // Don't use untagged enum to dispatch [{...}] and {...}. Doing that would move any
    // parsing error to the document level and line/column information would be lost.
    if json_data.trim_start().starts_with('{') {
        let unit: CompilationUnit = serde_json::from_str(json_data)?;
        Ok(unit.classes)
    } else {
        let units: Vec<CompilationUnit> = serde_json::from_str(json_data)?;
        Ok(units.into_iter().flat_map(|u| u.classes).collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deserialize_class_decl() {
        let data = r###"{
            "className": "QAction",
            "enums": [
                {
                    "isClass": false,
                    "isFlag": false,
                    "name": "Priority",
                    "values": [
                        "LowPriority",
                        "NormalPriority",
                        "HighPriority"
                    ]
                }
            ],
            "object": true,
            "properties": [
                {
                    "constant": false,
                    "designable": true,
                    "final": false,
                    "name": "checkable",
                    "notify": "changed",
                    "read": "isCheckable",
                    "required": false,
                    "scriptable": true,
                    "stored": true,
                    "type": "bool",
                    "user": false,
                    "write": "setCheckable"
                }
            ],
            "qualifiedClassName": "QAction",
            "signals": [
                {
                    "access": "public",
                    "name": "changed",
                    "returnType": "void"
                },
                {
                    "access": "public",
                    "arguments": [
                        {
                            "name": "checked",
                            "type": "bool"
                        }
                    ],
                    "name": "triggered",
                    "returnType": "void"
                }
            ],
            "slots": [
                {
                    "access": "public",
                    "name": "trigger",
                    "returnType": "void"
                },
                {
                    "access": "public",
                    "name": "hover",
                    "returnType": "void"
                },
                {
                    "access": "public",
                    "arguments": [
                        {
                            "type": "bool"
                        }
                    ],
                    "name": "setChecked",
                    "returnType": "void"
                }
            ],
            "superClasses": [
                {
                    "access": "public",
                    "name": "QObject"
                }
            ]
        }"###;
        let _meta: Class = serde_json::from_str(data).unwrap();
    }

    #[test]
    fn extract_classes_unit() {
        let data = r###"{
            "classes": [
                {"className": "Foo", "qualifiedClassName": "Foo", "object": true},
                {"className": "Bar", "qualifiedClassName": "Bar", "object": true}
            ]
        }"###;
        let classes = extract_classes_from_str(data).unwrap();
        assert_eq!(classes.len(), 2);
    }

    #[test]
    fn extract_classes_nested() {
        let data = r###"[
            {"classes": [{"className": "Foo", "qualifiedClassName": "Foo", "object": true}]},
            {"classes": [{"className": "Bar", "qualifiedClassName": "Bar", "object": true}]}
        ]"###;
        let classes = extract_classes_from_str(data).unwrap();
        assert_eq!(classes.len(), 2);
    }
}
