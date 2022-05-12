//! Qt metatypes.json data types.

use serde::Deserialize;

// TODO: Symbols are stored as owned strings, but we'll eventually want to intern them,
// where we can probably remove these temporary owned strings at all.

/// C++ access specifier.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum AccessSpecifier {
    Private,
    Protected,
    Public,
}

/// Class metadata.
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Class {
    pub class_name: String,
    pub qualified_class_name: String,
    #[serde(default)]
    pub super_classes: Vec<SuperClassSpecifier>,
    // TODO: class_infos
    // TODO: interfaces
    #[serde(default)]
    pub object: bool,
    #[serde(default)]
    pub gadget: bool,

    #[serde(default)]
    pub enums: Vec<Enum>,
    #[serde(default)]
    pub properties: Vec<Property>,
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
        S: AsRef<str>,
    {
        Self::with_basic_params(name.as_ref(), true)
    }

    /// Creates class metadata of gadget type.
    pub fn new_gadget<S>(name: S) -> Self
    where
        S: AsRef<str>,
    {
        Self::with_basic_params(name.as_ref(), false)
    }

    /// Creates class metadata of object type with public super classes.
    pub fn with_supers<S, I>(name: S, supers: I) -> Self
    where
        S: AsRef<str>,
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        let mut cls = Self::with_basic_params(name.as_ref(), true);
        cls.super_classes = supers
            .into_iter()
            .map(|n| SuperClassSpecifier {
                name: n.as_ref().to_owned(),
                access: AccessSpecifier::Public,
            })
            .collect();
        cls
    }

    fn with_basic_params(name: &str, object: bool) -> Self {
        Class {
            class_name: unqualify_name(name).to_owned(),
            qualified_class_name: name.to_owned(),
            super_classes: vec![],
            object: object,
            gadget: !object,
            enums: vec![],
            properties: vec![],
            signals: vec![],
            slots: vec![],
            methods: vec![],
        }
    }
}

fn unqualify_name(name: &str) -> &str {
    name.rsplit_once("::").map(|(_, n)| n).unwrap_or(name)
}

/// Super class reference with the access specifier.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct SuperClassSpecifier {
    pub name: String,
    pub access: AccessSpecifier,
}

/// Enum (and flag) metadata.
#[derive(Clone, Debug, Deserialize)]
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
        S: AsRef<str>,
    {
        Enum {
            name: name.as_ref().to_owned(),
            alias: None,
            is_class: false,
            is_flag: false,
            values: vec![],
        }
    }

    /// Creates flag metadata.
    pub fn new_flag<S, T>(name: S, alias: T) -> Self
    where
        S: AsRef<str>,
        T: AsRef<str>,
    {
        Enum {
            name: name.as_ref().to_owned(),
            alias: Some(alias.as_ref().to_owned()),
            is_class: false,
            is_flag: true,
            values: vec![],
        }
    }

    /// Creates enum metadata with values.
    pub fn with_values<S, I>(name: S, values: I) -> Self
    where
        S: AsRef<str>,
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        Enum {
            name: name.as_ref().to_owned(),
            alias: None,
            is_class: false,
            is_flag: false,
            values: values.into_iter().map(|n| n.as_ref().to_owned()).collect(),
        }
    }
}

/// Property metadata.
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Property {
    pub name: String,
    pub r#type: String,

    pub member: Option<String>,
    pub read: Option<String>,
    pub write: Option<String>,
    pub reset: Option<String>,
    pub notify: Option<String>,

    #[serde(default)]
    pub revision: u32,
    pub designable: bool,
    pub scriptable: bool,
    pub stored: bool,
    pub user: bool,
    pub constant: bool,
    pub r#final: bool,
    pub required: bool,
}

impl Property {
    /// Creates property metadata.
    pub fn new<S, T>(name: S, type_name: T) -> Self
    where
        S: AsRef<str>,
        T: AsRef<str>,
    {
        Property {
            name: name.as_ref().to_owned(),
            r#type: type_name.as_ref().to_owned(),
            member: None,
            read: None,
            write: None,
            reset: None,
            notify: None,
            revision: 0,
            designable: true,
            scriptable: true,
            stored: true,
            user: false,
            constant: false,
            r#final: false,
            required: false,
        }
    }
}

/// Signal, slot, or callable function metadata.
#[derive(Clone, Debug, Deserialize)]
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

/// Signal, slot, or callable function argument.
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Argument {
    pub name: Option<String>,
    pub r#type: String,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct CompilationUnit {
    classes: Vec<Class>,
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
