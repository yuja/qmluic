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
    pub object: bool,

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
#[serde(untagged)]
enum MetatypesDoc {
    Nested(Vec<CompilationUnit>),
    Unit(CompilationUnit),
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
    serde_json::from_str(json_data).map(flatten_classes)
}

fn flatten_classes(doc: MetatypesDoc) -> Vec<Class> {
    match doc {
        MetatypesDoc::Nested(units) => units.into_iter().flat_map(|u| u.classes).collect(),
        MetatypesDoc::Unit(u) => u.classes,
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
