//! Environment and utility for TIR tests.

#![cfg(test)]

use super::builder;
use super::core::CodeBody;
use super::dump;
use crate::diagnostic::Diagnostics;
use crate::metatype;
use crate::opcode::BuiltinFunctionKind;
use crate::qmlast::{UiObjectDefinition, UiProgram};
use crate::qmldoc::UiDocument;
use crate::typedexpr::{RefKind, RefSpace};
use crate::typemap::{
    Class, ModuleData, ModuleId, NamedType, Namespace, TypeMap, TypeMapError, TypeSpace as _,
};

pub(super) struct Env {
    type_map: TypeMap,
    module_id: ModuleId<'static>,
}

impl Env {
    pub fn new() -> Self {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo".into());
        let mut module_data = ModuleData::with_builtins();
        let foo_meta = metatype::Class {
            class_name: "Foo".to_owned(),
            qualified_class_name: "Foo".to_owned(),
            object: true,
            enums: vec![metatype::Enum::with_values(
                "Bar",
                ["Bar0", "Bar1", "Bar2", "Bar3"],
            )],
            properties: vec![
                metatype::Property {
                    name: "checked".to_owned(),
                    r#type: "bool".to_owned(),
                    read: Some("isChecked".to_owned()),
                    write: Some("setChecked".to_owned()),
                    notify: Some("toggled".to_owned()),
                    ..Default::default()
                },
                metatype::Property {
                    name: "currentIndex".to_owned(),
                    r#type: "int".to_owned(),
                    read: Some("currentIndex".to_owned()),
                    write: Some("setCurrentIndex".to_owned()),
                    notify: Some("currentIndexChanged".to_owned()),
                    ..Default::default()
                },
                metatype::Property {
                    name: "text".to_owned(),
                    r#type: "QString".to_owned(),
                    read: Some("text".to_owned()),
                    write: Some("setText".to_owned()),
                    notify: Some("textChanged".to_owned()),
                    ..Default::default()
                },
            ],
            slots: vec![metatype::Method {
                name: "done".to_owned(),
                access: metatype::AccessSpecifier::Public,
                return_type: "void".to_owned(),
                arguments: vec![metatype::Argument {
                    name: None,
                    r#type: "int".to_owned(),
                }],
                ..Default::default()
            }],
            ..Default::default()
        };
        module_data.extend([foo_meta, metatype::Class::new("A")]);
        type_map.insert_module(module_id.clone(), module_data);
        Env {
            type_map,
            module_id,
        }
    }

    pub fn build(&self, expr_source: &str) -> CodeBody {
        self.try_build(expr_source).unwrap()
    }

    pub fn try_build(&self, expr_source: &str) -> Result<CodeBody, Diagnostics> {
        let ctx = Context {
            type_space: self.type_map.get_module(&self.module_id).unwrap(),
        };

        let doc = UiDocument::parse(format!("A {{ a: {expr_source}}}"), "MyType", None);
        let program = UiProgram::from_node(doc.root_node(), doc.source()).unwrap();
        let obj = UiObjectDefinition::from_node(program.root_object_node(), doc.source()).unwrap();
        let map = obj.build_binding_map(doc.source()).unwrap();
        let node = map.get("a").unwrap().get_node().unwrap();

        let mut diagnostics = Diagnostics::new();
        builder::build(&ctx, node, doc.source(), &mut diagnostics).ok_or(diagnostics)
    }
}

struct Context<'a> {
    type_space: Namespace<'a>,
}

impl<'a> RefSpace<'a> for Context<'a> {
    fn get_ref(&self, name: &str) -> Option<Result<RefKind<'a>, TypeMapError>> {
        match name {
            "foo" | "foo2" | "foo3" | "foo4" => {
                match self.type_space.get_type("Foo").unwrap().unwrap() {
                    NamedType::Class(cls) => Some(Ok(RefKind::Object(cls))),
                    _ => panic!("Foo must be of class type"),
                }
            }
            "qsTr" => Some(Ok(RefKind::BuiltinFunction(BuiltinFunctionKind::Tr))),
            _ => self.type_space.get_ref(name),
        }
    }

    fn this_object(&self) -> Option<(Class<'a>, String)> {
        None
    }
}

pub(super) fn dump(expr_source: &str) -> String {
    let env = Env::new();
    let code = env.build(expr_source);
    let mut buf = Vec::new();
    dump::dump_code_body(&mut buf, &code).unwrap();
    String::from_utf8(buf).unwrap()
}