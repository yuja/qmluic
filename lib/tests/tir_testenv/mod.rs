//! Environment and utility for TIR tests.

use qmluic::diagnostic::Diagnostics;
use qmluic::metatype;
use qmluic::opcode::BuiltinFunctionKind;
use qmluic::qmlast::{Node, UiObjectDefinition, UiProgram};
use qmluic::qmldoc::UiDocument;
use qmluic::tir::{self, CodeBody};
use qmluic::typedexpr::{RefKind, RefSpace, TypeAnnotationSpace};
use qmluic::typemap::{
    Class, ImportedModuleSpace, ModuleData, ModuleId, NamedType, TypeKind, TypeMap, TypeMapError,
    TypeSpace as _,
};

pub struct Env {
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
        self.try_build_with(expr_source, tir::build)
    }

    pub fn build_callback(&self, expr_source: &str) -> CodeBody {
        self.try_build_callback(expr_source).unwrap()
    }

    pub fn try_build_callback(&self, expr_source: &str) -> Result<CodeBody, Diagnostics> {
        self.try_build_with(expr_source, tir::build_callback)
    }

    fn try_build_with<'a>(
        &'a self,
        expr_source: &str,
        build: impl FnOnce(&Context<'a>, Node, &str, &mut Diagnostics) -> Option<CodeBody<'a>>,
    ) -> Result<CodeBody<'a>, Diagnostics> {
        let mut type_space = ImportedModuleSpace::new(&self.type_map);
        assert!(type_space.import_module(ModuleId::Builtins));
        assert!(type_space.import_module(&self.module_id));
        let ctx = Context { type_space };

        let doc = UiDocument::parse(format!("A {{ a: {expr_source}}}"), "MyType", None);
        let program = UiProgram::from_node(doc.root_node(), doc.source()).unwrap();
        let obj = UiObjectDefinition::from_node(program.root_object_node(), doc.source()).unwrap();
        let map = obj.build_binding_map(doc.source()).unwrap();
        let node = map.get("a").unwrap().get_node().unwrap();

        let mut diagnostics = Diagnostics::new();
        build(&ctx, node, doc.source(), &mut diagnostics).ok_or(diagnostics)
    }
}

struct Context<'a> {
    type_space: ImportedModuleSpace<'a>,
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

impl<'a> TypeAnnotationSpace<'a> for Context<'a> {
    fn get_annotated_type_scoped(
        &self,
        scoped_name: &str,
    ) -> Option<Result<TypeKind<'a>, TypeMapError>> {
        self.type_space.get_type_scoped(scoped_name).map(|r| {
            r.map(|ty| {
                if ty.name() == "Foo" {
                    TypeKind::Pointer(ty)
                } else {
                    TypeKind::Just(ty)
                }
            })
        })
    }
}

pub fn dump(expr_source: &str) -> String {
    let env = Env::new();
    let code = env.build(expr_source);
    dump_code(&code)
}

pub fn dump_code(code: &CodeBody) -> String {
    let mut buf = Vec::new();
    tir::dump_code_body(&mut buf, &code).unwrap();
    String::from_utf8(buf).unwrap()
}
