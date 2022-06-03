//! Manages types loaded from Qt metatypes.json.

use camino::Utf8PathBuf;
use std::collections::HashMap;
use std::mem;

mod class;
mod core;
mod enum_;
mod module;
mod namespace;
mod qml_component;

pub use self::class::*; // re-export
pub use self::core::*; // re-export
pub use self::enum_::*; // re-export
pub use self::module::*; // re-export
pub use self::namespace::*; // re-export
pub use self::qml_component::*; // re-export

/// Storage to map type name to class or enum representation.
#[derive(Debug)]
pub struct TypeMap {
    builtins: ModuleData,
    named_module_map: HashMap<String, ModuleData>,
    directory_module_map: HashMap<Utf8PathBuf, ModuleData>,
}

impl TypeMap {
    /// Creates empty type map.
    pub fn empty() -> Self {
        TypeMap {
            builtins: ModuleData::default(),
            named_module_map: HashMap::default(),
            directory_module_map: HashMap::default(),
        }
    }

    /// Creates type map containing the primitive types.
    pub fn with_primitive_types() -> Self {
        use PrimitiveType::*;

        let builtins = ModuleData::with_namespace(NamespaceData::with_primitive_types(&[
            Bool, Int, QReal, QString, UInt, Void,
        ]));

        TypeMap {
            builtins,
            named_module_map: HashMap::default(),
            directory_module_map: HashMap::default(),
        }
    }

    /// Checks if the specified module exists.
    pub fn contains_module<'s, S>(&self, id: S) -> bool
    where
        S: AsRef<ModuleId<'s>>,
    {
        match id.as_ref() {
            ModuleId::Builtins => true,
            ModuleId::Named(name) => self.named_module_map.contains_key(name.as_ref()),
            ModuleId::Directory(path) => self.directory_module_map.contains_key(path.as_ref()),
        }
    }

    /// Looks up module by identifier.
    pub fn get_module<'a, 's, S>(&'a self, id: S) -> Option<Module<'a>>
    where
        S: AsRef<ModuleId<'s>>,
    {
        match id.as_ref() {
            ModuleId::Builtins => Some(Module::new(&self.builtins, self)),
            ModuleId::Named(name) => self
                .named_module_map
                .get(name.as_ref())
                .map(|d| Module::new(d, self)),
            ModuleId::Directory(path) => self
                .directory_module_map
                .get(path.as_ref())
                .map(|d| Module::new(d, self)),
        }
    }

    /// Looks up mutable module data by identifier.
    pub fn get_module_data<'a, 's, S>(&'a self, id: S) -> Option<&'a ModuleData>
    where
        S: AsRef<ModuleId<'s>>,
    {
        match id.as_ref() {
            ModuleId::Builtins => Some(&self.builtins),
            ModuleId::Named(name) => self.named_module_map.get(name.as_ref()),
            ModuleId::Directory(path) => self.directory_module_map.get(path.as_ref()),
        }
    }

    /// Looks up mutable module data by identifier.
    pub fn get_module_data_mut<'a, 's, S>(&'a mut self, id: S) -> Option<&'a mut ModuleData>
    where
        S: AsRef<ModuleId<'s>>,
    {
        match id.as_ref() {
            ModuleId::Builtins => Some(&mut self.builtins),
            ModuleId::Named(name) => self.named_module_map.get_mut(name.as_ref()),
            ModuleId::Directory(path) => self.directory_module_map.get_mut(path.as_ref()),
        }
    }

    /// Inserts new module.
    pub fn insert_module<S>(&mut self, id: S, data: ModuleData) -> Option<ModuleData>
    where
        S: Into<ModuleId<'static>>,
    {
        match id.into() {
            ModuleId::Builtins => Some(mem::replace(&mut self.builtins, data)),
            ModuleId::Named(name) => self.named_module_map.insert(name.into_owned(), data),
            ModuleId::Directory(path) => self.directory_module_map.insert(path.into_owned(), data),
        }
    }
}

/// Type variants returned by [`TypeMap`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type<'a> {
    Class(Class<'a>),
    Enum(Enum<'a>),
    Namespace(Namespace<'a>),
    Primitive(PrimitiveType),
    QmlComponent(QmlComponent<'a>),
}

impl<'a> TypeSpace<'a> for Type<'a> {
    fn name(&self) -> &str {
        match self {
            Type::Class(cls) => cls.name(),
            Type::Enum(en) => en.name(),
            Type::Namespace(ns) => ns.name(),
            Type::Primitive(pt) => pt.name(),
            Type::QmlComponent(ns) => ns.name(),
        }
    }

    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        match self {
            Type::Class(cls) => cls.get_type(name),
            Type::Enum(_) => None,
            Type::Namespace(ns) => ns.get_type(name),
            Type::Primitive(_) => None,
            Type::QmlComponent(ns) => ns.get_type(name),
        }
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        match self {
            Type::Class(cls) => cls.lexical_parent(),
            Type::Enum(en) => en.lexical_parent(),
            Type::Namespace(ns) => ns.lexical_parent(),
            Type::Primitive(_) => None,
            Type::QmlComponent(ns) => ns.lexical_parent(),
        }
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        match self {
            Type::Class(cls) => cls.get_enum_by_variant(name),
            Type::Enum(en) => en.get_enum_by_variant(name),
            Type::Namespace(ns) => ns.get_enum_by_variant(name),
            Type::Primitive(_) => None,
            Type::QmlComponent(ns) => ns.get_enum_by_variant(name),
        }
    }
}

/// Type variants for parent space.
#[derive(Clone, Debug, Eq, PartialEq)]
#[doc(hidden)] // this is implementation detail, but exposed by TypeSpace trait
pub enum ParentSpace<'a> {
    Class(Class<'a>),
    Module(Module<'a>),
    Namespace(Namespace<'a>),
    QmlComponent(QmlComponent<'a>),
}

impl<'a> TypeSpace<'a> for ParentSpace<'a> {
    fn name(&self) -> &str {
        match self {
            ParentSpace::Class(cls) => cls.name(),
            ParentSpace::Module(ns) => ns.name(),
            ParentSpace::Namespace(ns) => ns.name(),
            ParentSpace::QmlComponent(ns) => ns.name(),
        }
    }

    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        match self {
            ParentSpace::Class(cls) => cls.get_type(name),
            ParentSpace::Module(ns) => ns.get_type(name),
            ParentSpace::Namespace(ns) => ns.get_type(name),
            ParentSpace::QmlComponent(ns) => ns.get_type(name),
        }
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        match self {
            ParentSpace::Class(cls) => cls.lexical_parent(),
            ParentSpace::Module(ns) => ns.lexical_parent(),
            ParentSpace::Namespace(ns) => ns.lexical_parent(),
            ParentSpace::QmlComponent(ns) => ns.lexical_parent(),
        }
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        match self {
            ParentSpace::Class(cls) => cls.get_enum_by_variant(name),
            ParentSpace::Module(ns) => ns.get_enum_by_variant(name),
            ParentSpace::Namespace(ns) => ns.get_enum_by_variant(name),
            ParentSpace::QmlComponent(ns) => ns.get_enum_by_variant(name),
        }
    }
}

/// Value types provided by C++ language and Qt runtime.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrimitiveType {
    Bool,
    Int,
    QReal,
    QString,
    UInt,
    Void,
    // TODO: ...
}

impl PrimitiveType {
    pub const fn name(&self) -> &'static str {
        use PrimitiveType::*;
        match self {
            Bool => "bool",
            Int => "int",
            QReal => "qreal",
            QString => "QString",
            UInt => "uint",
            Void => "void",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metatype;

    fn unwrap_class(ty: Option<Type>) -> Class {
        match ty {
            Some(Type::Class(x)) => x,
            _ => panic!("unexpected type: {ty:?}"),
        }
    }

    fn unwrap_enum(ty: Option<Type>) -> Enum {
        match ty {
            Some(Type::Enum(x)) => x,
            _ => panic!("unexpected type: {ty:?}"),
        }
    }

    #[test]
    fn type_eq() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let module_data = type_map.get_module_data_mut(&module_id).unwrap();
        module_data.extend([metatype::Class::new("Foo"), metatype::Class::new("Bar")]);
        module_data.extend([metatype::Enum::new("Baz")]);

        let module = type_map.get_module(module_id).unwrap();
        assert_eq!(
            module.get_type("Foo").unwrap(),
            module.get_type("Foo").unwrap()
        );
        assert_ne!(
            module.get_type("Foo").unwrap(),
            module.get_type("Bar").unwrap()
        );
        assert_ne!(
            module.get_type("Foo").unwrap(),
            module.get_type("Baz").unwrap()
        );
        assert_eq!(
            module.get_type("Baz").unwrap(),
            module.get_type("Baz").unwrap()
        );
    }

    #[test]
    fn named_module() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        type_map
            .get_module_data_mut(&module_id)
            .unwrap()
            .extend([metatype::Class::new("Foo")]);
        let module = type_map.get_module(&ModuleId::Named("foo".into())).unwrap();
        assert!(module.get_type("Foo").is_some());
        assert!(module.get_type("int").is_some()); // TODO: only for imported type resolution
    }

    #[test]
    fn aliased_enum_eq() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        type_map.get_module_data_mut(&module_id).unwrap().extend([
            metatype::Enum::new("Foo"),
            metatype::Enum::new_flag("Foos", "Foo"),
        ]);

        let module = type_map.get_module(module_id).unwrap();
        assert_eq!(
            unwrap_enum(module.get_type("Foos")).alias_enum(),
            Some(unwrap_enum(module.get_type("Foo")))
        );
    }

    #[test]
    fn qualified_name() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut foo_meta = metatype::Class::new("Foo");
        foo_meta.enums.push(metatype::Enum::new("Bar"));
        type_map
            .get_module_data_mut(&module_id)
            .unwrap()
            .extend([foo_meta]);

        let module = type_map.get_module(module_id).unwrap();
        assert_eq!(module.get_type("int").unwrap().qualified_name(), "int");
        let foo_type = module.get_type("Foo").unwrap();
        assert_eq!(foo_type.qualified_name(), "Foo");
        assert_eq!(
            foo_type.get_type("Bar").unwrap().qualified_name(),
            "Foo::Bar"
        );
    }

    #[test]
    fn get_type_of_root() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        type_map
            .get_module_data_mut(&module_id)
            .unwrap()
            .extend([metatype::Class::new("Foo")]);

        let module = type_map.get_module(module_id).unwrap();
        assert_eq!(
            module.get_type("int").unwrap(),
            Type::Primitive(PrimitiveType::Int)
        );
        let foo_class = unwrap_class(module.get_type("Foo"));
        assert!(foo_class.get_type("int").is_none());
        assert_eq!(
            foo_class.resolve_type("int").unwrap(),
            Type::Primitive(PrimitiveType::Int)
        );
    }

    #[test]
    fn get_type_of_root_scoped() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut foo_meta = metatype::Class::new("Foo");
        foo_meta.enums.push(metatype::Enum::new("Bar"));
        type_map
            .get_module_data_mut(&module_id)
            .unwrap()
            .extend([foo_meta]);

        let module = type_map.get_module(module_id).unwrap();
        let foo_class = unwrap_class(module.get_type("Foo"));
        assert_eq!(
            module.get_type_scoped("Foo").unwrap(),
            module.get_type("Foo").unwrap()
        );
        assert_eq!(
            module.get_type_scoped("Foo::Bar").unwrap(),
            foo_class.get_type("Bar").unwrap()
        );
        assert!(foo_class.get_type_scoped("Foo::Bar").is_none());
        assert_eq!(
            foo_class.resolve_type_scoped("Foo::Bar").unwrap(),
            foo_class.get_type("Bar").unwrap()
        );
    }

    #[test]
    fn do_not_resolve_intermediate_type_scoped() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        type_map
            .get_module_data_mut(&module_id)
            .unwrap()
            .extend([metatype::Class::new("Foo")]);

        let module = type_map.get_module(module_id).unwrap();
        let foo_class = unwrap_class(module.get_type("Foo"));
        assert!(foo_class.resolve_type_scoped("int").is_some());
        assert_eq!(
            unwrap_class(foo_class.resolve_type_scoped("Foo")),
            foo_class
        );
        assert!(foo_class.resolve_type_scoped("Foo::int").is_none());
    }

    #[test]
    fn get_super_class_type() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut root_meta = metatype::Class::new("Root");
        root_meta.enums.push(metatype::Enum::new("RootEnum"));
        type_map.get_module_data_mut(&module_id).unwrap().extend([
            root_meta,
            metatype::Class::with_supers("Sub1", ["Root"]),
            metatype::Class::with_supers("Sub2", ["Sub1"]),
        ]);

        let module = type_map.get_module(module_id).unwrap();
        let root_class = unwrap_class(module.get_type("Root"));
        let sub2_class = unwrap_class(module.get_type("Sub2"));
        assert_eq!(
            sub2_class.get_type("RootEnum").unwrap(),
            root_class.get_type("RootEnum").unwrap()
        );
        assert_eq!(
            module.get_type_scoped("Sub2::RootEnum").unwrap(),
            root_class.get_type("RootEnum").unwrap()
        );
    }

    #[test]
    fn class_derived_from() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        type_map.get_module_data_mut(&module_id).unwrap().extend([
            metatype::Class::new("Root"),
            metatype::Class::with_supers("Sub1", ["Root"]),
            metatype::Class::with_supers("Sub2", ["Sub1"]),
            metatype::Class::with_supers("Sub3", ["Root"]),
        ]);

        let module = type_map.get_module(module_id).unwrap();
        let root_class = unwrap_class(module.get_type("Root"));
        let sub1_class = unwrap_class(module.get_type("Sub1"));
        let sub2_class = unwrap_class(module.get_type("Sub2"));
        let sub3_class = unwrap_class(module.get_type("Sub3"));

        assert!(root_class.is_derived_from(&root_class));
        assert!(sub1_class.is_derived_from(&root_class));
        assert!(sub2_class.is_derived_from(&root_class));
        assert!(sub3_class.is_derived_from(&root_class));

        assert!(!sub3_class.is_derived_from(&sub1_class));
        assert!(!sub3_class.is_derived_from(&sub2_class));

        assert!(!root_class.is_derived_from(&sub1_class));
    }

    #[test]
    fn get_type_of_property() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut foo_meta = metatype::Class::new("Foo");
        foo_meta
            .properties
            .push(metatype::Property::new("foo_prop", "int"));
        let mut bar_meta = metatype::Class::with_supers("Bar", ["Foo"]);
        bar_meta
            .properties
            .push(metatype::Property::new("bar_prop", "bool"));
        type_map
            .get_module_data_mut(&module_id)
            .unwrap()
            .extend([foo_meta, bar_meta]);

        let module = type_map.get_module(module_id).unwrap();
        let bar_class = unwrap_class(module.get_type("Bar"));
        assert_eq!(
            bar_class.get_property_type("bar_prop").unwrap(),
            module.get_type("bool").unwrap()
        );
        assert_eq!(
            bar_class.get_property_type("foo_prop").unwrap(),
            module.get_type("int").unwrap()
        );
    }

    #[test]
    fn get_enum_by_variant() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut foo_meta = metatype::Class::new("Foo");
        let unscoped_meta = metatype::Enum::with_values("Unscoped", ["X", "Y"]);
        let mut scoped_meta = metatype::Enum::with_values("Scoped", ["A", "Y"]);
        scoped_meta.is_class = true;
        foo_meta.enums.extend([unscoped_meta, scoped_meta]);
        type_map
            .get_module_data_mut(&module_id)
            .unwrap()
            .extend([foo_meta]);

        let module = type_map.get_module(module_id).unwrap();
        let foo_class = unwrap_class(module.get_type("Foo"));
        let unscoped_enum = unwrap_enum(foo_class.get_type("Unscoped"));
        assert_eq!(foo_class.get_enum_by_variant("X").unwrap(), unscoped_enum);
        assert_eq!(foo_class.get_enum_by_variant("Y").unwrap(), unscoped_enum);
        assert!(foo_class.get_enum_by_variant("A").is_none());

        let scoped_enum = unwrap_enum(foo_class.get_type("Scoped"));
        assert!(unscoped_enum.get_enum_by_variant("X").is_none());
        assert_eq!(scoped_enum.get_enum_by_variant("A").unwrap(), scoped_enum);
    }

    #[test]
    fn get_super_class_enum_by_variant() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo".into());
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut foo_meta = metatype::Class::new("Foo");
        let unscoped_meta = metatype::Enum::with_values("Unscoped", ["X", "Y"]);
        foo_meta.enums.extend([unscoped_meta]);
        let bar_meta = metatype::Class::with_supers("Bar", ["Foo"]);
        type_map
            .get_module_data_mut(&module_id)
            .unwrap()
            .extend([foo_meta, bar_meta]);

        let module = type_map.get_module(module_id).unwrap();
        let foo_class = unwrap_class(module.get_type("Bar"));
        let bar_class = unwrap_class(module.get_type("Bar"));
        let unscoped_enum = unwrap_enum(foo_class.get_type("Unscoped"));
        assert_eq!(bar_class.get_enum_by_variant("X").unwrap(), unscoped_enum);
    }
}
