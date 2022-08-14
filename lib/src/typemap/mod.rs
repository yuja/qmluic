//! Manages types loaded from Qt metatypes.json.

use camino::Utf8PathBuf;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::mem;

mod class;
mod core;
mod enum_;
mod function;
mod module;
mod namespace;
mod primitive;
mod qml_component;
mod util;

pub use self::class::*; // re-export
pub use self::core::*; // re-export
pub use self::enum_::*; // re-export
pub use self::function::*; // re-export
pub use self::module::*; // re-export
pub use self::namespace::*; // re-export
pub use self::primitive::*; // re-export
pub use self::qml_component::*; // re-export
use self::util::TypeMapRef;

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
        let mut builtins =
            ModuleData::with_namespace(NamespaceData::with_primitive_types(&PrimitiveType::ALL));
        builtins.push_alias("qreal", "double").unwrap();

        TypeMap {
            builtins,
            named_module_map: HashMap::default(),
            directory_module_map: HashMap::default(),
        }
    }

    /// Checks if the specified module exists.
    pub fn contains_module(&self, id: ModuleId) -> bool {
        match id {
            ModuleId::Builtins => true,
            ModuleId::Named(name) => self.named_module_map.contains_key(name),
            ModuleId::Directory(path) => self.directory_module_map.contains_key(path),
        }
    }

    /// Looks up module by identifier.
    pub fn get_module<'a>(&'a self, id: ModuleId) -> Option<Namespace<'a>> {
        match id {
            ModuleId::Builtins => Some(self.builtins.to_namespace(TypeMapRef(self))),
            ModuleId::Named(name) => self
                .named_module_map
                .get(name)
                .map(|d| d.to_namespace(TypeMapRef(self))),
            ModuleId::Directory(path) => self
                .directory_module_map
                .get(path)
                .map(|d| d.to_namespace(TypeMapRef(self))),
        }
    }

    /// Looks up mutable module data by identifier.
    pub fn get_module_data<'a>(&'a self, id: ModuleId) -> Option<&'a ModuleData> {
        match id {
            ModuleId::Builtins => Some(&self.builtins),
            ModuleId::Named(name) => self.named_module_map.get(name),
            ModuleId::Directory(path) => self.directory_module_map.get(path),
        }
    }

    /// Looks up mutable module data by identifier.
    pub fn get_module_data_mut<'a>(&'a mut self, id: ModuleId) -> Option<&'a mut ModuleData> {
        match id {
            ModuleId::Builtins => Some(&mut self.builtins),
            ModuleId::Named(name) => self.named_module_map.get_mut(name),
            ModuleId::Directory(path) => self.directory_module_map.get_mut(path),
        }
    }

    /// Inserts new module.
    pub fn insert_module<S>(&mut self, id: S, data: ModuleData) -> Option<ModuleData>
    where
        S: Into<ModuleIdBuf>,
    {
        match id.into() {
            ModuleIdBuf::Builtins => Some(mem::replace(&mut self.builtins, data)),
            ModuleIdBuf::Named(name) => self.named_module_map.insert(name, data),
            ModuleIdBuf::Directory(path) => self.directory_module_map.insert(path, data),
        }
    }
}

/// Type variants returned by [`TypeMap`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum NamedType<'a> {
    Class(Class<'a>),
    Enum(Enum<'a>),
    Namespace(Namespace<'a>),
    Primitive(PrimitiveType),
    QmlComponent(QmlComponent<'a>),
}

impl<'a> NamedType<'a> {
    /// Returns true if this type is supposed to be passed by `const T &`.
    fn is_const_ref_preferred(&self) -> bool {
        match self {
            NamedType::Class(_) => true,
            NamedType::Enum(_) => false,
            NamedType::Namespace(_) => false, // invalid
            NamedType::Primitive(pt) => pt.is_const_ref_preferred(),
            NamedType::QmlComponent(_) => true,
        }
    }

    /// Turns this into class representation if supported by the underlying type.
    pub fn into_class(self) -> Option<Class<'a>> {
        match self {
            NamedType::Class(cls) => Some(cls),
            NamedType::Enum(_) => None,
            NamedType::Namespace(_) => None,
            NamedType::Primitive(pt) => pt.to_class(),
            NamedType::QmlComponent(ns) => Some(ns.into_class()),
        }
    }
}

impl<'a> TypeSpace<'a> for NamedType<'a> {
    fn name(&self) -> &str {
        match self {
            NamedType::Class(cls) => cls.name(),
            NamedType::Enum(en) => en.name(),
            NamedType::Namespace(ns) => ns.name(),
            NamedType::Primitive(pt) => pt.name(),
            NamedType::QmlComponent(_) => "",
        }
    }

    fn get_type(&self, name: &str) -> Option<Result<NamedType<'a>, TypeMapError>> {
        match self {
            NamedType::Class(cls) => cls.get_type(name),
            NamedType::Enum(_) => None,
            NamedType::Namespace(ns) => ns.get_type(name),
            NamedType::Primitive(_) => None,
            NamedType::QmlComponent(_) => None,
        }
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        match self {
            NamedType::Class(cls) => cls.lexical_parent(),
            NamedType::Enum(en) => en.lexical_parent(),
            NamedType::Namespace(ns) => ns.lexical_parent(),
            NamedType::Primitive(_) => None,
            NamedType::QmlComponent(_) => None,
        }
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Result<Enum<'a>, TypeMapError>> {
        match self {
            NamedType::Class(cls) => cls.get_enum_by_variant(name),
            NamedType::Enum(en) => en.get_enum_by_variant(name),
            NamedType::Namespace(ns) => ns.get_enum_by_variant(name),
            NamedType::Primitive(_) => None,
            NamedType::QmlComponent(_) => None,
        }
    }
}

/// Type variants for parent space.
#[derive(Clone, Eq, Hash, PartialEq)]
#[doc(hidden)] // this is implementation detail, but exposed by TypeSpace trait
pub enum ParentSpace<'a> {
    Class(Class<'a>),
    ImportedModuleSpace(ImportedModuleSpace<'a>),
    Namespace(Namespace<'a>),
}

impl<'a> TypeSpace<'a> for ParentSpace<'a> {
    fn name(&self) -> &str {
        match self {
            ParentSpace::Class(cls) => cls.name(),
            ParentSpace::ImportedModuleSpace(ns) => ns.name(),
            ParentSpace::Namespace(ns) => ns.name(),
        }
    }

    fn get_type(&self, name: &str) -> Option<Result<NamedType<'a>, TypeMapError>> {
        match self {
            ParentSpace::Class(cls) => cls.get_type(name),
            ParentSpace::ImportedModuleSpace(ns) => ns.get_type(name),
            ParentSpace::Namespace(ns) => ns.get_type(name),
        }
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        match self {
            ParentSpace::Class(cls) => cls.lexical_parent(),
            ParentSpace::ImportedModuleSpace(ns) => ns.lexical_parent(),
            ParentSpace::Namespace(ns) => ns.lexical_parent(),
        }
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Result<Enum<'a>, TypeMapError>> {
        match self {
            ParentSpace::Class(cls) => cls.get_enum_by_variant(name),
            ParentSpace::ImportedModuleSpace(ns) => ns.get_enum_by_variant(name),
            ParentSpace::Namespace(ns) => ns.get_enum_by_variant(name),
        }
    }
}

impl fmt::Debug for ParentSpace<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // don't print ancestor contents recursively, which would be quite noisy
        match self {
            Self::Class(cls) => f
                .debug_tuple("Class")
                .field(&format_args!("{:?}", cls.qualified_cxx_name()))
                .finish(),
            Self::ImportedModuleSpace(ns) => f
                .debug_tuple("ImportedModuleSpace")
                .field(&format_args!("{:?}", ns.qualified_cxx_name()))
                .finish(),
            Self::Namespace(ns) => f
                .debug_tuple("Namespace")
                .field(&format_args!("{:?}", ns.qualified_cxx_name()))
                .finish(),
        }
    }
}

/// Type of variables, properties, function arguments, etc.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TypeKind<'a> {
    /// Just a named (or value) type.
    ///
    /// The type should be a [`PrimitiveType`], [`Enum`], or gadget [`Class`].
    Just(NamedType<'a>),

    /// Pointer to a named object type.
    ///
    /// The type should be an object [`Class`]. It's unlikely we'll support a pointer to
    /// a value type.
    Pointer(NamedType<'a>),

    /// List of pointers to a named object type.
    ///
    /// The type should be an object [`Class`].
    ///
    /// This may be structured as `List(Box<Pointer(Type)>)`, but is flattened for convenience.
    PointerList(NamedType<'a>),
}

impl TypeKind<'_> {
    pub const BOOL: Self = TypeKind::Just(NamedType::Primitive(PrimitiveType::Bool));
    pub const DOUBLE: Self = TypeKind::Just(NamedType::Primitive(PrimitiveType::Double));
    pub const INT: Self = TypeKind::Just(NamedType::Primitive(PrimitiveType::Int));
    pub const UINT: Self = TypeKind::Just(NamedType::Primitive(PrimitiveType::Uint));
    pub const STRING: Self = TypeKind::Just(NamedType::Primitive(PrimitiveType::QString));
    pub const STRING_LIST: Self = TypeKind::Just(NamedType::Primitive(PrimitiveType::QStringList));
    pub const VARIANT: Self = TypeKind::Just(NamedType::Primitive(PrimitiveType::QVariant));
    pub const VOID: Self = TypeKind::Just(NamedType::Primitive(PrimitiveType::Void));

    /// Returns true if this type is supposed to be passed by `const T &`.
    pub fn is_const_ref_preferred(&self) -> bool {
        match self {
            TypeKind::Just(ty) => ty.is_const_ref_preferred(),
            TypeKind::Pointer(_) => false,
            TypeKind::PointerList(_) => true,
        }
    }

    pub fn qualified_cxx_name(&self) -> Cow<'_, str> {
        match self {
            TypeKind::Just(ty) => ty.qualified_cxx_name(),
            TypeKind::Pointer(ty) => ty.qualified_cxx_name() + "*",
            TypeKind::PointerList(ty) => format!("QList<{}*>", ty.qualified_cxx_name()).into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metatype;

    fn unwrap_class(r: Option<Result<NamedType, TypeMapError>>) -> Class {
        match r {
            Some(Ok(NamedType::Class(x))) => x,
            _ => panic!("unexpected type: {r:?}"),
        }
    }

    fn unwrap_enum(r: Option<Result<NamedType, TypeMapError>>) -> Enum {
        match r {
            Some(Ok(NamedType::Enum(x))) => x,
            _ => panic!("unexpected type: {r:?}"),
        }
    }

    #[test]
    fn type_eq() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let module_data = type_map.get_module_data_mut(module_id).unwrap();
        module_data.extend([metatype::Class::new("Foo"), metatype::Class::new("Bar")]);
        module_data.extend([metatype::Enum::new("Baz")]);

        let module = type_map.get_module(module_id).unwrap();
        assert_eq!(
            module.get_type("Foo").unwrap().unwrap(),
            module.get_type("Foo").unwrap().unwrap()
        );
        assert_ne!(
            module.get_type("Foo").unwrap().unwrap(),
            module.get_type("Bar").unwrap().unwrap()
        );
        assert_ne!(
            module.get_type("Foo").unwrap().unwrap(),
            module.get_type("Baz").unwrap().unwrap()
        );
        assert_eq!(
            module.get_type("Baz").unwrap().unwrap(),
            module.get_type("Baz").unwrap().unwrap()
        );
    }

    #[test]
    fn named_module() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        type_map
            .get_module_data_mut(module_id)
            .unwrap()
            .extend([metatype::Class::new("Foo")]);
        let module = type_map.get_module(module_id).unwrap();
        assert!(module.get_type("Foo").is_some());
        assert!(
            module.get_type("int").is_none(),
            "imported type not in namespace"
        );
    }

    #[test]
    fn aliased_type() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo");
        let mut module_data = ModuleData::default();
        module_data.extend([metatype::Class::new("Foo")]);
        module_data.push_alias("aliased_foo", "Foo").unwrap();
        type_map.insert_module(module_id.clone(), module_data);

        let module = type_map.get_module(module_id).unwrap();
        // generic type alias is purely an alias. no new type wouldn't be created.
        assert_eq!(
            module.get_type("aliased_foo").unwrap().unwrap(),
            module.get_type_scoped("Foo").unwrap().unwrap()
        );
    }

    #[test]
    fn aliased_enum_eq() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        type_map.get_module_data_mut(module_id).unwrap().extend([
            metatype::Enum::new("Foo"),
            metatype::Enum::new_flag("Foos", "Foo"),
        ]);

        let module = type_map.get_module(module_id).unwrap();
        // unlike generic type alias, aliased enum (or flag) is a separate type.
        assert_eq!(
            unwrap_enum(module.get_type("Foos"))
                .alias_enum()
                .unwrap()
                .unwrap(),
            unwrap_enum(module.get_type("Foo"))
        );
    }

    #[test]
    fn qualified_cxx_name() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut foo_meta = metatype::Class::new("Foo");
        foo_meta.enums.push(metatype::Enum::new("Bar"));
        type_map
            .get_module_data_mut(module_id)
            .unwrap()
            .extend([foo_meta]);

        let module = type_map.get_module(module_id).unwrap();
        assert_eq!(
            module
                .resolve_type("int")
                .unwrap()
                .unwrap()
                .qualified_cxx_name(),
            "int"
        );
        let foo_type = module.get_type("Foo").unwrap().unwrap();
        assert_eq!(foo_type.qualified_cxx_name(), "Foo");
        assert_eq!(
            foo_type
                .get_type("Bar")
                .unwrap()
                .unwrap()
                .qualified_cxx_name(),
            "Foo::Bar"
        );
    }

    #[test]
    fn get_type_of_root() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        type_map
            .get_module_data_mut(module_id)
            .unwrap()
            .extend([metatype::Class::new("Foo")]);

        let module = type_map.get_module(module_id).unwrap();
        assert_eq!(
            module.resolve_type("int").unwrap().unwrap(),
            NamedType::Primitive(PrimitiveType::Int)
        );
        let foo_class = unwrap_class(module.get_type("Foo"));
        assert!(foo_class.get_type("int").is_none());
        assert_eq!(
            foo_class.resolve_type("int").unwrap().unwrap(),
            NamedType::Primitive(PrimitiveType::Int)
        );
    }

    #[test]
    fn get_type_of_root_scoped() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut foo_meta = metatype::Class::new("Foo");
        foo_meta.enums.push(metatype::Enum::new("Bar"));
        type_map
            .get_module_data_mut(module_id)
            .unwrap()
            .extend([foo_meta]);

        let module = type_map.get_module(module_id).unwrap();
        let foo_class = unwrap_class(module.get_type("Foo"));
        assert_eq!(
            module.get_type_scoped("Foo").unwrap().unwrap(),
            module.get_type("Foo").unwrap().unwrap()
        );
        assert_eq!(
            module.get_type_scoped("Foo::Bar").unwrap().unwrap(),
            foo_class.get_type("Bar").unwrap().unwrap()
        );
        assert!(foo_class.get_type_scoped("Foo::Bar").is_none());
        assert_eq!(
            foo_class.resolve_type_scoped("Foo::Bar").unwrap().unwrap(),
            foo_class.get_type("Bar").unwrap().unwrap()
        );
    }

    #[test]
    fn do_not_resolve_intermediate_type_scoped() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        type_map
            .get_module_data_mut(module_id)
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
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut root_meta = metatype::Class::new("Root");
        root_meta.enums.push(metatype::Enum::new("RootEnum"));
        type_map.get_module_data_mut(module_id).unwrap().extend([
            root_meta,
            metatype::Class::with_supers("Sub1", ["Root"]),
            metatype::Class::with_supers("Sub2", ["Sub1"]),
        ]);

        let module = type_map.get_module(module_id).unwrap();
        let root_class = unwrap_class(module.get_type("Root"));
        let sub2_class = unwrap_class(module.get_type("Sub2"));
        assert_eq!(
            sub2_class.get_type("RootEnum").unwrap().unwrap(),
            root_class.get_type("RootEnum").unwrap().unwrap()
        );
        assert_eq!(
            module.get_type_scoped("Sub2::RootEnum").unwrap().unwrap(),
            root_class.get_type("RootEnum").unwrap().unwrap()
        );
    }

    #[test]
    fn class_derived_from() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        type_map.get_module_data_mut(module_id).unwrap().extend([
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
        let module_id = ModuleId::Named("foo");
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
            .get_module_data_mut(module_id)
            .unwrap()
            .extend([foo_meta, bar_meta]);

        let module = type_map.get_module(module_id).unwrap();
        let bar_class = unwrap_class(module.get_type("Bar"));
        assert_eq!(
            bar_class
                .get_property("bar_prop")
                .unwrap()
                .unwrap()
                .value_type(),
            &TypeKind::Just(module.resolve_type("bool").unwrap().unwrap())
        );
        assert_eq!(
            bar_class
                .get_property("foo_prop")
                .unwrap()
                .unwrap()
                .value_type(),
            &TypeKind::Just(module.resolve_type("int").unwrap().unwrap())
        );
    }

    #[test]
    fn get_type_of_property_decorated() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo");
        let mut module_data = ModuleData::with_builtins();
        let mut foo_meta = metatype::Class::new("Foo");
        foo_meta.properties.extend([
            metatype::Property::new("pointer", "Foo*"),
            metatype::Property::new("pointer_list", "QList<Foo*>"),
            metatype::Property::new("pointer_vector", "QVector<Foo*>"),
        ]);
        module_data.extend([foo_meta]);
        type_map.insert_module(module_id.clone(), module_data);

        let module = type_map.get_module(module_id).unwrap();
        let foo_class = unwrap_class(module.get_type("Foo"));
        assert_eq!(
            foo_class
                .get_property("pointer")
                .unwrap()
                .unwrap()
                .value_type(),
            &TypeKind::Pointer(NamedType::Class(foo_class.clone()))
        );
        assert_eq!(
            foo_class
                .get_property("pointer_list")
                .unwrap()
                .unwrap()
                .value_type(),
            &TypeKind::PointerList(NamedType::Class(foo_class.clone()))
        );
        assert_eq!(
            foo_class
                .get_property("pointer_vector")
                .unwrap()
                .unwrap()
                .value_type(),
            &TypeKind::PointerList(NamedType::Class(foo_class.clone()))
        );
    }

    #[test]
    fn get_enum_by_variant() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut foo_meta = metatype::Class::new("Foo");
        let unscoped_meta = metatype::Enum::with_values("Unscoped", ["X", "Y"]);
        let mut scoped_meta = metatype::Enum::with_values("Scoped", ["A", "Y"]);
        scoped_meta.is_class = true;
        foo_meta.enums.extend([unscoped_meta, scoped_meta]);
        type_map
            .get_module_data_mut(module_id)
            .unwrap()
            .extend([foo_meta]);

        let module = type_map.get_module(module_id).unwrap();
        let foo_class = unwrap_class(module.get_type("Foo"));
        let unscoped_enum = unwrap_enum(foo_class.get_type("Unscoped"));
        assert_eq!(
            foo_class.get_enum_by_variant("X").unwrap().unwrap(),
            unscoped_enum
        );
        assert_eq!(
            foo_class.get_enum_by_variant("Y").unwrap().unwrap(),
            unscoped_enum
        );
        assert!(foo_class.get_enum_by_variant("A").is_none());

        let scoped_enum = unwrap_enum(foo_class.get_type("Scoped"));
        assert!(unscoped_enum.get_enum_by_variant("X").is_none());
        assert_eq!(
            scoped_enum.get_enum_by_variant("A").unwrap().unwrap(),
            scoped_enum
        );
    }

    #[test]
    fn get_super_class_enum_by_variant() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo");
        type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
        let mut foo_meta = metatype::Class::new("Foo");
        let unscoped_meta = metatype::Enum::with_values("Unscoped", ["X", "Y"]);
        foo_meta.enums.extend([unscoped_meta]);
        let bar_meta = metatype::Class::with_supers("Bar", ["Foo"]);
        type_map
            .get_module_data_mut(module_id)
            .unwrap()
            .extend([foo_meta, bar_meta]);

        let module = type_map.get_module(module_id).unwrap();
        let foo_class = unwrap_class(module.get_type("Bar"));
        let bar_class = unwrap_class(module.get_type("Bar"));
        let unscoped_enum = unwrap_enum(foo_class.get_type("Unscoped"));
        assert_eq!(
            bar_class.get_enum_by_variant("X").unwrap().unwrap(),
            unscoped_enum
        );
    }
}
