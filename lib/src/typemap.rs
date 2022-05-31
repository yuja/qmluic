//! Manages types loaded from Qt metatypes.json.

use super::metatype;
use camino::{Utf8Path, Utf8PathBuf};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::mem;
use std::ptr;

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
        use TypeIndex::Primitive;

        let name_map = HashMap::from(
            [Bool, Int, QReal, QString, UInt, Void].map(|t| (t.name().to_owned(), Primitive(t))),
        );
        let builtins = ModuleData {
            namespace: NamespaceData {
                name_map,
                ..Default::default()
            },
            imports: vec![],
        };

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

/// Top-level module identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ModuleId<'s> {
    /// Built-in types.
    Builtins,
    /// QML module which can be imported by (dotted) name.
    Named(Cow<'s, str>),
    /// Directory containing QML files, which can be imported by string.
    ///
    /// The path must be normalized in a certain form. Typically it is an absolute path.
    Directory(Cow<'s, Utf8Path>),
}

impl<'s> AsRef<ModuleId<'s>> for ModuleId<'s> {
    fn as_ref(&self) -> &ModuleId<'s> {
        self
    }
}

/// Interface to look up type by name.
pub trait TypeSpace<'a> {
    /// Name of this type.
    fn name(&self) -> &str;

    /// Scoped name of this type from the root.
    ///
    /// This is primarily designed for diagnostics.
    fn qualified_name(&self) -> Cow<'_, str> {
        if let Some(ty) = self.lexical_parent() {
            // ty may be the root namespace
            if ty.name().is_empty() && ty.lexical_parent().is_none() {
                Cow::Borrowed(self.name())
            } else {
                Cow::Owned(format!("{}::{}", ty.qualified_name(), self.name()))
            }
        } else {
            // primitive type can't return the root namespace as parent
            Cow::Borrowed(self.name())
        }
    }

    /// Looks up type by name.
    ///
    /// If this type is a class, the returned type may be inherited from one of the super classes.
    fn get_type(&self, name: &str) -> Option<Type<'a>>;

    /// Looks up type by scoped name.
    fn get_type_scoped(&self, scoped_name: &str) -> Option<Type<'a>> {
        // no support for the ::<top-level> notation
        let mut parts = scoped_name.split("::");
        let head = self.get_type(parts.next().expect("split should have at least one name"));
        parts.fold(head, |outer, name| outer.and_then(|ty| ty.get_type(name)))
    }

    /// Parent space of this type.
    ///
    /// This is unrelated to the super type of the class inheritance.
    fn lexical_parent(&self) -> Option<&Type<'a>>;

    /// Looks up type by name from this towards the parent type space.
    fn resolve_type(&self, name: &str) -> Option<Type<'a>> {
        self.get_type(name)
            .or_else(|| self.lexical_parent().and_then(|ty| ty.resolve_type(name)))
    }

    /// Looks up type by scoped name from this towards the parent type space.
    fn resolve_type_scoped(&self, scoped_name: &str) -> Option<Type<'a>> {
        // no support for the ::<top-level> notation
        let mut parts = scoped_name.split("::");
        let head = self.resolve_type(parts.next().expect("split should have at least one name"));
        // "resolve" only the first node. the remainder should be direct child.
        parts.fold(head, |outer, name| outer.and_then(|ty| ty.get_type(name)))
    }

    /// Looks up enum type by variant name.
    ///
    /// The returned type is not an abstract [`Type`] since Qt metatype does not support
    /// an arbitrary static constant.
    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>>;
}

/// Represents a type map in tree (or a namespace), which is created temporarily by borrowing
/// type maps.
#[derive(Clone, Debug)]
pub struct Namespace<'a> {
    data: &'a NamespaceData,
    type_map: &'a TypeMap,
    // TODO: parent_space
}

/// Stored type namespace.
#[derive(Clone, Debug, Default)]
pub struct NamespaceData {
    name_map: HashMap<String, TypeIndex>,
    classes: Vec<ClassData>,
    enums: Vec<EnumData>,
    enum_variant_map: HashMap<String, usize>,
    qml_components: Vec<QmlComponentData>,
}

impl<'a> Namespace<'a> {
    // TODO: map C++ namespace to this type
    /*
    fn root(data: &'a NamespaceData, type_map: &'a TypeMap) -> Self {
        Namespace { data, type_map }
    }
    */
}

impl<'a> PartialEq for Namespace<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data) && ptr::eq(self.type_map, other.type_map)
        /*&& self.parent_space == other.parent_space*/
    }
}

impl<'a> Eq for Namespace<'a> {}

impl<'a> TypeSpace<'a> for Namespace<'a> {
    fn name(&self) -> &str {
        "" // TODO: return name if not root namespace
    }

    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        self.data
            .get_type_with(name, self.type_map, || Type::Namespace(self.clone()))
    }

    fn lexical_parent(&self) -> Option<&Type<'a>> {
        None // TODO: return some if not root namespace
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        self.data
            .get_enum_by_variant_with(name, || Type::Namespace(self.clone()))
    }
}

impl NamespaceData {
    // TODO: redesign mutation API

    fn extend_classes<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = metatype::Class>,
    {
        let start = self.classes.len();
        for (i, meta) in iter.into_iter().enumerate() {
            // TODO: use qualified type name and insert namespace node accordingly
            let name = meta.class_name.clone();
            self.classes.push(ClassData::from_meta(meta));
            self.name_map.insert(name, TypeIndex::Class(i + start));
        }
    }

    fn extend_enums<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = metatype::Enum>,
    {
        let start = self.enums.len();
        for (i, meta) in iter.into_iter().enumerate() {
            let name = meta.name.clone();
            let data = EnumData::from_meta(meta);
            let index = i + start;
            self.name_map.insert(name, TypeIndex::Enum(index));
            if !data.is_class {
                self.enum_variant_map
                    .extend(data.variants.iter().map(|v| (v.to_owned(), index)));
            }
            self.enums.push(data);
        }
    }

    fn push_qml_component(&mut self, data: QmlComponentData) {
        let start = self.qml_components.len();
        let name = data.class.class_name.clone();
        self.qml_components.push(data);
        self.name_map.insert(name, TypeIndex::QmlComponent(start));
    }

    fn get_type_with<'a, F>(
        &'a self,
        name: &str,
        type_map: &'a TypeMap,
        make_parent_space: F,
    ) -> Option<Type<'a>>
    where
        F: FnOnce() -> Type<'a>,
    {
        self.name_map.get(name).map(|&index| match index {
            TypeIndex::Class(i) => {
                Type::Class(Class::new(&self.classes[i], type_map, make_parent_space()))
            }
            TypeIndex::Enum(i) => Type::Enum(Enum::new(&self.enums[i], make_parent_space())),
            TypeIndex::Primitive(t) => Type::Primitive(t),
            TypeIndex::QmlComponent(i) => {
                Type::QmlComponent(QmlComponent::new(&self.qml_components[i], type_map))
            }
        })
    }

    fn get_enum_by_variant_with<'a, F>(
        &'a self,
        name: &str,
        make_parent_space: F,
    ) -> Option<Enum<'a>>
    where
        F: FnOnce() -> Type<'a>,
    {
        self.enum_variant_map
            .get(name)
            .map(|&i| Enum::new(&self.enums[i], make_parent_space()))
    }
}

/// Represents a top-level namespace with imports list.
#[derive(Clone, Debug)]
pub struct Module<'a> {
    data: &'a ModuleData,
    type_map: &'a TypeMap,
}

/// Stored top-level namespace with imports list.
#[derive(Clone, Debug, Default)]
pub struct ModuleData {
    namespace: NamespaceData,
    imports: Vec<ModuleId<'static>>,
}

impl<'a> Module<'a> {
    pub fn new(data: &'a ModuleData, type_map: &'a TypeMap) -> Self {
        Module { data, type_map }
    }
}

impl<'a> PartialEq for Module<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data) && ptr::eq(self.type_map, other.type_map)
    }
}

impl<'a> Eq for Module<'a> {}

impl<'a> TypeSpace<'a> for Module<'a> {
    fn name(&self) -> &str {
        "" // TODO
    }

    // TODO: separate function to get imported/exported (i.e. private/public) types
    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        self.data
            .namespace
            .get_type_with(name, self.type_map, || Type::Module(self.clone()))
            .or_else(|| self.data.get_imported_type(name, self.type_map))
    }

    fn lexical_parent(&self) -> Option<&Type<'a>> {
        None
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        self.data
            .namespace
            .get_enum_by_variant_with(name, || Type::Module(self.clone()))
    }
}

impl ModuleData {
    // TODO: redesign mutation API

    /// Creates new module storage with builtin imports.
    pub fn with_builtins() -> Self {
        ModuleData {
            namespace: NamespaceData::default(),
            imports: vec![ModuleId::Builtins],
        }
    }

    /// Adds the specified module to the import list from which type names will be resolved.
    pub fn import_module<S>(&mut self, id: S)
    where
        S: Into<ModuleId<'static>>,
    {
        self.imports.push(id.into())
    }

    fn get_imported_type<'a>(&'a self, name: &str, type_map: &'a TypeMap) -> Option<Type<'a>> {
        // TODO: detect cycle
        // TODO: module not found error?
        self.imports
            .iter()
            .find_map(|id| type_map.get_module(id).and_then(|ns| ns.get_type(name)))
    }

    pub fn push_qml_component(&mut self, data: QmlComponentData) {
        self.namespace.push_qml_component(data);
    }
}

impl Extend<metatype::Class> for ModuleData {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = metatype::Class>,
    {
        self.namespace.extend_classes(iter)
    }
}

impl Extend<metatype::Enum> for ModuleData {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = metatype::Enum>,
    {
        self.namespace.extend_enums(iter)
    }
}

/// Index to type variant stored in [`NamespaceData`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TypeIndex {
    Class(usize),
    Enum(usize),
    Primitive(PrimitiveType),
    QmlComponent(usize),
}

/// Type variants returned by [`TypeMap`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type<'a> {
    Class(Class<'a>),
    Enum(Enum<'a>),
    Module(Module<'a>),
    Namespace(Namespace<'a>),
    Primitive(PrimitiveType),
    QmlComponent(QmlComponent<'a>),
}

impl<'a> TypeSpace<'a> for Type<'a> {
    fn name(&self) -> &str {
        match self {
            Type::Class(cls) => cls.name(),
            Type::Enum(en) => en.name(),
            Type::Module(ns) => ns.name(),
            Type::Namespace(ns) => ns.name(),
            Type::Primitive(pt) => pt.name(),
            Type::QmlComponent(ns) => ns.name(),
        }
    }

    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        match self {
            Type::Class(cls) => cls.get_type(name),
            Type::Enum(_) => None,
            Type::Module(ns) => ns.get_type(name),
            Type::Namespace(ns) => ns.get_type(name),
            Type::Primitive(_) => None,
            Type::QmlComponent(ns) => ns.get_type(name),
        }
    }

    fn lexical_parent(&self) -> Option<&Type<'a>> {
        match self {
            Type::Class(cls) => cls.lexical_parent(),
            Type::Enum(en) => en.lexical_parent(),
            Type::Module(ns) => ns.lexical_parent(),
            Type::Namespace(ns) => ns.lexical_parent(),
            Type::Primitive(_) => None,
            Type::QmlComponent(ns) => ns.lexical_parent(),
        }
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        match self {
            Type::Class(cls) => cls.get_enum_by_variant(name),
            Type::Enum(en) => en.get_enum_by_variant(name),
            Type::Module(ns) => ns.get_enum_by_variant(name),
            Type::Namespace(ns) => ns.get_enum_by_variant(name),
            Type::Primitive(_) => None,
            Type::QmlComponent(ns) => ns.get_enum_by_variant(name),
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

/// QObject (or gadget) class representation.
#[derive(Clone, Debug)]
pub struct Class<'a> {
    data: &'a ClassData,
    type_map: &'a TypeMap,
    parent_space: Box<Type<'a>>, // should be either Class or Namespace
}

/// Stored class representation.
#[derive(Clone, Debug)]
struct ClassData {
    class_name: String,
    public_super_class_names: Vec<String>,
    inner_type_map: NamespaceData,
    property_map: HashMap<String, PropertyData>,
    // TODO
}

impl<'a> Class<'a> {
    fn new(data: &'a ClassData, type_map: &'a TypeMap, parent_space: Type<'a>) -> Self {
        Class {
            data,
            type_map,
            parent_space: Box::new(parent_space),
        }
    }

    pub fn public_super_classes<'b>(&'b self) -> impl Iterator<Item = Class<'a>> + 'b {
        self.data.public_super_class_names.iter().filter_map(|n| {
            match self.parent_space.resolve_type_scoped(n) {
                Some(Type::Class(x)) => Some(x),
                Some(Type::QmlComponent(ns)) => Some(ns.to_class()),
                Some(Type::Enum(_) | Type::Module(_) | Type::Namespace(_) | Type::Primitive(_))
                | None => None, // TODO: error?
            }
        })
    }

    pub fn is_derived_from(&self, base: &Class) -> bool {
        if self == base {
            true
        } else {
            self.public_super_classes().any(|c| c.is_derived_from(base))
        }
    }

    /// Looks up type of the specified property.
    pub fn get_property_type(&self, name: &str) -> Option<Type<'a>> {
        // TODO: error out if type name can't be resolved?
        self.data
            .property_map
            .get(name)
            .and_then(|p| self.resolve_type_scoped(&p.type_name))
            .or_else(|| {
                self.public_super_classes()
                    .find_map(|cls| cls.get_property_type(name))
            })
    }
}

impl<'a> PartialEq for Class<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data)
            && ptr::eq(self.type_map, other.type_map)
            && self.parent_space == other.parent_space
    }
}

impl<'a> Eq for Class<'a> {}

impl<'a> TypeSpace<'a> for Class<'a> {
    fn name(&self) -> &str {
        &self.data.class_name
    }

    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        // TODO: detect cycle in super-class chain
        self.data
            .inner_type_map
            .get_type_with(name, self.type_map, || Type::Class(self.clone()))
            .or_else(|| {
                self.public_super_classes()
                    .find_map(|cls| cls.get_type(name))
            })
    }

    fn lexical_parent(&self) -> Option<&Type<'a>> {
        Some(&self.parent_space)
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        // TODO: detect cycle in super-class chain
        self.data
            .inner_type_map
            .get_enum_by_variant_with(name, || Type::Class(self.clone()))
            .or_else(|| {
                self.public_super_classes()
                    .find_map(|cls| cls.get_enum_by_variant(name))
            })
    }
}

impl ClassData {
    fn from_meta(meta: metatype::Class) -> Self {
        let public_super_class_names = meta
            .super_classes
            .into_iter()
            .filter_map(|s| {
                if s.access == metatype::AccessSpecifier::Public {
                    Some(s.name)
                } else {
                    None
                }
            })
            .collect();

        let mut inner_type_map = NamespaceData::default();
        inner_type_map.extend_enums(meta.enums);

        let property_map = meta
            .properties
            .into_iter()
            .map(PropertyData::pair_from_meta)
            .collect();

        ClassData {
            class_name: meta.class_name,
            public_super_class_names,
            inner_type_map,
            property_map,
        }
    }
}

/// Stored property representation.
#[derive(Clone, Debug)]
struct PropertyData {
    type_name: String,
}

impl PropertyData {
    fn pair_from_meta(meta: metatype::Property) -> (String, Self) {
        let data = PropertyData {
            type_name: meta.r#type,
        };
        (meta.name, data)
    }
}

/// Enum representation.
#[derive(Clone, Debug)]
pub struct Enum<'a> {
    data: &'a EnumData,
    parent_space: Box<Type<'a>>, // should be either Class or Namespace
}

/// Stored enum representation.
#[derive(Clone, Debug)]
struct EnumData {
    name: String,
    alias: Option<String>,
    is_class: bool,
    is_flag: bool,
    variants: Vec<String>,
    variant_set: HashSet<String>,
}

impl<'a> Enum<'a> {
    fn new(data: &'a EnumData, parent_space: Type<'a>) -> Self {
        Enum {
            data,
            parent_space: Box::new(parent_space),
        }
    }

    pub fn alias_enum(&self) -> Option<Enum<'a>> {
        self.data
            .alias
            .as_ref()
            .and_then(|n| match self.parent_space.resolve_type_scoped(n) {
                Some(Type::Enum(x)) => Some(x),
                _ => None, // TODO: error?
            })
    }

    pub fn is_scoped(&self) -> bool {
        self.data.is_class
    }

    pub fn is_flag(&self) -> bool {
        self.data.is_flag
    }

    pub fn variants(&self) -> impl Iterator<Item = &str> {
        self.data.variants.iter().map(String::as_str)
    }

    pub fn contains_variant(&self, name: &str) -> bool {
        self.data.variant_set.contains(name)
    }

    pub fn qualify_variant_name(&self, name: &str) -> String {
        if self.is_scoped() {
            format!("{}::{}", self.qualified_name(), name)
        } else if let Some(p) = self.lexical_parent() {
            format!("{}::{}", p.qualified_name(), name)
        } else {
            // enum should have a parent space, but if there were none, returning the variant
            // name would make sense.
            name.to_owned()
        }
    }
}

impl<'a> PartialEq for Enum<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data) && self.parent_space == other.parent_space
    }
}

impl<'a> Eq for Enum<'a> {}

impl<'a> TypeSpace<'a> for Enum<'a> {
    fn name(&self) -> &str {
        &self.data.name
    }

    fn get_type(&self, _name: &str) -> Option<Type<'a>> {
        None
    }

    fn lexical_parent(&self) -> Option<&Type<'a>> {
        Some(&self.parent_space)
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
        if self.is_scoped() && self.contains_variant(name) {
            Some(self.clone())
        } else {
            None
        }
    }
}

impl EnumData {
    fn from_meta(meta: metatype::Enum) -> Self {
        let variant_set = HashSet::from_iter(meta.values.iter().cloned());
        EnumData {
            name: meta.name,
            alias: meta.alias,
            is_class: meta.is_class,
            is_flag: meta.is_flag,
            variants: meta.values,
            variant_set,
        }
    }
}

/// QML component representation.
///
/// This is basically a class inside an anonymous namespace. Since this represents
/// the namespace part, no types defined in the QML component will be returned from
/// this `TypeSpace`. Use [`QmlComponent::to_class`] to get the class part.
#[derive(Clone, Debug)]
pub struct QmlComponent<'a> {
    data: &'a QmlComponentData,
    type_map: &'a TypeMap,
}

/// Stored QML component representation.
#[derive(Clone, Debug)]
pub struct QmlComponentData {
    imports: Vec<ModuleId<'static>>,
    class: ClassData,
}

impl<'a> QmlComponent<'a> {
    pub fn new(data: &'a QmlComponentData, type_map: &'a TypeMap) -> Self {
        QmlComponent { data, type_map }
    }

    /// Returns the inner class representation of this QML component.
    pub fn to_class(&self) -> Class<'a> {
        // TODO: inner enum has to be qualified whereas inner class (or inline component) isn't
        Class::new(
            &self.data.class,
            self.type_map,
            Type::QmlComponent(self.clone()),
        )
    }
}

impl<'a> PartialEq for QmlComponent<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data) && ptr::eq(self.type_map, other.type_map)
    }
}

impl<'a> Eq for QmlComponent<'a> {}

impl<'a> TypeSpace<'a> for QmlComponent<'a> {
    fn name(&self) -> &str {
        "" // TODO
    }

    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        self.data.get_imported_type(name, self.type_map)
    }

    fn lexical_parent(&self) -> Option<&Type<'a>> {
        None
    }

    fn get_enum_by_variant(&self, _name: &str) -> Option<Enum<'a>> {
        None
    }
}

impl QmlComponentData {
    // TODO: redesign constructor or builder API
    pub fn with_super<S, T>(name: S, super_name: T) -> Self
    where
        S: Into<String>,
        T: Into<String>,
    {
        QmlComponentData {
            imports: vec![ModuleId::Builtins],
            class: ClassData {
                class_name: name.into(),
                public_super_class_names: vec![super_name.into()],
                inner_type_map: NamespaceData::default(),
                property_map: HashMap::new(),
            },
        }
    }

    /// Adds the specified module to the import list from which type names will be resolved.
    pub fn import_module<S>(&mut self, id: S)
    where
        S: Into<ModuleId<'static>>,
    {
        self.imports.push(id.into())
    }

    fn get_imported_type<'a>(&'a self, name: &str, type_map: &'a TypeMap) -> Option<Type<'a>> {
        // TODO: detect cycle
        // TODO: module not found error?
        self.imports
            .iter()
            .find_map(|id| type_map.get_module(id).and_then(|ns| ns.get_type(name)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
