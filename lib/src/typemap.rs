//! Manages types loaded from Qt metatypes.json.

use super::metatype;
use std::collections::HashMap;
use std::ptr;

/// Storage to map type name to class or enum representation.
#[derive(Clone, Debug)]
pub struct TypeMap {
    data: TypeMapData,
}

/// Stored type map.
#[derive(Clone, Debug, Default)]
struct TypeMapData {
    name_map: HashMap<String, TypeIndex>,
    classes: Vec<ClassData>,
    enums: Vec<EnumData>,
}

impl TypeMap {
    /// Creates empty type map.
    pub fn new() -> Self {
        TypeMap {
            data: Default::default(),
        }
    }

    /// Creates type map containing the primitive types.
    pub fn with_primitive_types() -> Self {
        use PrimitiveType::*;
        use TypeIndex::Primitive;

        let name_map = HashMap::from([
            ("bool".to_owned(), Primitive(Bool)),
            ("int".to_owned(), Primitive(Int)),
            ("qreal".to_owned(), Primitive(QReal)),
            ("QString".to_owned(), Primitive(QString)),
            ("uint".to_owned(), Primitive(UInt)),
            ("void".to_owned(), Primitive(Void)),
        ]);

        TypeMap {
            data: TypeMapData {
                name_map,
                ..Default::default()
            },
        }
    }

    /// Root type space.
    pub fn root(&self) -> Namespace {
        Namespace::root(&self.data)
    }

    /// Looks up type by name.
    pub fn get_type<'a>(&'a self, name: &str) -> Option<Type<'a>> {
        self.root().get_type(name)
    }

    /// Looks up type by scoped name.
    pub fn get_type_scoped<'a>(&'a self, scoped_name: &str) -> Option<Type<'a>> {
        self.root().get_type_scoped(scoped_name)
    }
}

impl Extend<metatype::Class> for TypeMap {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = metatype::Class>,
    {
        self.data.extend_classes(iter)
    }
}

impl Extend<metatype::Enum> for TypeMap {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = metatype::Enum>,
    {
        self.data.extend_enums(iter)
    }
}

impl TypeMapData {
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
            self.enums.push(EnumData::from_meta(meta));
            self.name_map.insert(name, TypeIndex::Enum(i + start));
        }
    }

    fn get_type_with<'a, F>(&'a self, name: &str, make_parent_space: F) -> Option<Type<'a>>
    where
        F: FnOnce() -> Type<'a>,
    {
        self.name_map.get(name).map(|&index| match index {
            TypeIndex::Class(i) => Type::Class(Class::new(&self.classes[i], make_parent_space())),
            TypeIndex::Enum(i) => Type::Enum(Enum::new(&self.enums[i], make_parent_space())),
            TypeIndex::Primitive(t) => Type::Primitive(t),
        })
    }
}

/// Interface to look up type by name.
pub trait TypeSpace<'a> {
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

    /// Looks up type by name from this towards the parent type space.
    fn resolve_type(&self, name: &str) -> Option<Type<'a>>;

    /// Looks up type by scoped name from this towards the parent type space.
    fn resolve_type_scoped(&self, scoped_name: &str) -> Option<Type<'a>> {
        // no support for the ::<top-level> notation
        let mut parts = scoped_name.split("::");
        let head = self.resolve_type(parts.next().expect("split should have at least one name"));
        // "resolve" only the first node. the remainder should be direct child.
        parts.fold(head, |outer, name| outer.and_then(|ty| ty.get_type(name)))
    }
}

/// Represents a type map in tree (or a namespace), which is created temporarily by borrowing
/// type maps.
#[derive(Clone, Debug)]
pub struct Namespace<'a> {
    data: &'a TypeMapData,
    // TODO: parent_space
}

impl<'a> Namespace<'a> {
    fn root(data: &'a TypeMapData) -> Self {
        Namespace { data }
    }
}

impl<'a> PartialEq for Namespace<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data) /*&& self.parent_space == other.parent_space*/
    }
}

impl<'a> Eq for Namespace<'a> {}

impl<'a> TypeSpace<'a> for Namespace<'a> {
    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        self.data
            .get_type_with(name, || Type::Namespace(self.clone()))
    }

    fn resolve_type(&self, name: &str) -> Option<Type<'a>> {
        self.get_type(name) /* or_else( .. parent_space ..) */
    }
}

/// Index to type variant stored in [`TypeMapData`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TypeIndex {
    Class(usize),
    Enum(usize),
    Primitive(PrimitiveType),
}

/// Type variants returned by [`TypeMap`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type<'a> {
    Class(Class<'a>),
    Enum(Enum<'a>),
    Namespace(Namespace<'a>),
    Primitive(PrimitiveType),
}

impl<'a> TypeSpace<'a> for Type<'a> {
    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        match self {
            Type::Class(cls) => cls.get_type(name),
            Type::Enum(_) => None,
            Type::Namespace(node) => node.get_type(name),
            Type::Primitive(_) => None,
        }
    }

    fn resolve_type(&self, name: &str) -> Option<Type<'a>> {
        match self {
            Type::Class(cls) => cls.resolve_type(name),
            Type::Enum(_) => None,
            Type::Namespace(node) => node.resolve_type(name),
            Type::Primitive(_) => None,
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

/// QObject (or gadget) class representation.
#[derive(Clone, Debug)]
pub struct Class<'a> {
    data: &'a ClassData,
    parent_space: Box<Type<'a>>, // should be either Class or Namespace
}

/// Stored class representation.
#[derive(Clone, Debug)]
struct ClassData {
    class_name: String,
    public_super_class_names: Vec<String>,
    inner_type_map: TypeMapData,
    property_map: HashMap<String, PropertyData>,
    // TODO
}

impl<'a> Class<'a> {
    fn new(data: &'a ClassData, parent_space: Type<'a>) -> Self {
        Class {
            data,
            parent_space: Box::new(parent_space),
        }
    }

    pub fn name(&self) -> &str {
        &self.data.class_name
    }

    pub fn public_super_classes<'b>(&'b self) -> impl Iterator<Item = Class<'a>> + 'b {
        self.data.public_super_class_names.iter().filter_map(|n| {
            match self.parent_space.resolve_type_scoped(n) {
                Some(Type::Class(x)) => Some(x),
                _ => None, // TODO: error?
            }
        })
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
        ptr::eq(self.data, other.data) && self.parent_space == other.parent_space
    }
}

impl<'a> Eq for Class<'a> {}

impl<'a> TypeSpace<'a> for Class<'a> {
    fn get_type(&self, name: &str) -> Option<Type<'a>> {
        // TODO: detect cycle in super-class chain
        self.data
            .inner_type_map
            .get_type_with(name, || Type::Class(self.clone()))
            .or_else(|| self.public_super_classes().find_map(|ty| ty.get_type(name)))
    }

    fn resolve_type(&self, name: &str) -> Option<Type<'a>> {
        self.get_type(name)
            .or_else(|| self.parent_space.resolve_type(name))
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

        let mut inner_type_map = TypeMapData::default();
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
    values: Vec<String>,
}

impl<'a> Enum<'a> {
    fn new(data: &'a EnumData, parent_space: Type<'a>) -> Self {
        Enum {
            data,
            parent_space: Box::new(parent_space),
        }
    }

    pub fn name(&self) -> &str {
        &self.data.name
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

    pub fn values(&self) -> impl Iterator<Item = &str> {
        self.data.values.iter().map(String::as_str)
    }
}

impl<'a> PartialEq for Enum<'a> {
    fn eq(&self, other: &Self) -> bool {
        // two types should be equal if both borrow the identical data.
        ptr::eq(self.data, other.data) && self.parent_space == other.parent_space
    }
}

impl<'a> Eq for Enum<'a> {}

impl EnumData {
    fn from_meta(meta: metatype::Enum) -> Self {
        EnumData {
            name: meta.name,
            alias: meta.alias,
            is_class: meta.is_class,
            is_flag: meta.is_flag,
            values: meta.values,
        }
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
        let mut type_map = TypeMap::new();
        type_map.extend([metatype::Class::new("Foo"), metatype::Class::new("Bar")]);
        type_map.extend([metatype::Enum::new("Baz")]);
        assert_eq!(
            type_map.get_type("Foo").unwrap(),
            type_map.get_type("Foo").unwrap()
        );
        assert_ne!(
            type_map.get_type("Foo").unwrap(),
            type_map.get_type("Bar").unwrap()
        );
        assert_ne!(
            type_map.get_type("Foo").unwrap(),
            type_map.get_type("Baz").unwrap()
        );
        assert_eq!(
            type_map.get_type("Baz").unwrap(),
            type_map.get_type("Baz").unwrap()
        );
    }

    #[test]
    fn aliased_enum_eq() {
        let mut type_map = TypeMap::new();
        type_map.extend([
            metatype::Enum::new("Foo"),
            metatype::Enum::new_flag("Foos", "Foo"),
        ]);
        assert_eq!(
            unwrap_enum(type_map.get_type("Foos")).alias_enum(),
            Some(unwrap_enum(type_map.get_type("Foo")))
        );
    }

    #[test]
    fn get_type_of_root() {
        let mut type_map = TypeMap::with_primitive_types();
        type_map.extend([metatype::Class::new("Foo")]);
        assert_eq!(
            type_map.get_type("int").unwrap(),
            Type::Primitive(PrimitiveType::Int)
        );
        let foo_class = unwrap_class(type_map.get_type("Foo"));
        assert!(foo_class.get_type("int").is_none());
        assert_eq!(
            foo_class.resolve_type("int").unwrap(),
            Type::Primitive(PrimitiveType::Int)
        );
    }

    #[test]
    fn get_type_of_root_scoped() {
        let mut type_map = TypeMap::with_primitive_types();
        let mut foo_meta = metatype::Class::new("Foo");
        foo_meta.enums.push(metatype::Enum::new("Bar"));
        type_map.extend([foo_meta]);

        let foo_class = unwrap_class(type_map.get_type("Foo"));
        assert_eq!(
            type_map.get_type_scoped("Foo").unwrap(),
            type_map.get_type("Foo").unwrap()
        );
        assert_eq!(
            type_map.get_type_scoped("Foo::Bar").unwrap(),
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
        type_map.extend([metatype::Class::new("Foo")]);
        let foo_class = unwrap_class(type_map.get_type("Foo"));
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
        let mut root_meta = metatype::Class::new("Root");
        root_meta.enums.push(metatype::Enum::new("RootEnum"));
        type_map.extend([
            root_meta,
            metatype::Class::with_supers("Sub1", &["Root"]),
            metatype::Class::with_supers("Sub2", &["Sub1"]),
        ]);

        let root_class = unwrap_class(type_map.get_type("Root"));
        let sub2_class = unwrap_class(type_map.get_type("Sub2"));
        assert_eq!(
            sub2_class.get_type("RootEnum").unwrap(),
            root_class.get_type("RootEnum").unwrap()
        );
        assert_eq!(
            type_map.get_type_scoped("Sub2::RootEnum").unwrap(),
            root_class.get_type("RootEnum").unwrap()
        );
    }

    #[test]
    fn get_type_of_property() {
        let mut type_map = TypeMap::with_primitive_types();
        let mut foo_meta = metatype::Class::new("Foo");
        foo_meta
            .properties
            .push(metatype::Property::new("foo_prop", "int"));
        let mut bar_meta = metatype::Class::with_supers("Bar", &["Foo"]);
        bar_meta
            .properties
            .push(metatype::Property::new("bar_prop", "bool"));
        type_map.extend([foo_meta, bar_meta]);

        let bar_class = unwrap_class(type_map.get_type("Bar"));
        assert_eq!(
            bar_class.get_property_type("bar_prop").unwrap(),
            type_map.get_type("bool").unwrap()
        );
        assert_eq!(
            bar_class.get_property_type("foo_prop").unwrap(),
            type_map.get_type("int").unwrap()
        );
    }
}
