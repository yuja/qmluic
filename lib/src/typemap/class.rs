use super::core::{TypeMapError, TypeSpace};
use super::enum_::Enum;
use super::function::{Method, MethodDataTable, MethodKind, MethodMatches};
use super::namespace::NamespaceData;
use super::util::{self, TypeDataRef};
use super::{NamedType, ParentSpace, TypeKind};
use crate::metatype;
use crate::qtname;
use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::FusedIterator;
use std::slice;

/// QObject (or gadget) class representation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Class<'a> {
    data: TypeDataRef<'a, ClassData>,
    parent_space: Box<ParentSpace<'a>>,
}

/// Stored class representation.
#[derive(Clone, Debug)]
pub(super) struct ClassData {
    class_name: String,
    public_super_class_names: Vec<String>,
    attached_class_name: Option<String>,
    inner_type_map: NamespaceData,
    property_map: HashMap<String, PropertyData>,
    public_methods: MethodDataTable,
}

impl<'a> Class<'a> {
    pub(super) fn new(data: TypeDataRef<'a, ClassData>, parent_space: ParentSpace<'a>) -> Self {
        Class {
            data,
            parent_space: Box::new(parent_space),
        }
    }

    pub fn public_super_classes(&self) -> SuperClasses<'a> {
        SuperClasses::new(
            self.data.as_ref().public_super_class_names.iter(),
            *self.parent_space.clone(),
        )
    }

    fn base_classes(&self) -> BaseClasses<'a> {
        BaseClasses::new(self.public_super_classes())
    }

    fn find_map_self_and_base_classes<T>(
        &self,
        mut f: impl FnMut(&Class<'a>) -> Option<Result<T, TypeMapError>>,
    ) -> Option<Result<T, TypeMapError>> {
        if let Some(r) = f(self) {
            Some(r)
        } else {
            self.base_classes()
                .find_map(|r| r.and_then(|c| f(&c).transpose()).transpose())
        }
    }

    pub fn is_derived_from(&self, base: &Class) -> bool {
        self.is_derived_from_pedantic(base)
            .and_then(|r| r.ok())
            .is_some()
    }

    fn is_derived_from_pedantic(&self, base: &Class) -> Option<Result<(), TypeMapError>> {
        if self == base {
            Some(Ok(()))
        } else {
            self.base_classes()
                .find_map(|r| r.map(|c| (&c == base).then(|| ())).transpose())
        }
    }

    pub fn common_base_class(&self, other: &Class<'a>) -> Option<Result<Class<'a>, TypeMapError>> {
        // quadratic, but the inheritance chain should be short
        self.find_map_self_and_base_classes(|cls| {
            other
                .is_derived_from_pedantic(cls)
                .map(|r| r.map(|()| cls.clone()))
        })
    }

    pub fn attached_class(&self) -> Option<Result<Class<'a>, TypeMapError>> {
        self.data
            .as_ref()
            .attached_class_name
            .as_ref()
            .map(|n| resolve_class_scoped(&self.parent_space, n))
    }

    fn get_type_no_super(&self, name: &str) -> Option<Result<NamedType<'a>, TypeMapError>> {
        self.data
            .as_ref()
            .inner_type_map
            .get_type_with(name, None, || ParentSpace::Class(self.clone()))
    }

    fn get_enum_by_variant_no_super(&self, name: &str) -> Option<Result<Enum<'a>, TypeMapError>> {
        self.data
            .as_ref()
            .inner_type_map
            .get_enum_by_variant_with(name, || ParentSpace::Class(self.clone()))
    }

    /// Looks up property by name.
    pub fn get_property(&self, name: &str) -> Option<Result<Property<'a>, TypeMapError>> {
        self.find_map_self_and_base_classes(|cls| cls.get_property_no_super(name))
    }

    fn get_property_no_super(&self, name: &str) -> Option<Result<Property<'a>, TypeMapError>> {
        self.data
            .as_ref()
            .property_map
            .get_key_value(name)
            .map(|(n, d)| Property::new(TypeDataRef(n), TypeDataRef(d), self.clone()))
    }

    /// Looks up signals, slots, and methods by name.
    ///
    /// Though signals are conceptually different from slots and methods, they live in
    /// the same name resolution space. Therefore, they are looked up together. Use
    /// [`kind()`](super::Method::kind()) to find the nature of the function.
    pub fn get_public_method(&self, name: &str) -> Option<Result<MethodMatches<'a>, TypeMapError>> {
        // TODO: does it follow the shadowing rule of Qt meta methods?
        self.find_map_self_and_base_classes(|cls| cls.get_public_method_no_super(name))
    }

    fn get_public_method_no_super(
        &self,
        name: &str,
    ) -> Option<Result<MethodMatches<'a>, TypeMapError>> {
        self.data
            .as_ref()
            .public_methods
            .get_method_with(name, || self.clone())
    }
}

impl<'a> TypeSpace<'a> for Class<'a> {
    fn name(&self) -> &str {
        &self.data.as_ref().class_name
    }

    fn get_type(&self, name: &str) -> Option<Result<NamedType<'a>, TypeMapError>> {
        self.find_map_self_and_base_classes(|cls| cls.get_type_no_super(name))
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        Some(&self.parent_space)
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Result<Enum<'a>, TypeMapError>> {
        self.find_map_self_and_base_classes(|cls| cls.get_enum_by_variant_no_super(name))
    }
}

impl ClassData {
    pub(super) fn with_supers<S, I>(name: S, super_names: I) -> Self
    where
        S: Into<String>,
        I: IntoIterator,
        I::Item: Into<String>,
    {
        ClassData {
            class_name: name.into(),
            public_super_class_names: super_names.into_iter().map(Into::into).collect(),
            attached_class_name: None,
            inner_type_map: NamespaceData::default(),
            property_map: HashMap::new(),
            public_methods: MethodDataTable::default(),
        }
    }

    pub(super) fn from_meta(meta: metatype::Class) -> Self {
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

        let attached_class_name = meta
            .class_infos
            .into_iter()
            .find_map(|x| (x.name == "QML.Attached").then(|| x.value));

        let mut inner_type_map = NamespaceData::default();
        inner_type_map.extend_enums(meta.enums);

        let property_map = meta
            .properties
            .into_iter()
            .map(PropertyData::pair_from_meta)
            .collect();

        let public_methods = MethodDataTable::from_meta(
            [
                (meta.signals, MethodKind::Signal),
                (meta.slots, MethodKind::Slot),
                (meta.methods, MethodKind::Method),
            ],
            metatype::AccessSpecifier::Public,
        );

        ClassData {
            class_name: meta.class_name,
            public_super_class_names,
            attached_class_name,
            inner_type_map,
            property_map,
            public_methods,
        }
    }

    pub(super) fn class_name(&self) -> &str {
        &self.class_name
    }
}

/// Property representation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Property<'a> {
    name: TypeDataRef<'a, String>,
    data: TypeDataRef<'a, PropertyData>,
    value_type: TypeKind<'a>,
    object_class: Class<'a>,
}

/// Stored property representation.
#[derive(Clone, Debug)]
struct PropertyData {
    type_name: String,
    read_func_name: Option<String>,
    write_func_name: Option<String>,
    notify_signal_name: Option<String>,
    constant: bool,
}

impl<'a> Property<'a> {
    fn new(
        name: TypeDataRef<'a, String>,
        data: TypeDataRef<'a, PropertyData>,
        object_class: Class<'a>,
    ) -> Result<Self, TypeMapError> {
        let value_type = util::decorated_type(&data.as_ref().type_name, |n| {
            object_class.resolve_type_scoped(n)
        })?;
        Ok(Property {
            name,
            data,
            value_type,
            object_class,
        })
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    /// Type of the property value.
    pub fn value_type(&self) -> &TypeKind<'a> {
        &self.value_type
    }

    /// Type name of the property value.
    pub fn value_type_name(&self) -> &str {
        &self.data.as_ref().type_name
    }

    /// Whether or not this property can be get.
    pub fn is_readable(&self) -> bool {
        self.data.as_ref().read_func_name.is_some()
    }

    /// Whether or not this property can be set.
    pub fn is_writable(&self) -> bool {
        self.data.as_ref().write_func_name.is_some()
    }

    /// Whether or not changes on this property can be notified.
    pub fn is_notifiable(&self) -> bool {
        self.data.as_ref().notify_signal_name.is_some()
    }

    /// Whether or not this property never changes.
    pub fn is_constant(&self) -> bool {
        self.data.as_ref().constant
    }

    /// Whether or not this property provides the standard setter function.
    ///
    /// See `PropertyDef::stdCppSet()` in `qtbase/src/tools/moc/moc.h` for details.
    pub fn is_std_set(&self) -> bool {
        let d = self.data.as_ref();
        qtname::is_std_set_property(self.name.as_ref(), d.write_func_name.as_deref())
    }

    /// Function name to get this property.
    pub fn read_func_name(&self) -> Option<&str> {
        self.data.as_ref().read_func_name.as_deref()
    }

    /// Function name to set this property.
    pub fn write_func_name(&self) -> Option<&str> {
        self.data.as_ref().write_func_name.as_deref()
    }

    /// Signal function to notify changes on this property.
    pub fn notify_signal(&self) -> Option<Result<Method<'a>, TypeMapError>> {
        self.notify_signal_name()
            .map(|n| self.find_notify_signal(n))
    }

    fn find_notify_signal(&self, name: &str) -> Result<Method<'a>, TypeMapError> {
        let matches = self
            .object_class
            .get_public_method(name)
            .unwrap_or_else(|| Err(TypeMapError::InvalidNotifySignal(name.to_owned())))?;
        let mut best: Option<Method<'a>> = None;
        for m in matches
            .into_iter()
            .filter(|m| m.kind() == MethodKind::Signal)
        {
            if best
                .as_ref()
                .map(|k| m.arguments_len() <= k.arguments_len())
                .unwrap_or(false)
            {
                // Prefer function having more arguments. Default parameter is invisible
                // to metatype, but its existence is significant for overload resolution.
                continue;
            }
            if m.arguments_len() == 0 || m.argument_type(0) == self.value_type() {
                best = Some(m);
            }
        }
        best.ok_or_else(|| TypeMapError::InvalidNotifySignal(name.to_owned()))
    }

    /// Signal name to notify changes on this property.
    pub fn notify_signal_name(&self) -> Option<&str> {
        self.data.as_ref().notify_signal_name.as_deref()
    }

    /// Type of the object which this property is associated with.
    pub fn object_class(&self) -> &Class<'a> {
        &self.object_class
    }
}

impl PropertyData {
    fn pair_from_meta(meta: metatype::Property) -> (String, Self) {
        let data = PropertyData {
            type_name: meta.r#type,
            read_func_name: meta.read,
            write_func_name: meta.write,
            notify_signal_name: meta.notify,
            constant: meta.constant,
        };
        (meta.name, data)
    }
}

/// Iterator over the direct super classes.
#[derive(Clone, Debug)]
pub struct SuperClasses<'a> {
    names_iter: slice::Iter<'a, String>,
    parent_space: ParentSpace<'a>,
}

impl<'a> SuperClasses<'a> {
    fn new(names_iter: slice::Iter<'a, String>, parent_space: ParentSpace<'a>) -> Self {
        SuperClasses {
            names_iter,
            parent_space,
        }
    }
}

impl<'a> Iterator for SuperClasses<'a> {
    type Item = Result<Class<'a>, TypeMapError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.names_iter
            .next()
            .map(|n| resolve_class_scoped(&self.parent_space, n))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.names_iter.size_hint()
    }
}

impl<'a> FusedIterator for SuperClasses<'a> where slice::Iter<'a, String>: FusedIterator {}

/// Iterator to walk up the super classes.
///
/// This runs breadth-first search over super classes just because it's easier to implement
/// than DFS. That shouldn't matter since the resolution would be ambiguous if two super classes
/// in different chains had conflicting definitions. Don't rely on the behavior specific to
/// BFS or DFS.
#[derive(Clone, Debug)]
struct BaseClasses<'a> {
    pending: VecDeque<SuperClasses<'a>>,
    visited: HashSet<Class<'a>>,
}

impl<'a> BaseClasses<'a> {
    fn new(supers: SuperClasses<'a>) -> Self {
        BaseClasses {
            pending: VecDeque::from([supers]),
            visited: HashSet::new(),
        }
    }
}

impl<'a> Iterator for BaseClasses<'a> {
    type Item = Result<Class<'a>, TypeMapError>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(supers) = self.pending.front_mut() {
            for r in supers.by_ref() {
                return match r {
                    Ok(c) if self.visited.insert(c.clone()) => {
                        self.pending.push_back(c.public_super_classes());
                        Some(Ok(c))
                    }
                    Ok(_) => continue, // already visited
                    Err(_) => Some(r),
                };
            }
            self.pending.pop_front();
        }
        None
    }
}

impl<'a> FusedIterator for BaseClasses<'a> {}

fn resolve_class_scoped<'a>(
    parent_space: &ParentSpace<'a>,
    scoped_name: &str,
) -> Result<Class<'a>, TypeMapError> {
    let ty = parent_space
        .resolve_type_scoped(scoped_name)
        .unwrap_or_else(|| Err(TypeMapError::InvalidTypeRef(scoped_name.to_owned())))?;
    ty.into_class()
        .ok_or_else(|| TypeMapError::InvalidSuperClassType(scoped_name.to_owned()))
}

#[cfg(test)]
mod tests {
    use super::super::{ModuleData, ModuleId, TypeMap};
    use super::*;
    use crate::metatype;

    fn unwrap_class(r: Option<Result<NamedType, TypeMapError>>) -> Class {
        match r {
            Some(Ok(NamedType::Class(x))) => x,
            _ => panic!("unexpected type: {r:?}"),
        }
    }

    #[test]
    fn base_classes_simple() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo");
        let mut module_data = ModuleData::default();
        module_data.extend([
            metatype::Class::new("Root"),
            metatype::Class::with_supers("Sub1", ["Root"]),
            metatype::Class::with_supers("Sub2", ["Sub1"]),
        ]);
        type_map.insert_module(module_id.clone(), module_data);

        let module = type_map.get_module(module_id).unwrap();
        let root_class = unwrap_class(module.get_type("Root"));
        let sub1_class = unwrap_class(module.get_type("Sub1"));
        let sub2_class = unwrap_class(module.get_type("Sub2"));

        assert_eq!(
            root_class
                .base_classes()
                .collect::<Result<Vec<_>, _>>()
                .unwrap(),
            []
        );
        assert_eq!(
            sub1_class
                .base_classes()
                .collect::<Result<Vec<_>, _>>()
                .unwrap(),
            [root_class.clone()]
        );
        assert_eq!(
            sub2_class
                .base_classes()
                .collect::<Result<Vec<_>, _>>()
                .unwrap(),
            [sub1_class.clone(), root_class.clone()]
        );
    }

    #[test]
    fn base_classes_multiple() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo");
        let mut module_data = ModuleData::default();
        module_data.extend([
            metatype::Class::new("Root1"),
            metatype::Class::new("Root2"),
            metatype::Class::with_supers("Sub1", ["Root1"]),
            metatype::Class::with_supers("Sub2", ["Sub1", "Root2"]),
        ]);
        type_map.insert_module(module_id.clone(), module_data);

        let module = type_map.get_module(module_id).unwrap();
        let root1_class = unwrap_class(module.get_type("Root1"));
        let root2_class = unwrap_class(module.get_type("Root2"));
        let sub1_class = unwrap_class(module.get_type("Sub1"));
        let sub2_class = unwrap_class(module.get_type("Sub2"));

        assert_eq!(
            sub2_class
                .base_classes()
                .collect::<Result<Vec<_>, _>>()
                .unwrap(),
            [sub1_class.clone(), root2_class.clone(), root1_class.clone()]
        );
    }

    #[test]
    fn base_classes_diamond() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo");
        let mut module_data = ModuleData::default();
        module_data.extend([
            metatype::Class::new("Root"),
            metatype::Class::with_supers("Mid1", ["Root"]),
            metatype::Class::with_supers("Mid2", ["Root"]),
            metatype::Class::with_supers("Leaf", ["Mid1", "Mid2"]),
        ]);
        type_map.insert_module(module_id.clone(), module_data);

        let module = type_map.get_module(module_id).unwrap();
        let root_class = unwrap_class(module.get_type("Root"));
        let mid1_class = unwrap_class(module.get_type("Mid1"));
        let mid2_class = unwrap_class(module.get_type("Mid2"));
        let leaf_class = unwrap_class(module.get_type("Leaf"));

        assert_eq!(
            leaf_class
                .base_classes()
                .collect::<Result<Vec<_>, _>>()
                .unwrap(),
            [mid1_class.clone(), mid2_class.clone(), root_class.clone()]
        );
    }

    #[test]
    fn common_base_class() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo");
        let mut module_data = ModuleData::default();
        module_data.extend([
            metatype::Class::new("Root1"),
            metatype::Class::new("Root2"),
            metatype::Class::with_supers("Mid1", ["Root1"]),
            metatype::Class::with_supers("Mid2", ["Root2"]),
            metatype::Class::with_supers("Mid3", ["Root2"]),
            metatype::Class::with_supers("Leaf1", ["Mid1", "Mid2"]),
            metatype::Class::with_supers("Leaf2", ["Mid3"]),
        ]);
        type_map.insert_module(module_id.clone(), module_data);

        // Root1 <--- Mid1 <--- Leaf1
        //                   /
        // Root2 <--- Mid2 <-
        //         \
        //          - Mid3 <--- Leaf2
        let module = type_map.get_module(module_id).unwrap();
        let root1_class = unwrap_class(module.get_type("Root1"));
        let root2_class = unwrap_class(module.get_type("Root2"));
        let mid1_class = unwrap_class(module.get_type("Mid1"));
        let mid2_class = unwrap_class(module.get_type("Mid2"));
        let mid3_class = unwrap_class(module.get_type("Mid3"));
        let leaf1_class = unwrap_class(module.get_type("Leaf1"));
        let leaf2_class = unwrap_class(module.get_type("Leaf2"));

        assert!(Class::common_base_class(&root1_class, &root2_class).is_none());
        assert!(Class::common_base_class(&mid1_class, &mid2_class).is_none());

        assert_eq!(
            Class::common_base_class(&mid2_class, &mid3_class)
                .unwrap()
                .unwrap(),
            root2_class
        );
        assert_eq!(
            Class::common_base_class(&mid3_class, &mid2_class)
                .unwrap()
                .unwrap(),
            root2_class
        );

        assert_eq!(
            Class::common_base_class(&leaf1_class, &mid1_class)
                .unwrap()
                .unwrap(),
            mid1_class
        );
        assert_eq!(
            Class::common_base_class(&mid1_class, &leaf1_class)
                .unwrap()
                .unwrap(),
            mid1_class
        );

        assert_eq!(
            Class::common_base_class(&leaf1_class, &mid2_class)
                .unwrap()
                .unwrap(),
            mid2_class
        );
        assert_eq!(
            Class::common_base_class(&mid2_class, &leaf1_class)
                .unwrap()
                .unwrap(),
            mid2_class
        );

        assert_eq!(
            Class::common_base_class(&leaf1_class, &leaf2_class)
                .unwrap()
                .unwrap(),
            root2_class
        );
        assert_eq!(
            Class::common_base_class(&leaf2_class, &leaf1_class)
                .unwrap()
                .unwrap(),
            root2_class
        );
    }

    #[test]
    fn attached_class() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo");
        let mut module_data = ModuleData::default();
        module_data.extend([
            metatype::Class::new("FooAttached"),
            metatype::Class {
                class_name: "Foo".to_owned(),
                qualified_class_name: "Foo".to_owned(),
                class_infos: vec![metatype::ClassInfo::new("QML.Attached", "FooAttached")],
                ..Default::default()
            },
        ]);
        type_map.insert_module(module_id.clone(), module_data);

        let module = type_map.get_module(module_id).unwrap();
        let foo_class = unwrap_class(module.get_type("Foo"));
        let foo_attached_class = unwrap_class(module.get_type("FooAttached"));

        assert_eq!(
            foo_class.attached_class().unwrap().unwrap(),
            foo_attached_class
        );
    }

    #[test]
    fn property_std_set() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo");
        let mut module_data = ModuleData::with_builtins();
        module_data.extend([
            metatype::Class {
                class_name: "Root".to_owned(),
                qualified_class_name: "Foo".to_owned(),
                properties: vec![
                    metatype::Property {
                        name: "rootStd".to_owned(),
                        r#type: "int".to_owned(),
                        write: Some("setRootStd".to_owned()),
                        ..Default::default()
                    },
                    metatype::Property {
                        name: "rootNoStd".to_owned(),
                        r#type: "int".to_owned(),
                        write: Some("setRootNoStdWriteFunc".to_owned()),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            },
            metatype::Class {
                class_name: "Sub".to_owned(),
                qualified_class_name: "Sub".to_owned(),
                super_classes: vec![metatype::SuperClassSpecifier::public("Root")],
                properties: vec![
                    metatype::Property {
                        name: "subStd".to_owned(),
                        r#type: "int".to_owned(),
                        write: Some("setSubStd".to_owned()),
                        ..Default::default()
                    },
                    metatype::Property {
                        name: "subNoWrite".to_owned(),
                        r#type: "int".to_owned(),
                        write: None,
                        ..Default::default()
                    },
                ],
                ..Default::default()
            },
        ]);
        type_map.insert_module(module_id.clone(), module_data);

        let module = type_map.get_module(module_id).unwrap();
        let sub_class = unwrap_class(module.get_type("Sub"));

        assert_eq!(
            sub_class
                .get_property("rootStd")
                .unwrap()
                .unwrap()
                .is_std_set(),
            true
        );
        assert_eq!(
            sub_class
                .get_property("rootNoStd")
                .unwrap()
                .unwrap()
                .is_std_set(),
            false
        );
        assert_eq!(
            sub_class
                .get_property("subStd")
                .unwrap()
                .unwrap()
                .is_std_set(),
            true
        );
        assert_eq!(
            sub_class
                .get_property("subNoWrite")
                .unwrap()
                .unwrap()
                .is_std_set(),
            false
        );
    }

    #[test]
    fn property_method() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo");
        let mut module_data = ModuleData::with_builtins();
        module_data.extend([metatype::Class {
            class_name: "Foo".to_owned(),
            qualified_class_name: "Foo".to_owned(),
            properties: vec![
                metatype::Property {
                    name: "prop1".to_owned(),
                    r#type: "int".to_owned(),
                    read: Some("prop1".to_owned()),
                    write: Some("setProp1".to_owned()),
                    notify: Some("prop1Changed".to_owned()),
                    ..Default::default()
                },
                metatype::Property {
                    name: "prop2".to_owned(),
                    r#type: "int".to_owned(),
                    read: Some("prop2".to_owned()),
                    write: Some("setProp2".to_owned()),
                    notify: Some("prop2Changed".to_owned()),
                    ..Default::default()
                },
            ],
            signals: vec![
                metatype::Method::nullary("prop1Changed", "void"),
                // prop2Changed(int = default_value), prop2Changed(QString, int)
                metatype::Method::with_argument_types("prop2Changed", "void", ["int"]),
                metatype::Method::nullary("prop2Changed", "void"),
                metatype::Method::with_argument_types("prop2Changed", "void", ["QString", "int"]),
            ],
            ..Default::default()
        }]);
        type_map.insert_module(module_id.clone(), module_data);

        let module = type_map.get_module(module_id).unwrap();
        let foo_class = unwrap_class(module.get_type("Foo"));

        let prop1 = foo_class.get_property("prop1").unwrap().unwrap();
        let prop1_notify = prop1.notify_signal().unwrap().unwrap();
        assert_eq!(prop1_notify.name(), "prop1Changed");
        assert_eq!(prop1_notify.arguments_len(), 0);

        let prop2 = foo_class.get_property("prop2").unwrap().unwrap();
        let prop2_notify = prop2.notify_signal().unwrap().unwrap();
        assert_eq!(prop2_notify.name(), "prop2Changed");
        assert_eq!(prop2_notify.arguments_len(), 1);
        assert_eq!(
            prop2_notify.argument_type(0),
            &TypeKind::Just(module.resolve_type("int").unwrap().unwrap())
        );
    }
}
