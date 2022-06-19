use super::core::TypeSpace;
use super::enum_::Enum;
use super::namespace::NamespaceData;
use super::util::{self, TypeDataRef, TypeMapRef};
use super::{NamedType, ParentSpace, TypeKind};
use crate::metatype;
use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::FusedIterator;
use std::slice;

/// QObject (or gadget) class representation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Class<'a> {
    data: TypeDataRef<'a, ClassData>,
    type_map: TypeMapRef<'a>,
    parent_space: Box<ParentSpace<'a>>,
}

/// Stored class representation.
#[derive(Clone, Debug)]
pub(super) struct ClassData {
    class_name: String,
    public_super_class_names: Vec<String>,
    inner_type_map: NamespaceData,
    property_map: HashMap<String, PropertyData>,
    // TODO
}

impl<'a> Class<'a> {
    pub(super) fn new(
        data: TypeDataRef<'a, ClassData>,
        type_map: TypeMapRef<'a>,
        parent_space: ParentSpace<'a>,
    ) -> Self {
        Class {
            data,
            type_map,
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
        mut f: impl FnMut(&Class<'a>) -> Option<T>,
    ) -> Option<T> {
        if let Some(r) = f(self) {
            Some(r)
        } else {
            self.base_classes().find_map(|cls| f(&cls))
        }
    }

    pub fn is_derived_from(&self, base: &Class) -> bool {
        if self == base {
            true
        } else {
            self.base_classes().any(|c| &c == base)
        }
    }

    pub fn common_base_class(&self, other: &Class<'a>) -> Option<Class<'a>> {
        // quadratic, but the inheritance chain should be short
        self.find_map_self_and_base_classes(|cls| other.is_derived_from(cls).then(|| cls.clone()))
    }

    fn get_type_no_super(&self, name: &str) -> Option<NamedType<'a>> {
        self.data
            .as_ref()
            .inner_type_map
            .get_type_with(name, self.type_map, || ParentSpace::Class(self.clone()))
    }

    fn get_enum_by_variant_no_super(&self, name: &str) -> Option<Enum<'a>> {
        self.data
            .as_ref()
            .inner_type_map
            .get_enum_by_variant_with(name, || ParentSpace::Class(self.clone()))
    }

    /// Looks up type of the specified property.
    pub fn get_property_type(&self, name: &str) -> Option<TypeKind<'a>> {
        self.find_map_self_and_base_classes(|cls| cls.get_property_type_no_super(name))
    }

    fn get_property_type_no_super(&self, name: &str) -> Option<TypeKind<'a>> {
        // TODO: error out if type name can't be resolved?
        self.data
            .as_ref()
            .property_map
            .get(name)
            .and_then(|p| util::decorated_type(&p.type_name, |n| self.resolve_type_scoped(n)))
    }

    /// Whether or not the specified property provides the standard setter function.
    ///
    /// See `PropertyDef::stdCppSet()` in `qtbase/src/tools/moc/moc.h` for details.
    pub fn is_property_std_set(&self, name: &str) -> Option<bool> {
        self.find_map_self_and_base_classes(|cls| cls.is_property_std_set_no_super(name))
    }

    fn is_property_std_set_no_super(&self, name: &str) -> Option<bool> {
        let p = self.data.as_ref().property_map.get(name)?;
        if let (Some(f), Some(h)) = (p.write_func_name.as_ref(), name.chars().next()) {
            // f == set<Name>
            Some(
                f.starts_with("set")
                    && f[3..].starts_with(h.to_ascii_uppercase())
                    && f[(3 + h.len_utf8())..] == name[h.len_utf8()..],
            )
        } else {
            Some(false)
        }
    }
}

impl<'a> TypeSpace<'a> for Class<'a> {
    fn name(&self) -> &str {
        &self.data.as_ref().class_name
    }

    fn get_type(&self, name: &str) -> Option<NamedType<'a>> {
        self.find_map_self_and_base_classes(|cls| cls.get_type_no_super(name))
    }

    fn lexical_parent(&self) -> Option<&ParentSpace<'a>> {
        Some(&self.parent_space)
    }

    fn get_enum_by_variant(&self, name: &str) -> Option<Enum<'a>> {
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
            inner_type_map: NamespaceData::default(),
            property_map: HashMap::new(),
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

    pub(super) fn class_name(&self) -> &str {
        &self.class_name
    }
}

/// Stored property representation.
#[derive(Clone, Debug)]
struct PropertyData {
    type_name: String,
    write_func_name: Option<String>,
}

impl PropertyData {
    fn pair_from_meta(meta: metatype::Property) -> (String, Self) {
        let data = PropertyData {
            type_name: meta.r#type,
            write_func_name: meta.write,
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
    type Item = Class<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        for n in self.names_iter.by_ref() {
            match self.parent_space.resolve_type_scoped(n) {
                Some(NamedType::Class(x)) => return Some(x),
                Some(NamedType::QmlComponent(ns)) => return Some(ns.into_class()),
                Some(NamedType::Enum(_) | NamedType::Namespace(_) | NamedType::Primitive(_))
                | None => (), // TODO: error?
            }
        }
        None
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
    type Item = Class<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(supers) = self.pending.front_mut() {
            for cls in supers.by_ref() {
                if self.visited.insert(cls.clone()) {
                    self.pending.push_back(cls.public_super_classes());
                    return Some(cls);
                }
            }
            self.pending.pop_front();
        }
        None
    }
}

impl<'a> FusedIterator for BaseClasses<'a> {}

#[cfg(test)]
mod tests {
    use super::super::{ModuleData, ModuleId, TypeMap};
    use super::*;
    use crate::metatype;

    fn unwrap_class(ty: Option<NamedType>) -> Class {
        match ty {
            Some(NamedType::Class(x)) => x,
            _ => panic!("unexpected type: {ty:?}"),
        }
    }

    #[test]
    fn base_classes_simple() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo".into());
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

        assert_eq!(root_class.base_classes().collect::<Vec<_>>(), []);
        assert_eq!(
            sub1_class.base_classes().collect::<Vec<_>>(),
            [root_class.clone()]
        );
        assert_eq!(
            sub2_class.base_classes().collect::<Vec<_>>(),
            [sub1_class.clone(), root_class.clone()]
        );
    }

    #[test]
    fn base_classes_multiple() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo".into());
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
            sub2_class.base_classes().collect::<Vec<_>>(),
            [sub1_class.clone(), root2_class.clone(), root1_class.clone()]
        );
    }

    #[test]
    fn base_classes_diamond() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo".into());
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
            leaf_class.base_classes().collect::<Vec<_>>(),
            [mid1_class.clone(), mid2_class.clone(), root_class.clone()]
        );
    }

    #[test]
    fn common_base_class() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo".into());
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
            Class::common_base_class(&mid2_class, &mid3_class).unwrap(),
            root2_class
        );
        assert_eq!(
            Class::common_base_class(&mid3_class, &mid2_class).unwrap(),
            root2_class
        );

        assert_eq!(
            Class::common_base_class(&leaf1_class, &mid1_class).unwrap(),
            mid1_class
        );
        assert_eq!(
            Class::common_base_class(&mid1_class, &leaf1_class).unwrap(),
            mid1_class
        );

        assert_eq!(
            Class::common_base_class(&leaf1_class, &mid2_class).unwrap(),
            mid2_class
        );
        assert_eq!(
            Class::common_base_class(&mid2_class, &leaf1_class).unwrap(),
            mid2_class
        );

        assert_eq!(
            Class::common_base_class(&leaf1_class, &leaf2_class).unwrap(),
            root2_class
        );
        assert_eq!(
            Class::common_base_class(&leaf2_class, &leaf1_class).unwrap(),
            root2_class
        );
    }

    #[test]
    fn property_std_set() {
        let mut type_map = TypeMap::empty();
        let module_id = ModuleId::Named("foo".into());
        let mut module_data = ModuleData::default();
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

        assert_eq!(sub_class.is_property_std_set("rootStd"), Some(true));
        assert_eq!(sub_class.is_property_std_set("rootNoStd"), Some(false));
        assert_eq!(sub_class.is_property_std_set("subStd"), Some(true));
        assert_eq!(sub_class.is_property_std_set("subNoWrite"), Some(false));
        assert!(sub_class.is_property_std_set("unknown").is_none());
    }
}
