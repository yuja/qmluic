use super::class::Class;
use super::core::{TypeMapError, TypeSpace as _};
use super::util::{self, TypeDataRef};
use super::TypeKind;
use crate::metatype;
use std::slice;
use std::vec;

/// Stored method table wrapper to help name-based lookup.
#[derive(Clone, Debug, Default)]
pub(super) struct MethodDataTable {
    methods: Vec<MethodData>,
}

impl MethodDataTable {
    pub(super) fn from_meta<I>(meta_iter: I, access: metatype::AccessSpecifier) -> Self
    where
        I: IntoIterator<Item = (Vec<metatype::Method>, MethodKind)>,
    {
        let mut methods: Vec<_> = meta_iter
            .into_iter()
            .flat_map(|(ms, k)| {
                ms.into_iter()
                    .filter_map(move |m| (m.access == access).then(|| MethodData::from_meta(m, k)))
            })
            .collect();
        methods.sort_by(|a, b| a.name.cmp(&b.name)); // so we can do binary search
        MethodDataTable { methods }
    }

    pub(super) fn get_method_with<'a>(
        &'a self,
        name: &str,
        mut make_object_class: impl FnMut() -> Class<'a>,
    ) -> Option<Result<MethodMatches<'a>, TypeMapError>> {
        let start = self.methods.partition_point(|d| d.name.as_str() < name);
        assert!(start <= self.methods.len());
        let count = self.methods[start..]
            .iter()
            .take_while(|d| d.name == name)
            .count();
        match count {
            0 => None,
            1 => {
                let r = Method::new(TypeDataRef(&self.methods[start]), make_object_class())
                    .map(MethodMatches::Unique);
                Some(r)
            }
            _ => {
                let r = self.methods[start..start + count]
                    .iter()
                    .map(|d| Method::new(TypeDataRef(d), make_object_class()))
                    .collect::<Result<Vec<_>, _>>()
                    .map(MethodMatches::Overloaded);
                Some(r)
            }
        }
    }
}

/// Method representation.
///
/// All type information is resolved when the `Method` object is created, so you don't
/// have to deal with `TypeMapError` later.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Method<'a> {
    data: TypeDataRef<'a, MethodData>,
    return_type: TypeKind<'a>,
    argument_types: Vec<TypeKind<'a>>,
    object_class: Class<'a>,
}

/// Stored method representation.
#[derive(Clone, Debug)]
struct MethodData {
    name: String,
    kind: MethodKind,
    return_type_name: String,
    arguments: Vec<ArgumentData>,
}

/// Nature of the method.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum MethodKind {
    Signal,
    Slot,
    Method,
}

#[derive(Clone, Debug)]
struct ArgumentData {
    name: Option<String>,
    type_name: String,
}

impl<'a> Method<'a> {
    fn new(
        data: TypeDataRef<'a, MethodData>,
        object_class: Class<'a>,
    ) -> Result<Self, TypeMapError> {
        let make_type = |n| util::decorated_type(n, |n| object_class.resolve_type_scoped(n));
        let return_type = make_type(&data.as_ref().return_type_name)?;
        let argument_types = data
            .as_ref()
            .arguments
            .iter()
            .map(|d| make_type(&d.type_name))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Method {
            data,
            return_type,
            argument_types,
            object_class,
        })
    }

    pub fn name(&self) -> &str {
        &self.data.as_ref().name
    }

    pub fn kind(&self) -> MethodKind {
        self.data.as_ref().kind
    }

    /// Type of the return value.
    pub fn return_type(&self) -> &TypeKind<'a> {
        &self.return_type
    }

    /// Type name of the return value.
    pub fn return_type_name(&self) -> &str {
        &self.data.as_ref().return_type_name
    }

    /// Number of the arguments.
    pub fn arguments_len(&self) -> usize {
        self.data.as_ref().arguments.len()
    }

    /// Name of the nth argument if available.
    pub fn argument_name(&self, index: usize) -> Option<&str> {
        self.data.as_ref().arguments[index].name.as_deref()
    }

    /// Type of the nth argument.
    pub fn argument_type(&self, index: usize) -> &TypeKind<'a> {
        &self.argument_types[index]
    }

    /// Type name of the nth argument.
    pub fn argument_type_name(&self, index: usize) -> &str {
        &self.data.as_ref().arguments[index].type_name
    }

    /// Type of arguments in order.
    pub fn argument_types(&self) -> &[TypeKind<'a>] {
        &self.argument_types
    }

    /// Type of the object which this method is associated with.
    pub fn object_class(&self) -> &Class<'a> {
        &self.object_class
    }
}

impl MethodData {
    fn from_meta(meta: metatype::Method, kind: MethodKind) -> Self {
        let arguments = meta
            .arguments
            .into_iter()
            .map(|m| ArgumentData {
                name: m.name,
                type_name: m.r#type,
            })
            .collect();
        MethodData {
            name: meta.name,
            kind,
            return_type_name: meta.return_type,
            arguments,
        }
    }
}

/// Methods found by name.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum MethodMatches<'a> {
    Unique(Method<'a>),
    Overloaded(Vec<Method<'a>>),
}

impl<'a> MethodMatches<'a> {
    /// Returns true if this method isn't overloaded.
    pub fn is_unique(&self) -> bool {
        matches!(self, MethodMatches::Unique(_))
    }

    pub fn as_unique(&self) -> Option<&Method<'a>> {
        match self {
            MethodMatches::Unique(m) => Some(m),
            MethodMatches::Overloaded(_) => None,
        }
    }

    pub fn into_unique(self) -> Option<Method<'a>> {
        match self {
            MethodMatches::Unique(m) => Some(m),
            MethodMatches::Overloaded(_) => None,
        }
    }

    #[allow(clippy::len_without_is_empty)] // should never be empty
    pub fn len(&self) -> usize {
        self.as_slice().len()
    }

    pub fn iter(&self) -> slice::Iter<Method<'a>> {
        self.as_slice().iter()
    }

    pub fn as_slice(&self) -> &[Method<'a>] {
        match self {
            MethodMatches::Unique(m) => slice::from_ref(m),
            MethodMatches::Overloaded(ms) => ms,
        }
    }

    pub fn into_vec(self) -> Vec<Method<'a>> {
        match self {
            MethodMatches::Unique(m) => vec![m],
            MethodMatches::Overloaded(ms) => ms,
        }
    }
}

impl<'a> IntoIterator for MethodMatches<'a> {
    type Item = Method<'a>;
    type IntoIter = vec::IntoIter<Method<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_vec().into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::super::{ModuleData, ModuleId, NamedType, TypeMap};
    use super::*;
    use crate::metatype;

    fn unwrap_class(r: Option<Result<NamedType, TypeMapError>>) -> Class {
        match r {
            Some(Ok(NamedType::Class(x))) => x,
            _ => panic!("unexpected type: {r:?}"),
        }
    }

    #[test]
    fn method_lookup() {
        let mut type_map = TypeMap::with_primitive_types();
        let module_id = ModuleId::Named("foo".into());
        let mut module_data = ModuleData::with_builtins();
        module_data.extend([
            metatype::Class {
                class_name: "Root".to_owned(),
                qualified_class_name: "Root".to_owned(),
                signals: vec![metatype::Method {
                    name: "signal0".to_owned(),
                    access: metatype::AccessSpecifier::Public,
                    return_type: "void".to_owned(),
                    ..Default::default()
                }],
                slots: vec![metatype::Method {
                    name: "slot0".to_owned(),
                    access: metatype::AccessSpecifier::Public,
                    return_type: "void".to_owned(),
                    ..Default::default()
                }],
                methods: vec![
                    metatype::Method {
                        name: "overloaded0".to_owned(),
                        access: metatype::AccessSpecifier::Public,
                        return_type: "void".to_owned(),
                        ..Default::default()
                    },
                    metatype::Method {
                        name: "overloaded0".to_owned(),
                        access: metatype::AccessSpecifier::Public,
                        return_type: "void".to_owned(),
                        arguments: vec![metatype::Argument {
                            name: None,
                            r#type: "int".to_owned(),
                        }],
                        ..Default::default()
                    },
                ],
                ..Default::default()
            },
            metatype::Class::with_supers("Sub", ["Root"]),
        ]);
        type_map.insert_module(module_id.clone(), module_data);

        let module = type_map.get_module(module_id).unwrap();
        let root_class = unwrap_class(module.get_type("Root"));
        let sub_class = unwrap_class(module.get_type("Sub"));

        let m = root_class.get_public_method("signal0").unwrap().unwrap();
        assert!(m.is_unique());
        assert_eq!(m.as_unique().unwrap().name(), "signal0");
        assert_eq!(m.as_unique().unwrap().kind(), MethodKind::Signal);

        let m = sub_class.get_public_method("slot0").unwrap().unwrap();
        assert!(m.is_unique());
        assert_eq!(m.as_unique().unwrap().name(), "slot0");
        assert_eq!(m.as_unique().unwrap().kind(), MethodKind::Slot);

        let m = root_class
            .get_public_method("overloaded0")
            .unwrap()
            .unwrap();
        assert!(!m.is_unique());
        assert_eq!(m.len(), 2);
        assert!(m.iter().all(|m| m.name() == "overloaded0"));
        assert!(m.iter().all(|m| m.kind() == MethodKind::Method));

        assert!(root_class.get_public_method("a").is_none());
        assert!(root_class.get_public_method("z").is_none());
        assert!(sub_class.get_public_method("a").is_none());
        assert!(sub_class.get_public_method("z").is_none());
    }
}
