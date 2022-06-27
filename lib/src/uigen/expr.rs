use super::context::ObjectContext;
use super::gadget::{Gadget, GadgetKind, ModelItem, PaletteColorGroup};
use super::property::{self, PropertiesMap};
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::color::Color;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{BinaryOperator, Node, UiBindingValue, UnaryOperator};
use crate::typedexpr::{self, DescribeType, ExpressionVisitor, TypeDesc};
use crate::typemap::{Class, Enum, NamedType, PrimitiveType, TypeKind, TypeSpace};
use quick_xml::events::{BytesStart, BytesText, Event};
use std::collections::HashMap;
use std::fmt;
use std::io;
use thiserror::Error;

/// Variant for the property values that can or cannot be serialized to UI XML.
#[derive(Clone, Debug)]
pub(super) enum PropertyValue<'a, 't> {
    Serializable(Value),
    /// List of static QComboBox/QAbstractItemView items.
    ItemModel(Vec<ModelItem>),
    /// List of identifiers referencing the objects.
    ObjectRefList(Vec<String>),
    /// Map of properties assigned to object pointer property.
    ObjectProperties(PropertiesMap<'a, 't>),
}

impl<'a, 't> PropertyValue<'a, 't> {
    /// Generates constant expression of `ty` type from the given `binding_value`.
    pub(super) fn from_binding_value(
        ctx: &ObjectContext,
        ty: &TypeKind<'a>,
        binding_value: &UiBindingValue<'t, '_>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match binding_value {
            UiBindingValue::Node(n) => match ty {
                TypeKind::Just(t) => {
                    parse_as_value_type(ctx, t, *n, diagnostics).map(PropertyValue::Serializable)
                }
                TypeKind::Pointer(NamedType::Class(cls))
                    if cls.is_derived_from(&ctx.classes.abstract_item_model) =>
                {
                    parse_item_model(ctx, *n, diagnostics).map(PropertyValue::ItemModel)
                }
                TypeKind::Pointer(NamedType::Class(cls)) => {
                    parse_object_reference(ctx, cls, *n, diagnostics)
                        .map(|v| PropertyValue::Serializable(Value::Simple(v)))
                }
                TypeKind::PointerList(NamedType::Class(cls)) => {
                    parse_object_reference_list(ctx, cls, *n, diagnostics)
                        .map(PropertyValue::ObjectRefList)
                }
                TypeKind::Pointer(_) | TypeKind::PointerList(_) => {
                    diagnostics.push(Diagnostic::error(
                        n.byte_range(),
                        format!(
                            "unsupported value expression of type '{}'",
                            ty.qualified_cxx_name(),
                        ),
                    ));
                    None
                }
            },
            UiBindingValue::Map(n, m) => match ty {
                TypeKind::Just(NamedType::Class(cls)) => {
                    let properties_map =
                        property::collect_properties_with_node(ctx, cls, m, diagnostics);
                    if let Some(kind) = GadgetKind::from_class(cls) {
                        let v = Gadget::new(kind, properties_map, diagnostics);
                        Some(PropertyValue::Serializable(Value::Gadget(v)))
                    } else if cls.name() == "QPaletteColorGroup" {
                        let v = PaletteColorGroup::new(properties_map, diagnostics);
                        Some(PropertyValue::Serializable(Value::PaletteColorGroup(v)))
                    } else {
                        diagnostics.push(Diagnostic::error(
                            n.byte_range(),
                            format!("unsupported gadget type: {}", cls.qualified_cxx_name()),
                        ));
                        None
                    }
                }
                TypeKind::Pointer(NamedType::Class(cls)) => Some(PropertyValue::ObjectProperties(
                    property::collect_properties_with_node(ctx, cls, m, diagnostics),
                )),
                _ => {
                    diagnostics.push(Diagnostic::error(
                        n.byte_range(),
                        format!(
                            "binding map cannot be parsed as non-class type '{}'",
                            ty.qualified_cxx_name()
                        ),
                    ));
                    None
                }
            },
        }
    }
}

/// Variant for the constant expressions which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub enum Value {
    Simple(SimpleValue),
    /// Generic structured value.
    Gadget(Gadget),
    /// Map of palette color roles created per color group.
    PaletteColorGroup(PaletteColorGroup),
    /// List of strings.
    StringList(Vec<String>, StringKind),
}

impl Value {
    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use Value::*;
        match self {
            Simple(x) => x.serialize_to_xml(writer),
            Gadget(x) => x.serialize_to_xml(writer),
            PaletteColorGroup(x) => x.serialize_to_xml(writer),
            StringList(x, k) => serialize_string_list_to_xml(writer, "stringlist", x, *k),
        }
    }

    pub(super) fn serialize_to_xml_as<W, T>(
        &self,
        writer: &mut XmlWriter<W>,
        tag_name: T,
    ) -> XmlResult<()>
    where
        W: io::Write,
        T: AsRef<[u8]>,
    {
        use Value::*;
        match self {
            Simple(x) => x.serialize_to_xml_as(writer, tag_name),
            Gadget(x) => x.serialize_to_xml_as(writer, tag_name),
            PaletteColorGroup(x) => x.serialize_to_xml_as(writer, tag_name),
            StringList(x, k) => serialize_string_list_to_xml(writer, tag_name, x, *k),
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Simple(x) => x.as_number(),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&str> {
        match self {
            Value::Simple(x) => x.as_enum(),
            _ => None,
        }
    }
}

/// Constant expression which can be serialized to UI XML as a simple tagged value.
#[derive(Clone, Debug)]
pub enum SimpleValue {
    Bool(bool),
    Number(f64),
    String(String, StringKind),
    Cstring(String),
    Enum(String),
    Set(String),
    CursorShape(String),
    Pixmap(String),
}

impl SimpleValue {
    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use SimpleValue::*;
        let tag_name = match self {
            Bool(_) => "bool",
            Number(_) => "number",
            String { .. } => "string",
            Cstring(_) => "cstring",
            Enum(_) => "enum",
            Set(_) => "set",
            CursorShape(_) => "cursorShape",
            Pixmap(_) => "pixmap",
        };
        self.serialize_to_xml_as(writer, tag_name)
    }

    pub(super) fn serialize_to_xml_as<W, T>(
        &self,
        writer: &mut XmlWriter<W>,
        tag_name: T,
    ) -> XmlResult<()>
    where
        W: io::Write,
        T: AsRef<[u8]>,
    {
        use SimpleValue::*;
        match self {
            String(s, k) => {
                let mut tag = BytesStart::borrowed_name(tag_name.as_ref());
                if *k == StringKind::NoTr {
                    tag.push_attribute(("notr", "true"));
                }
                writer.write_event(Event::Start(tag.to_borrowed()))?;
                writer.write_event(Event::Text(BytesText::from_plain_str(s)))?;
                writer.write_event(Event::End(tag.to_end()))
            }
            _ => xmlutil::write_tagged_str(writer, tag_name, self.to_string()),
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            SimpleValue::Number(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&str> {
        match self {
            SimpleValue::Enum(x) | SimpleValue::Set(x) => Some(x),
            _ => None,
        }
    }
}

impl fmt::Display for SimpleValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SimpleValue::*;
        match self {
            Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Number(d) => write!(f, "{}", d),
            String(s, _) | Cstring(s) | Enum(s) | Set(s) | CursorShape(s) | Pixmap(s) => {
                write!(f, "{}", s)
            }
        }
    }
}

/// Marker of bare or translatable string.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StringKind {
    /// Bare string.
    NoTr,
    /// String wrapped with `qsTr()`.
    Tr,
}

fn serialize_string_list_to_xml<W, T>(
    writer: &mut XmlWriter<W>,
    tag_name: T,
    strings: &[String],
    kind: StringKind,
) -> XmlResult<()>
where
    W: io::Write,
    T: AsRef<[u8]>,
{
    let mut tag = BytesStart::borrowed_name(tag_name.as_ref());
    if kind == StringKind::NoTr {
        tag.push_attribute(("notr", "true"));
    }
    writer.write_event(Event::Start(tag.to_borrowed()))?;
    for s in strings {
        xmlutil::write_tagged_str(writer, "string", s)?;
    }
    writer.write_event(Event::End(tag.to_end()))
}

fn parse_as_value_type(
    ctx: &ObjectContext,
    ty: &NamedType,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<Value> {
    match ty {
        NamedType::Class(cls) if cls.is_derived_from(&ctx.classes.brush) => {
            let color = parse_color_value(ctx, node, diagnostics).map(Value::Gadget)?;
            let style = SimpleValue::Enum("Qt::SolidPattern".to_owned());
            Some(Value::Gadget(Gadget {
                kind: GadgetKind::Brush,
                attributes: HashMap::from([("brushstyle".to_owned(), style)]),
                properties: HashMap::from([("color".to_owned(), color)]),
            }))
        }
        NamedType::Class(cls) if cls.is_derived_from(&ctx.classes.color) => {
            parse_color_value(ctx, node, diagnostics).map(Value::Gadget)
        }
        NamedType::Class(cls) if cls.is_derived_from(&ctx.classes.cursor) => {
            let (res_t, res_expr) = format_expression(ctx, node, diagnostics)?;
            match &res_t {
                TypeDesc::Enum(res_en) if is_compatible_enum(res_en, &ctx.classes.cursor_shape) => {
                    Some(Value::Simple(SimpleValue::CursorShape(
                        strip_enum_prefix(&res_expr).to_owned(),
                    )))
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!(
                            "expression type mismatch (expected: {}, actual: {})",
                            ctx.classes.cursor_shape.qualified_cxx_name(),
                            res_t.qualified_name()
                        ),
                    ));
                    None
                }
            }
        }
        NamedType::Class(cls) if cls.is_derived_from(&ctx.classes.key_sequence) => {
            // ExpressionFormatter can handle both enum and string literals, so try it first.
            let (res_t, res_expr) = format_expression(ctx, node, diagnostics)?;
            let standard_key_en = &ctx.classes.key_sequence_standard_key;
            let string_ty = NamedType::Primitive(PrimitiveType::QString);
            match &res_t {
                TypeDesc::Enum(res_en) if is_compatible_enum(res_en, standard_key_en) => {
                    if standard_key_en.is_flag() {
                        Some(Value::Simple(SimpleValue::Set(res_expr)))
                    } else {
                        Some(Value::Simple(SimpleValue::Enum(res_expr)))
                    }
                }
                TypeDesc::String => {
                    // evaluate as string
                    parse_as_value_type(ctx, &string_ty, node, diagnostics)
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!(
                            "expression type mismatch (expected: {} | {}, actual: {})",
                            standard_key_en.qualified_cxx_name(),
                            string_ty.qualified_cxx_name(),
                            res_t.qualified_name()
                        ),
                    ));
                    None
                }
            }
        }
        NamedType::Class(cls) if cls.is_derived_from(&ctx.classes.pixmap) => {
            let res = evaluate_expression(ctx, node, diagnostics)?;
            match res {
                EvaluatedValue::String(s, StringKind::NoTr) => {
                    Some(Value::Simple(SimpleValue::Pixmap(s)))
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!(
                            "evaluated type mismatch (expected: {}, actual: {})",
                            ty.qualified_cxx_name(),
                            res.type_desc().qualified_name()
                        ),
                    ));
                    None
                }
            }
        }
        NamedType::Enum(en) => {
            let (res_t, res_expr) = format_expression(ctx, node, diagnostics)?;
            match &res_t {
                TypeDesc::Enum(res_en) if is_compatible_enum(res_en, en) => {
                    if en.is_flag() {
                        Some(Value::Simple(SimpleValue::Set(res_expr)))
                    } else {
                        Some(Value::Simple(SimpleValue::Enum(res_expr)))
                    }
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!(
                            "expression type mismatch (expected: {}, actual: {})",
                            ty.qualified_cxx_name(),
                            res_t.qualified_name()
                        ),
                    ));
                    None
                }
            }
        }
        NamedType::Primitive(p) => {
            let res = evaluate_expression(ctx, node, diagnostics)?;
            match (p, res) {
                (PrimitiveType::Bool, EvaluatedValue::Bool(v)) => {
                    Some(Value::Simple(SimpleValue::Bool(v)))
                }
                (
                    PrimitiveType::Int | PrimitiveType::QReal | PrimitiveType::UInt,
                    EvaluatedValue::Number(v),
                ) => Some(Value::Simple(SimpleValue::Number(v))),
                (PrimitiveType::QString, EvaluatedValue::String(s, k)) => {
                    Some(Value::Simple(SimpleValue::String(s, k)))
                }
                (PrimitiveType::QStringList, EvaluatedValue::StringList(xs)) => {
                    // unfortunately, notr attribute is list level
                    let kind = xs.first().map(|(_, k)| *k).unwrap_or(StringKind::NoTr);
                    if xs.iter().all(|(_, k)| *k == kind) {
                        let ss = xs.into_iter().map(|(s, _)| s).collect();
                        Some(Value::StringList(ss, kind))
                    } else {
                        diagnostics.push(Diagnostic::error(
                            node.byte_range(),
                            "cannot mix bare and translatable strings",
                        ));
                        None
                    }
                }
                (PrimitiveType::QStringList, EvaluatedValue::EmptyList) => {
                    Some(Value::StringList(vec![], StringKind::NoTr))
                }
                (
                    PrimitiveType::Bool
                    | PrimitiveType::Int
                    | PrimitiveType::QReal
                    | PrimitiveType::QString
                    | PrimitiveType::QStringList
                    | PrimitiveType::UInt
                    | PrimitiveType::Void,
                    res,
                ) => {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!(
                            "evaluated type mismatch (expected: {}, actual: {})",
                            ty.qualified_cxx_name(),
                            res.type_desc().qualified_name()
                        ),
                    ));
                    None
                }
            }
        }
        NamedType::Class(_) | NamedType::QmlComponent(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "unsupported constant value expression: class '{}'",
                    ty.qualified_cxx_name(),
                ),
            ));
            None
        }
        NamedType::Namespace(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "unsupported constant value expression: namespace '{}'",
                    ty.qualified_cxx_name(),
                ),
            ));
            None
        }
    }
}

fn evaluate_expression(
    ctx: &ObjectContext,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<EvaluatedValue> {
    typedexpr::walk(
        ctx.type_space,
        ctx.object_tree,
        node,
        ctx.source,
        &ExpressionEvaluator,
        diagnostics,
    )
}

fn format_expression<'a>(
    ctx: &ObjectContext<'a, '_, '_>,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<(TypeDesc<'a>, String)> {
    typedexpr::walk(
        ctx.type_space,
        ctx.object_tree,
        node,
        ctx.source,
        &ExpressionFormatter,
        diagnostics,
    )
    .map(|(t, expr, _)| (t, expr))
}

fn parse_color_value(
    ctx: &ObjectContext,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<Gadget> {
    // TODO: handle Qt::GlobalColor enum
    let res = evaluate_expression(ctx, node, diagnostics)?;
    match res {
        EvaluatedValue::String(s, StringKind::NoTr) => match s.parse::<Color>() {
            Ok(c) => Some(c.into()),
            Err(e) => {
                diagnostics.push(Diagnostic::error(node.byte_range(), e.to_string()));
                None
            }
        },
        _ => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "evaluated type mismatch (expected: color, actual: {})",
                    res.type_desc().qualified_name()
                ),
            ));
            None
        }
    }
}

/// Parses string list as a static item model.
fn parse_item_model(
    ctx: &ObjectContext,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<Vec<ModelItem>> {
    let res = evaluate_expression(ctx, node, diagnostics)?;
    match res {
        EvaluatedValue::StringList(xs) => {
            let items = xs
                .into_iter()
                .map(|(s, k)| ModelItem::with_text(s, k))
                .collect();
            Some(items)
        }
        EvaluatedValue::EmptyList => Some(vec![]),
        _ => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "evaluated type mismatch (expected: list, actual: {})",
                    res.type_desc().qualified_name()
                ),
            ));
            None
        }
    }
}

fn parse_object_reference(
    ctx: &ObjectContext,
    expected_cls: &Class,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<SimpleValue> {
    let obj_ref = typedexpr::walk(
        ctx.type_space,
        ctx.object_tree,
        node,
        ctx.source,
        &ObjectRefCollector,
        diagnostics,
    )?;
    match obj_ref {
        ObjectRef::Just(res_cls, name) if res_cls.is_derived_from(expected_cls) => {
            Some(SimpleValue::Cstring(name))
        }
        ObjectRef::Just(res_cls, _) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "reference type mismatch (expected: {}, actual: {})",
                    expected_cls.qualified_cxx_name(),
                    res_cls.qualified_cxx_name()
                ),
            ));
            None
        }
        ObjectRef::List(..) | ObjectRef::EmptyList => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "reference type mismatch (expected: {}, actual: list)",
                    expected_cls.qualified_cxx_name(),
                ),
            ));
            None
        }
    }
}

fn parse_object_reference_list(
    ctx: &ObjectContext,
    expected_cls: &Class,
    node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<Vec<String>> {
    let obj_ref = typedexpr::walk(
        ctx.type_space,
        ctx.object_tree,
        node,
        ctx.source,
        &ObjectRefCollector,
        diagnostics,
    )?;
    match obj_ref {
        ObjectRef::Just(res_cls, _) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "reference type mismatch (expected: list, actual: {})",
                    res_cls.qualified_cxx_name(),
                ),
            ));
            None
        }
        ObjectRef::List(res_cls, names) if res_cls.is_derived_from(expected_cls) => Some(names),
        ObjectRef::EmptyList => Some(vec![]),
        ObjectRef::List(res_cls, _) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "element type mismatch (expected: {}, actual: {})",
                    expected_cls.qualified_cxx_name(),
                    res_cls.qualified_cxx_name()
                ),
            ));
            None
        }
    }
}

#[derive(Clone, Debug, Error)]
enum ExpressionError {
    #[error("unsupported literal: {0}")]
    UnsupportedLiteral(&'static str),
    #[error("unsupported reference")]
    UnsupportedReference,
    #[error("unsupported function call")]
    UnsupportedFunctionCall,
    #[error("unsupported unary operation on type: {0} <{1}>")]
    UnsupportedUnaryOperationOnType(UnaryOperator, String),
    #[error("unsupported binary operation on types: <{1}> {0} <{2}>")]
    UnsupportedBinaryOperationOnType(BinaryOperator, String, String),
    #[error("cannot evaluate as constant")]
    CannotEvaluateAsConstant,
    #[error("cannot deduce type from '{0}' and '{1}'")]
    CannotDeduceType(String, String),
}

/// Evaluates expression tree as arbitrary constant value expression.
///
/// Here we don't follow the JavaScript language model, but try to be stricter.
#[derive(Debug)]
struct ExpressionEvaluator;

#[derive(Clone, Debug)]
enum EvaluatedValue {
    Bool(bool),
    Number(f64),
    String(String, StringKind),
    StringList(Vec<(String, StringKind)>),
    EmptyList,
}

impl DescribeType<'_> for EvaluatedValue {
    fn type_desc(&self) -> TypeDesc<'static> {
        match self {
            EvaluatedValue::Bool(_) => TypeDesc::Bool,
            EvaluatedValue::Number(_) => TypeDesc::Number,
            EvaluatedValue::String(..) => TypeDesc::String,
            EvaluatedValue::StringList(_) => TypeDesc::StringList,
            EvaluatedValue::EmptyList => TypeDesc::EmptyList,
        }
    }
}

impl<'a> ExpressionVisitor<'a> for ExpressionEvaluator {
    type Item = EvaluatedValue;
    type Error = ExpressionError;

    fn visit_number(&self, value: f64) -> Result<Self::Item, Self::Error> {
        Ok(EvaluatedValue::Number(value))
    }

    fn visit_string(&self, value: String) -> Result<Self::Item, Self::Error> {
        Ok(EvaluatedValue::String(value, StringKind::NoTr))
    }

    fn visit_bool(&self, value: bool) -> Result<Self::Item, Self::Error> {
        Ok(EvaluatedValue::Bool(value))
    }

    fn visit_enum(&self, _enum_ty: Enum<'a>, _variant: &str) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("enum")) // enum value is unknown
    }

    fn visit_array(&self, elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error> {
        match elements.first() {
            Some(EvaluatedValue::String(..)) => elements
                .into_iter()
                .map(|x| match x {
                    EvaluatedValue::String(s, k) => Ok((s, k)),
                    _ => Err(ExpressionError::CannotDeduceType(
                        "string".to_owned(),
                        x.type_desc().qualified_name().into(),
                    )),
                })
                .collect::<Result<_, _>>()
                .map(EvaluatedValue::StringList),
            Some(_) => Err(ExpressionError::UnsupportedLiteral("non-string array")),
            None => Ok(EvaluatedValue::EmptyList),
        }
    }

    fn visit_object_ref(&self, _cls: Class<'a>, _name: &str) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedReference)
    }

    fn visit_call_expression(
        &self,
        function: &str,
        mut arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        match function {
            "qsTr" if arguments.len() == 1 => match arguments.pop() {
                Some(EvaluatedValue::String(a, StringKind::NoTr)) => {
                    Ok(EvaluatedValue::String(a, StringKind::Tr))
                }
                _ => Err(ExpressionError::UnsupportedFunctionCall),
            },
            _ => Err(ExpressionError::UnsupportedFunctionCall),
        }
    }

    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        argument: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        use UnaryOperator::*;
        let type_error = {
            let arg_t = argument.type_desc();
            move || {
                ExpressionError::UnsupportedUnaryOperationOnType(
                    operator,
                    arg_t.qualified_name().into(),
                )
            }
        };
        match argument {
            EvaluatedValue::Bool(a) => match operator {
                LogicalNot => Ok(EvaluatedValue::Bool(!a)),
                BitwiseNot => Err(type_error()),
                Minus | Plus => Err(type_error()),
                Typeof | Void | Delete => Err(type_error()),
            },
            EvaluatedValue::Number(a) => match operator {
                // TODO: handle overflow, etc.
                LogicalNot => Err(type_error()), // TODO: !
                BitwiseNot => Err(type_error()), // TODO: ~
                Minus => Ok(EvaluatedValue::Number(-a)),
                Plus => Ok(EvaluatedValue::Number(a)),
                Typeof | Void | Delete => Err(type_error()),
            },
            EvaluatedValue::String(_, StringKind::NoTr) => Err(type_error()),
            EvaluatedValue::String(_, StringKind::Tr) => {
                Err(ExpressionError::CannotEvaluateAsConstant)
            }
            EvaluatedValue::StringList(_) | EvaluatedValue::EmptyList => Err(type_error()),
        }
    }

    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        use BinaryOperator::*;
        let type_error = {
            let left_t = left.type_desc();
            let right_t = right.type_desc();
            move || {
                ExpressionError::UnsupportedBinaryOperationOnType(
                    operator,
                    left_t.qualified_name().into(),
                    right_t.qualified_name().into(),
                )
            }
        };
        match (left, right) {
            #[allow(clippy::bool_comparison)]
            (EvaluatedValue::Bool(l), EvaluatedValue::Bool(r)) => match operator {
                LogicalAnd => Ok(EvaluatedValue::Bool(l && r)),
                LogicalOr => Ok(EvaluatedValue::Bool(l || r)),
                RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                BitwiseAnd => Ok(EvaluatedValue::Bool(l & r)),
                BitwiseXor => Ok(EvaluatedValue::Bool(l ^ r)),
                BitwiseOr => Ok(EvaluatedValue::Bool(l | r)),
                Add | Sub | Mul | Div | Rem | Exp => Err(type_error()),
                Equal => Ok(EvaluatedValue::Bool(l == r)),
                StrictEqual => Ok(EvaluatedValue::Bool(l == r)),
                NotEqual => Ok(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Ok(EvaluatedValue::Bool(l != r)),
                LessThan => Ok(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Ok(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Ok(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Ok(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce | Instanceof | In => Err(type_error()),
            },
            (EvaluatedValue::Number(l), EvaluatedValue::Number(r)) => match operator {
                // TODO: handle overflow, etc.
                LogicalAnd | LogicalOr => Err(type_error()),
                // TODO: >>, (unsigned)>>, <<
                RightShift => Err(type_error()),
                UnsignedRightShift => Err(type_error()),
                LeftShift => Err(type_error()),
                // TODO: &, ^, |
                BitwiseAnd => Err(type_error()),
                BitwiseXor => Err(type_error()),
                BitwiseOr => Err(type_error()),
                Add => Ok(EvaluatedValue::Number(l + r)),
                Sub => Ok(EvaluatedValue::Number(l - r)),
                Mul => Ok(EvaluatedValue::Number(l * r)),
                Div => Ok(EvaluatedValue::Number(l / r)),
                Rem => Ok(EvaluatedValue::Number(l % r)),
                Exp => Ok(EvaluatedValue::Number(l.powf(r))),
                Equal => Ok(EvaluatedValue::Bool(l == r)),
                StrictEqual => Ok(EvaluatedValue::Bool(l == r)),
                NotEqual => Ok(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Ok(EvaluatedValue::Bool(l != r)),
                LessThan => Ok(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Ok(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Ok(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Ok(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce => Err(type_error()),
                Instanceof => Err(type_error()),
                In => Err(type_error()),
            },
            (
                EvaluatedValue::String(l, StringKind::NoTr),
                EvaluatedValue::String(r, StringKind::NoTr),
            ) => match operator {
                LogicalAnd | LogicalOr => Err(type_error()),
                RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                BitwiseAnd | BitwiseXor | BitwiseOr => Err(type_error()),
                Add => Ok(EvaluatedValue::String(l + &r, StringKind::NoTr)),
                Sub | Mul | Div | Rem | Exp => Err(type_error()),
                Equal => Ok(EvaluatedValue::Bool(l == r)),
                StrictEqual => Ok(EvaluatedValue::Bool(l == r)),
                NotEqual => Ok(EvaluatedValue::Bool(l != r)),
                StrictNotEqual => Ok(EvaluatedValue::Bool(l != r)),
                LessThan => Ok(EvaluatedValue::Bool(l < r)),
                LessThanEqual => Ok(EvaluatedValue::Bool(l <= r)),
                GreaterThan => Ok(EvaluatedValue::Bool(l > r)),
                GreaterThanEqual => Ok(EvaluatedValue::Bool(l >= r)),
                NullishCoalesce => Err(type_error()),
                Instanceof => Err(type_error()),
                In => Err(type_error()),
            },
            (EvaluatedValue::String(_, StringKind::Tr), _)
            | (_, EvaluatedValue::String(_, StringKind::Tr)) => {
                Err(ExpressionError::CannotEvaluateAsConstant)
            }
            _ => Err(type_error()),
        }
    }
}

/// Formats expression tree as arbitrary constant value expression.
///
/// Here we don't strictly follow the JavaScript language model, but try 1:1 mapping.
#[derive(Debug)]
struct ExpressionFormatter;

impl<'a> DescribeType<'a> for (TypeDesc<'a>, String, u32) {
    fn type_desc(&self) -> TypeDesc<'a> {
        self.0.clone()
    }
}

impl<'a> ExpressionVisitor<'a> for ExpressionFormatter {
    type Item = (TypeDesc<'a>, String, u32);
    type Error = ExpressionError;

    fn visit_number(&self, value: f64) -> Result<Self::Item, Self::Error> {
        Ok((TypeDesc::Number, value.to_string(), PREC_TERM))
    }

    fn visit_string(&self, value: String) -> Result<Self::Item, Self::Error> {
        Ok((TypeDesc::String, format!("{:?}", value), PREC_TERM)) // TODO: escape per C spec)
    }

    fn visit_bool(&self, value: bool) -> Result<Self::Item, Self::Error> {
        Ok((
            TypeDesc::Bool,
            if value {
                "true".to_owned()
            } else {
                "false".to_owned()
            },
            PREC_TERM,
        ))
    }

    fn visit_enum(&self, enum_ty: Enum<'a>, variant: &str) -> Result<Self::Item, Self::Error> {
        let res_expr = enum_ty.qualify_cxx_variant_name(variant);
        Ok((TypeDesc::Enum(enum_ty), res_expr, PREC_SCOPE))
    }

    fn visit_array(&self, _elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error> {
        // TODO: handle string/object-ref list
        Err(ExpressionError::UnsupportedLiteral("array"))
    }

    fn visit_object_ref(&self, cls: Class<'a>, name: &str) -> Result<Self::Item, Self::Error> {
        Ok((TypeDesc::ObjectRef(cls), name.to_owned(), PREC_TERM))
    }

    fn visit_call_expression(
        &self,
        function: &str,
        arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        match function {
            "qsTr" if arguments.len() == 1 => {
                if let (TypeDesc::String, expr, _prec) = &arguments[0] {
                    Ok((TypeDesc::String, format!("qsTr({})", expr), PREC_CALL))
                } else {
                    Err(ExpressionError::UnsupportedFunctionCall)
                }
            }
            _ => Err(ExpressionError::UnsupportedFunctionCall),
        }
    }

    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        (arg_t, arg_expr, arg_prec): Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        use UnaryOperator::*;
        let type_error = {
            let arg_t = arg_t.clone();
            move || {
                ExpressionError::UnsupportedUnaryOperationOnType(
                    operator,
                    arg_t.qualified_name().into(),
                )
            }
        };
        let res_prec = unary_operator_precedence(operator);
        let res_expr = [
            unary_operator_str(operator),
            &maybe_paren(res_prec, arg_expr, arg_prec),
        ]
        .concat();
        match arg_t {
            TypeDesc::Bool => match operator {
                LogicalNot => Ok((TypeDesc::Bool, res_expr, res_prec)),
                BitwiseNot => Err(type_error()),
                Minus | Plus => Err(type_error()),
                Typeof | Void | Delete => Err(type_error()),
            },
            TypeDesc::Number => match operator {
                LogicalNot => Ok((TypeDesc::Bool, res_expr, res_prec)),
                BitwiseNot | Minus | Plus => Ok((TypeDesc::Number, res_expr, res_prec)),
                Typeof | Void | Delete => Err(type_error()),
            },
            TypeDesc::String => Err(type_error()),
            TypeDesc::Enum(en) => match operator {
                LogicalNot => Ok((TypeDesc::Bool, res_expr, res_prec)),
                BitwiseNot => Ok((TypeDesc::Enum(en), res_expr, res_prec)),
                Minus | Plus => Err(type_error()),
                Typeof | Void | Delete => Err(type_error()),
            },
            TypeDesc::ObjectRef(_)
            | TypeDesc::ObjectRefList(_)
            | TypeDesc::StringList
            | TypeDesc::EmptyList => Err(type_error()),
        }
    }

    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        (left_t, left_expr, left_prec): Self::Item,
        (right_t, right_expr, right_prec): Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        use BinaryOperator::*;
        let type_error = {
            let left_t = left_t.clone();
            let right_t = right_t.clone();
            move || {
                ExpressionError::UnsupportedBinaryOperationOnType(
                    operator,
                    left_t.qualified_name().into(),
                    right_t.qualified_name().into(),
                )
            }
        };
        let res_prec = binary_operator_precedence(operator);
        match (left_t, right_t) {
            (TypeDesc::Bool, TypeDesc::Bool) => {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                    BitwiseAnd | BitwiseXor | BitwiseOr => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    Add | Sub | Mul | Div | Rem | Exp => Err(type_error()),
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (TypeDesc::Number, TypeDesc::Number) => {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Err(type_error()),
                    // TODO: >>, (unsigned)>>, <<
                    RightShift => Err(type_error()),
                    UnsignedRightShift => Err(type_error()),
                    LeftShift => Err(type_error()),
                    // TODO: &, ^, |
                    BitwiseAnd => Err(type_error()),
                    BitwiseXor => Err(type_error()),
                    BitwiseOr => Err(type_error()),
                    Add | Sub | Mul | Div | Rem => Ok((TypeDesc::Number, res_expr, res_prec)),
                    Exp => Err(type_error()), // TODO
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (TypeDesc::String, TypeDesc::String) => {
                let (left_expr, left_prec) = (format!("QString({})", left_expr), PREC_CALL);
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Err(type_error()),
                    RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                    BitwiseAnd | BitwiseXor | BitwiseOr => Err(type_error()),
                    Add => Ok((TypeDesc::String, res_expr, res_prec)),
                    Sub | Mul | Div | Rem | Exp => Err(type_error()),
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::Bool, res_expr, res_prec)),

                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (TypeDesc::Enum(left_en), TypeDesc::Enum(right_en))
                if is_compatible_enum(&left_en, &right_en) =>
            {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                    BitwiseAnd | BitwiseXor | BitwiseOr => {
                        Ok((TypeDesc::Enum(left_en), res_expr, res_prec))
                    }
                    Add | Sub | Mul | Div | Rem | Exp => Err(type_error()),
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::Bool, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            _ => Err(type_error()),
        }
    }
}

fn is_compatible_enum(left_en: &Enum, right_en: &Enum) -> bool {
    left_en == right_en
        || left_en.alias_enum().map_or(false, |en| &en == right_en)
        || right_en.alias_enum().map_or(false, |en| &en == left_en)
}

pub(super) fn strip_enum_prefix(s: &str) -> &str {
    s.split_once("::").map(|(_, t)| t).unwrap_or(s)
}

fn maybe_paren(res_prec: u32, arg_expr: String, arg_prec: u32) -> String {
    if res_prec >= arg_prec {
        arg_expr
    } else {
        format!("({})", arg_expr)
    }
}

// 1-17: https://en.cppreference.com/w/cpp/language/operator_precedence
const PREC_TERM: u32 = 0;
const PREC_SCOPE: u32 = 1;
const PREC_CALL: u32 = 2;

fn unary_operator_precedence(operator: UnaryOperator) -> u32 {
    use UnaryOperator::*;
    match operator {
        LogicalNot => 3,
        BitwiseNot => 3,
        Minus => 3,
        Plus => 3,
        Typeof | Void | Delete => u32::MAX, // unsupported
    }
}

fn binary_operator_precedence(operator: BinaryOperator) -> u32 {
    use BinaryOperator::*;
    match operator {
        LogicalAnd => 14,
        LogicalOr => 15,
        RightShift | UnsignedRightShift => 7,
        LeftShift => 7,
        BitwiseAnd => 11,
        BitwiseXor => 12,
        BitwiseOr => 13,
        Add => 6,
        Sub => 6,
        Mul => 5,
        Div => 5,
        Rem => 5,
        Exp => u32::MAX, // unsupported
        Equal | StrictEqual => 10,
        NotEqual | StrictNotEqual => 10,
        LessThan => 9,
        LessThanEqual => 9,
        GreaterThan => 9,
        GreaterThanEqual => 9,
        NullishCoalesce | Instanceof | In => u32::MAX, // unsupported
    }
}

fn unary_operator_str(operator: UnaryOperator) -> &'static str {
    use UnaryOperator::*;
    match operator {
        LogicalNot => "!",
        BitwiseNot => "~",
        Minus => "-",
        Plus => "+",
        Typeof | Void | Delete => "/* unsupported */",
    }
}

fn binary_operator_str(operator: BinaryOperator) -> &'static str {
    use BinaryOperator::*;
    match operator {
        LogicalAnd => "&&",
        LogicalOr => "||",
        RightShift | UnsignedRightShift => ">>",
        LeftShift => "<<",
        BitwiseAnd => "&",
        BitwiseXor => "^",
        BitwiseOr => "|",
        Add => "+",
        Sub => "-",
        Mul => "*",
        Div => "/",
        Rem => "%",
        Exp => "/* unsupported */",
        Equal | StrictEqual => "==",
        NotEqual | StrictNotEqual => "!=",
        LessThan => "<",
        LessThanEqual => "<=",
        GreaterThan => ">",
        GreaterThanEqual => ">=",
        NullishCoalesce | Instanceof | In => "/* unsupported */",
    }
}

/// Collects object references from identifier or array expression.
#[derive(Debug)]
struct ObjectRefCollector;

#[derive(Clone, Debug)]
enum ObjectRef<'a> {
    Just(Class<'a>, String),
    List(Class<'a>, Vec<String>),
    EmptyList,
}

impl<'a> DescribeType<'a> for ObjectRef<'a> {
    fn type_desc(&self) -> TypeDesc<'a> {
        match self {
            ObjectRef::Just(cls, _) => TypeDesc::ObjectRef(cls.clone()),
            ObjectRef::List(cls, _) => TypeDesc::ObjectRefList(cls.clone()),
            ObjectRef::EmptyList => TypeDesc::EmptyList,
        }
    }
}

impl<'a> ExpressionVisitor<'a> for ObjectRefCollector {
    type Item = ObjectRef<'a>;
    type Error = ExpressionError;

    fn visit_number(&self, _value: f64) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("number"))
    }

    fn visit_string(&self, _value: String) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("string"))
    }

    fn visit_bool(&self, _value: bool) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("bool"))
    }

    fn visit_enum(&self, _enum_ty: Enum<'a>, _variant: &str) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("enum"))
    }

    fn visit_array(&self, elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error> {
        let mut base_cls: Option<Class> = None;
        let mut names = Vec::with_capacity(elements.len());
        for r in elements {
            match r {
                ObjectRef::Just(cls, s) => {
                    base_cls = match base_cls {
                        Some(base) if base != cls => {
                            Some(base.common_base_class(&cls).ok_or_else(|| {
                                ExpressionError::CannotDeduceType(
                                    base.qualified_cxx_name().into(),
                                    cls.qualified_cxx_name().into(),
                                )
                            })?)
                        }
                        Some(_) => base_cls,
                        None => Some(cls),
                    };
                    names.push(s);
                }
                ObjectRef::List(..) | ObjectRef::EmptyList => {
                    return Err(ExpressionError::UnsupportedLiteral("nested array"));
                }
            }
        }

        if let Some(base) = base_cls {
            Ok(ObjectRef::List(base, names))
        } else {
            Ok(ObjectRef::EmptyList)
        }
    }

    fn visit_object_ref(&self, cls: Class<'a>, name: &str) -> Result<Self::Item, Self::Error> {
        Ok(ObjectRef::Just(cls, name.to_owned()))
    }

    fn visit_call_expression(
        &self,
        _function: &str,
        _arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedFunctionCall)
    }

    fn visit_unary_expression(
        &self,
        operator: UnaryOperator,
        argument: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedUnaryOperationOnType(
            operator,
            argument.type_desc().qualified_name().into(),
        ))
    }

    fn visit_binary_expression(
        &self,
        operator: BinaryOperator,
        left: Self::Item,
        right: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedBinaryOperationOnType(
            operator,
            left.type_desc().qualified_name().into(),
            right.type_desc().qualified_name().into(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Diagnostics;
    use crate::metatype;
    use crate::objtree::ObjectTree;
    use crate::qmlast::{UiObjectDefinition, UiProgram};
    use crate::qmldoc::UiDocument;
    use crate::typemap::{ModuleData, ModuleId, TypeMap};

    struct Env {
        doc: UiDocument,
        type_map: TypeMap,
        module_id: ModuleId<'static>,
    }

    impl Env {
        fn new(expr_source: &str) -> Self {
            let mut type_map = TypeMap::with_primitive_types();
            let module_id = ModuleId::Named("foo".into());
            type_map.insert_module(module_id.clone(), ModuleData::with_builtins());
            let mut foo_meta = metatype::Class::new("Foo");
            foo_meta
                .enums
                .push(metatype::Enum::with_values("Bar", ["Bar0", "Bar1", "Bar2"]));
            type_map
                .get_module_data_mut(&module_id)
                .unwrap()
                .extend([foo_meta, metatype::Class::new("A")]);
            Env {
                doc: UiDocument::parse(format!("A {{ a: {expr_source}}}"), None),
                type_map,
                module_id,
            }
        }

        fn root_expr_nodes(&self) -> (Node, Node) {
            let program = UiProgram::from_node(self.doc.root_node(), self.doc.source()).unwrap();
            let obj = UiObjectDefinition::from_node(program.root_object_node(), self.doc.source())
                .unwrap();
            let map = obj.build_binding_map(self.doc.source()).unwrap();
            (
                program.root_object_node(),
                map.get("a").unwrap().get_node().unwrap(),
            )
        }

        fn format(&self) -> (TypeDesc, String, u32) {
            self.try_format().unwrap()
        }

        fn try_format(&self) -> Result<(TypeDesc, String, u32), Diagnostics> {
            let mut diagnostics = Diagnostics::new();
            let type_space = self.type_map.get_module(&self.module_id).unwrap();
            let (root_node, node) = self.root_expr_nodes();
            let object_tree =
                ObjectTree::build(root_node, self.doc.source(), &type_space, &mut diagnostics)
                    .unwrap();
            typedexpr::walk(
                &type_space,
                &object_tree,
                node,
                self.doc.source(),
                &ExpressionFormatter,
                &mut diagnostics,
            )
            .ok_or(diagnostics)
        }
    }

    fn format_expr(expr_source: &str) -> String {
        let (_, expr, _) = Env::new(expr_source).format();
        expr
    }

    #[test]
    fn format_flags() {
        assert_eq!(format_expr("Foo.Bar0"), "Foo::Bar0");
        assert_eq!(
            format_expr("Foo.Bar0 | Foo.Bar1 | Foo.Bar2"),
            "Foo::Bar0|Foo::Bar1|Foo::Bar2"
        );
        assert_eq!(format_expr("Foo.Bar0 & ~Foo.Bar1"), "Foo::Bar0&~Foo::Bar1");
    }

    #[test]
    fn format_operator_precedence() {
        assert_eq!(format_expr("1 + (2 * 3) + 4"), "1+2*3+4");
        assert_eq!(format_expr("((1 + 2) * 3) + 4"), "(1+2)*3+4");
        assert_eq!(format_expr("1 + (2 * (3 + 4))"), "1+2*(3+4)");
        assert_eq!(format_expr("+(-1)"), "+-1");
        assert_eq!(format_expr("+((-1) + 1)"), "+(-1+1)");
    }

    #[test]
    fn format_operator_str() {
        assert_eq!(format_expr("1 === 1"), "1==1");
        assert_eq!(format_expr("1 !== 2"), "1!=2");
        assert_eq!(format_expr("'foo' + 'bar'"), r#"QString("foo")+"bar""#);
    }
}
