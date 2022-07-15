use super::context::ObjectContext;
use super::gadget::{Gadget, GadgetKind, ModelItem, PaletteColorGroup};
use super::interpret::{self, EvaluatedValue};
use super::objcode::{PropertyCode, PropertyCodeKind};
use super::property::{self, PropertiesMap};
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::color::Color;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::Node;
use crate::tir;
use crate::typemap::{NamedType, PrimitiveType, TypeKind, TypeSpace};
use quick_xml::events::{BytesStart, BytesText, Event};
use std::collections::HashMap;
use std::fmt;
use std::io;

/// Variant for the property values that can or cannot be serialized to UI XML.
#[derive(Clone, Debug)]
pub(super) enum PropertyValue<'a, 't> {
    Serializable(Value),
    /// Value expression to be evaluated at run time.
    Dynamic(tir::CodeBody<'a>),
    /// List of static QComboBox/QAbstractItemView items.
    ItemModel(Vec<ModelItem>),
    /// List of identifiers referencing the objects.
    ObjectRefList(Vec<String>),
    /// Map of properties assigned to object pointer property.
    ObjectProperties(PropertiesMap<'a, 't>),
}

impl<'a, 't> PropertyValue<'a, 't> {
    pub(super) fn build(
        ctx: &ObjectContext<'a, '_, '_>,
        property_code: &PropertyCode<'a, 't, '_>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let node = property_code.node();
        match property_code.kind() {
            PropertyCodeKind::Expr(ty, code) => {
                if let Some(res) = interpret::evaluate_code(code) {
                    // constant expression can be mapped to .ui value type
                    match ty {
                        TypeKind::Just(t) => {
                            parse_as_value_type(ctx, ty, t, node, code, res, diagnostics)
                                .map(PropertyValue::Serializable)
                        }
                        TypeKind::Pointer(NamedType::Class(cls))
                            if cls.is_derived_from(&ctx.classes.abstract_item_model) =>
                        {
                            verify_code_return_type(
                                node,
                                code,
                                &TypeKind::STRING_LIST,
                                diagnostics,
                            )?;
                            let items = res
                                .unwrap_string_list()
                                .into_iter()
                                .map(|(s, k)| ModelItem::with_text(s, k))
                                .collect();
                            Some(PropertyValue::ItemModel(items))
                        }
                        TypeKind::Pointer(NamedType::Class(_)) => {
                            verify_code_return_type(node, code, ty, diagnostics)?;
                            Some(PropertyValue::Serializable(Value::Simple(
                                SimpleValue::Cstring(res.unwrap_object_ref()),
                            )))
                        }
                        TypeKind::PointerList(NamedType::Class(_)) => {
                            verify_code_return_type(node, code, ty, diagnostics)?;
                            Some(PropertyValue::ObjectRefList(res.unwrap_object_ref_list()))
                        }
                        TypeKind::Pointer(_) | TypeKind::PointerList(_) => {
                            diagnostics.push(Diagnostic::error(
                                node.byte_range(),
                                format!(
                                    "unsupported value expression of type '{}'",
                                    ty.qualified_cxx_name(),
                                ),
                            ));
                            None
                        }
                    }
                } else {
                    verify_code_return_type(node, code, ty, diagnostics)?;
                    Some(PropertyValue::Dynamic(code.clone()))
                }
            }
            PropertyCodeKind::GadgetMap(cls, map) => {
                let properties_map = property::make_properties_from_code_map(ctx, map, diagnostics);
                if let Some(kind) = GadgetKind::from_class(cls, ctx.classes) {
                    let v = Gadget::new(kind, properties_map, diagnostics);
                    Some(PropertyValue::Serializable(Value::Gadget(v)))
                } else if cls.name() == "QPaletteColorGroup" {
                    let v = PaletteColorGroup::new(properties_map, diagnostics);
                    Some(PropertyValue::Serializable(Value::PaletteColorGroup(v)))
                } else {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!("unsupported gadget type: {}", cls.qualified_cxx_name()),
                    ));
                    None
                }
            }
            PropertyCodeKind::ObjectMap(_, map) => {
                let properties_map = property::make_properties_from_code_map(ctx, map, diagnostics);
                Some(PropertyValue::ObjectProperties(properties_map))
            }
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
    expected_ty: &TypeKind,
    expected_just_ty: &NamedType,
    node: Node,
    code: &tir::CodeBody,
    res: EvaluatedValue,
    diagnostics: &mut Diagnostics,
) -> Option<Value> {
    match expected_just_ty {
        NamedType::Class(cls) if cls == &ctx.classes.brush => {
            let color = parse_color_value(node, code, res, diagnostics).map(Value::Gadget)?;
            let style = SimpleValue::Enum("Qt::SolidPattern".to_owned());
            Some(Value::Gadget(Gadget {
                kind: GadgetKind::Brush,
                attributes: HashMap::from([("brushstyle".to_owned(), style)]),
                properties: HashMap::from([("color".to_owned(), color)]),
            }))
        }
        NamedType::Class(cls) if cls == &ctx.classes.color => {
            parse_color_value(node, code, res, diagnostics).map(Value::Gadget)
        }
        NamedType::Class(cls) if cls == &ctx.classes.cursor => {
            verify_code_return_type(
                node,
                code,
                &TypeKind::Just(NamedType::Enum(ctx.classes.cursor_shape.clone())),
                diagnostics,
            )?;
            let expr = res.unwrap_enum_set().join("|");
            Some(Value::Simple(SimpleValue::CursorShape(
                strip_enum_prefix(&expr).to_owned(),
            )))
        }
        NamedType::Class(cls) if cls == &ctx.classes.key_sequence => {
            let standard_key_en = &ctx.classes.key_sequence_standard_key;
            match (
                code.verify_return_type(&TypeKind::Just(NamedType::Enum(standard_key_en.clone()))),
                code.verify_return_type(&TypeKind::STRING),
            ) {
                (Ok(()), _) => {
                    let expr = res.unwrap_enum_set().join("|");
                    if standard_key_en.is_flag() {
                        Some(Value::Simple(SimpleValue::Set(expr)))
                    } else {
                        Some(Value::Simple(SimpleValue::Enum(expr)))
                    }
                }
                (_, Ok(())) => Some(Value::Simple(res.unwrap_into_simple_value())),
                (
                    Err(tir::TypeError::IncompatibleTypes(expected1, actual)),
                    Err(tir::TypeError::IncompatibleTypes(expected2, _)),
                ) => {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!(
                            "expression type mismatch (expected: {expected1} | {expected2}, actual: {actual})"
                        ),
                    ));
                    None
                }
                (Err(err), Err(_)) => {
                    diagnostics.push(Diagnostic::error(node.byte_range(), err.to_string()));
                    None
                }
            }
        }
        NamedType::Class(cls) if cls == &ctx.classes.pixmap => {
            verify_code_return_type(node, code, &TypeKind::STRING, diagnostics)?;
            extract_static_string(node, res, diagnostics)
                .map(|s| Value::Simple(SimpleValue::Pixmap(s)))
        }
        NamedType::Enum(en) => {
            verify_code_return_type(node, code, expected_ty, diagnostics)?;
            let expr = res.unwrap_enum_set().join("|");
            if en.is_flag() {
                Some(Value::Simple(SimpleValue::Set(expr)))
            } else {
                Some(Value::Simple(SimpleValue::Enum(expr)))
            }
        }
        NamedType::Primitive(
            PrimitiveType::Bool
            | PrimitiveType::Int
            | PrimitiveType::Uint
            | PrimitiveType::Double
            | PrimitiveType::QString,
        ) => {
            verify_code_return_type(node, code, expected_ty, diagnostics)?;
            Some(Value::Simple(res.unwrap_into_simple_value()))
        }
        NamedType::Primitive(PrimitiveType::QStringList) => {
            verify_code_return_type(node, code, expected_ty, diagnostics)?;
            extract_string_list(node, res, diagnostics)
        }
        NamedType::Primitive(PrimitiveType::Void) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                "invalid expression type: void",
            ));
            None
        }
        NamedType::Class(_) | NamedType::QmlComponent(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "unsupported constant value expression: class '{}'",
                    expected_ty.qualified_cxx_name(),
                ),
            ));
            None
        }
        NamedType::Namespace(_) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "unsupported constant value expression: namespace '{}'",
                    expected_ty.qualified_cxx_name(),
                ),
            ));
            None
        }
    }
}

#[must_use]
fn verify_code_return_type(
    node: Node,
    code: &tir::CodeBody,
    expected: &TypeKind,
    diagnostics: &mut Diagnostics,
) -> Option<()> {
    match code.verify_return_type(expected) {
        Ok(()) => Some(()),
        Err(tir::TypeError::IncompatibleTypes(expected, actual)) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!("expression type mismatch (expected: {expected}, actual: {actual})"),
            ));
            None
        }
        Err(err) => {
            diagnostics.push(Diagnostic::error(node.byte_range(), err.to_string()));
            None
        }
    }
}

fn parse_color_value(
    node: Node,
    code: &tir::CodeBody,
    res: EvaluatedValue,
    diagnostics: &mut Diagnostics,
) -> Option<Gadget> {
    // TODO: handle Qt::GlobalColor enum
    verify_code_return_type(node, code, &TypeKind::STRING, diagnostics)?;
    extract_static_string(node, res, diagnostics).and_then(|s| match s.parse::<Color>() {
        Ok(c) => Some(c.into()),
        Err(e) => {
            diagnostics.push(Diagnostic::error(node.byte_range(), e.to_string()));
            None
        }
    })
}

fn extract_static_string(
    node: Node,
    res: EvaluatedValue,
    diagnostics: &mut Diagnostics,
) -> Option<String> {
    if let (s, StringKind::NoTr) = res.unwrap_string() {
        Some(s)
    } else {
        diagnostics.push(Diagnostic::error(
            node.byte_range(),
            "must be a static string",
        ));
        None
    }
}

fn extract_string_list(
    node: Node,
    res: EvaluatedValue,
    diagnostics: &mut Diagnostics,
) -> Option<Value> {
    let xs = res.unwrap_string_list();
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

pub(super) fn strip_enum_prefix(s: &str) -> &str {
    s.split_once("::").map(|(_, t)| t).unwrap_or(s)
}
