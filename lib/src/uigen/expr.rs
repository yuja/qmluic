use super::context::ObjectContext;
use super::gadget::{Gadget, GadgetKind, ModelItem, PaletteColorGroup};
use super::property::{self, PropertiesMap};
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::color::Color;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Node, UiBindingValue};
use crate::tir;
use crate::typedexpr::BuiltinFunctionKind;
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
    /// Generates constant expression of `ty` type from the given `binding_value`.
    pub(super) fn from_binding_value(
        ctx: &ObjectContext<'a, '_, '_>,
        ty: &TypeKind<'a>,
        binding_value: &UiBindingValue<'t, '_>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        match binding_value {
            UiBindingValue::Node(n) => {
                let code = tir::build(ctx, *n, ctx.source, diagnostics)?;
                if let Some(res) = evaluate_code(&code) {
                    // constant expression can be mapped to .ui value type
                    match ty {
                        TypeKind::Just(t) => {
                            parse_as_value_type(ctx, t, *n, &code, res, diagnostics)
                                .map(PropertyValue::Serializable)
                        }
                        TypeKind::Pointer(NamedType::Class(cls))
                            if cls.is_derived_from(&ctx.classes.abstract_item_model) =>
                        {
                            verify_code_return_type(
                                *n,
                                &code,
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
                            verify_code_return_type(*n, &code, ty, diagnostics)?;
                            Some(PropertyValue::Serializable(Value::Simple(
                                SimpleValue::Cstring(res.unwrap_object_ref()),
                            )))
                        }
                        TypeKind::PointerList(NamedType::Class(_)) => {
                            verify_code_return_type(*n, &code, ty, diagnostics)?;
                            Some(PropertyValue::ObjectRefList(res.unwrap_object_ref_list()))
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
                    }
                } else {
                    verify_code_return_type(*n, &code, ty, diagnostics)?;
                    Some(PropertyValue::Dynamic(code))
                }
            }
            UiBindingValue::Map(n, m) => match ty {
                TypeKind::Just(NamedType::Class(cls)) => {
                    let properties_map =
                        property::collect_properties_with_node(ctx, cls, m, diagnostics);
                    if let Some(kind) = GadgetKind::from_class(cls, ctx.classes) {
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
    code: &tir::CodeBody,
    res: EvaluatedValue,
    diagnostics: &mut Diagnostics,
) -> Option<Value> {
    match ty {
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
                (_, Ok(())) => {
                    evaluate_as_primitive(PrimitiveType::QString, node, code, res, diagnostics)
                }
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
            match res {
                EvaluatedValue::String(s, StringKind::NoTr) => {
                    Some(Value::Simple(SimpleValue::Pixmap(s)))
                }
                EvaluatedValue::String(_, StringKind::Tr) => {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        "pixmap path must be a static string",
                    ));
                    None
                }
                _ => panic!("evaluated value must be string"),
            }
        }
        NamedType::Enum(en) => {
            verify_code_return_type(node, code, &TypeKind::Just(ty.clone()), diagnostics)?;
            let expr = res.unwrap_enum_set().join("|");
            if en.is_flag() {
                Some(Value::Simple(SimpleValue::Set(expr)))
            } else {
                Some(Value::Simple(SimpleValue::Enum(expr)))
            }
        }
        NamedType::Primitive(p) => evaluate_as_primitive(*p, node, code, res, diagnostics),
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

fn evaluate_as_primitive(
    p: PrimitiveType,
    node: Node,
    code: &tir::CodeBody,
    res: EvaluatedValue,
    diagnostics: &mut Diagnostics,
) -> Option<Value> {
    verify_code_return_type(
        node,
        code,
        &TypeKind::Just(NamedType::Primitive(p)),
        diagnostics,
    )?;
    match (p, res) {
        (PrimitiveType::Bool, EvaluatedValue::Bool(v)) => Some(Value::Simple(SimpleValue::Bool(v))),
        (PrimitiveType::Int | PrimitiveType::Uint, EvaluatedValue::Integer(v)) => {
            Some(Value::Simple(SimpleValue::Number(v as f64))) // TODO: handle overflow
        }
        (PrimitiveType::Double, EvaluatedValue::Float(v)) => {
            Some(Value::Simple(SimpleValue::Number(v)))
        }
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
            | PrimitiveType::Double
            | PrimitiveType::Int
            | PrimitiveType::QString
            | PrimitiveType::QStringList
            | PrimitiveType::Uint
            | PrimitiveType::Void,
            _,
        ) => panic!("evaluated type must be of {p:?} type"),
    }
}

/// Evaluates TIR code to constant value.
fn evaluate_code(code: &tir::CodeBody) -> Option<EvaluatedValue> {
    use tir::{BasicBlockRef, BinaryBitwiseOp, Operand, Rvalue, Statement, Terminator};

    // fast path for simple constant expression
    if let Terminator::Return(a @ Operand::Constant(_)) = code.basic_blocks[0].terminator() {
        return tir_operand_to_evaluated_value(&[], a, StringKind::NoTr);
    }

    let mut visited_blocks = vec![false; code.basic_blocks.len()];
    let mut visit_block = |r: BasicBlockRef| {
        if visited_blocks[r.0] {
            None // prevent infinite loop
        } else {
            visited_blocks[r.0] = true;
            Some(&code.basic_blocks[r.0])
        }
    };

    let mut block = visit_block(BasicBlockRef(0))?;
    let mut locals: Vec<Option<EvaluatedValue>> = vec![None; code.locals.len()];
    loop {
        for stmt in &block.statements {
            match stmt {
                Statement::Assign(l, r) => {
                    locals[l.0] = match r {
                        Rvalue::Copy(a) => {
                            tir_operand_to_evaluated_value(&locals, a, StringKind::NoTr)
                        }
                        Rvalue::BinaryBitwiseOp(BinaryBitwiseOp::Or, l, r) => {
                            tir_operands_to_evaluated_enum_set(&locals, l, r)
                        }
                        Rvalue::CallBuiltinFunction(BuiltinFunctionKind::Tr, args) => {
                            tir_operand_to_evaluated_value(&locals, &args[0], StringKind::Tr)
                        }
                        Rvalue::MakeList(args) => tir_operands_to_evaluated_list(&locals, args),
                        // No need to support other operations since constants are evaluated
                        // by TIR builder.
                        _ => return None,
                    }
                }
            }
        }

        match block.terminator() {
            Terminator::Br(r) => block = visit_block(*r)?,
            Terminator::BrCond(..) => return None, // unsupported
            Terminator::Return(a) => {
                return tir_operand_to_evaluated_value(&locals, a, StringKind::NoTr)
            }
        }
    }
}

fn tir_operand_to_evaluated_value(
    locals: &[Option<EvaluatedValue>],
    a: &tir::Operand,
    k: StringKind,
) -> Option<EvaluatedValue> {
    use tir::{ConstantValue, Operand};
    match a {
        Operand::Constant(x) => match x {
            ConstantValue::Bool(v) => Some(EvaluatedValue::Bool(*v)),
            ConstantValue::Integer(v) => Some(EvaluatedValue::Integer(*v)),
            ConstantValue::Float(v) => Some(EvaluatedValue::Float(*v)),
            ConstantValue::CString(v) => Some(EvaluatedValue::String(v.clone(), k)),
            ConstantValue::QString(v) => Some(EvaluatedValue::String(v.clone(), k)),
            ConstantValue::EmptyList => Some(EvaluatedValue::EmptyList),
        },
        Operand::EnumVariant(x) => Some(EvaluatedValue::EnumSet(vec![x.cxx_expression()])),
        Operand::Local(x) => locals[x.name.0].clone(),
        Operand::NamedObject(x) => Some(EvaluatedValue::ObjectRef(x.name.0.clone())),
    }
}

fn tir_operands_to_evaluated_list(
    locals: &[Option<EvaluatedValue>],
    args: &[tir::Operand],
) -> Option<EvaluatedValue> {
    let mut item_iter = args
        .iter()
        .map(|a| tir_operand_to_evaluated_value(locals, a, StringKind::NoTr))
        .peekable();
    match item_iter.peek() {
        Some(Some(EvaluatedValue::String(..))) => {
            let ss = item_iter
                .map(|v| {
                    if let Some(EvaluatedValue::String(s, k)) = v {
                        Some((s, k))
                    } else {
                        None
                    }
                })
                .collect::<Option<Vec<_>>>()?;
            Some(EvaluatedValue::StringList(ss))
        }
        Some(Some(EvaluatedValue::ObjectRef(..))) => {
            let ss = item_iter
                .map(|v| {
                    if let Some(EvaluatedValue::ObjectRef(s)) = v {
                        Some(s)
                    } else {
                        None
                    }
                })
                .collect::<Option<Vec<_>>>()?;
            Some(EvaluatedValue::ObjectRefList(ss))
        }
        _ => None,
    }
}

fn tir_operands_to_evaluated_enum_set(
    locals: &[Option<EvaluatedValue>],
    left: &tir::Operand,
    right: &tir::Operand,
) -> Option<EvaluatedValue> {
    match (
        tir_operand_to_evaluated_value(locals, left, StringKind::NoTr)?,
        tir_operand_to_evaluated_value(locals, right, StringKind::NoTr)?,
    ) {
        (EvaluatedValue::EnumSet(mut ls), EvaluatedValue::EnumSet(rs)) => {
            ls.extend(rs);
            Some(EvaluatedValue::EnumSet(ls))
        }
        _ => None,
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
    match res {
        EvaluatedValue::String(s, StringKind::NoTr) => match s.parse::<Color>() {
            Ok(c) => Some(c.into()),
            Err(e) => {
                diagnostics.push(Diagnostic::error(node.byte_range(), e.to_string()));
                None
            }
        },
        EvaluatedValue::String(_, StringKind::Tr) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                "color must be a static string",
            ));
            None
        }
        _ => panic!("evaluated value must be string"),
    }
}

#[derive(Clone, Debug, PartialEq)]
enum EvaluatedValue {
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String, StringKind),
    StringList(Vec<(String, StringKind)>),
    EnumSet(Vec<String>),
    ObjectRef(String),
    ObjectRefList(Vec<String>),
    EmptyList,
}

impl EvaluatedValue {
    fn unwrap_string_list(self) -> Vec<(String, StringKind)> {
        match self {
            EvaluatedValue::StringList(xs) => xs,
            EvaluatedValue::EmptyList => vec![],
            _ => panic!("evaluated value must be string list"),
        }
    }

    fn unwrap_enum_set(self) -> Vec<String> {
        match self {
            EvaluatedValue::EnumSet(es) => es,
            _ => panic!("evaluated value must be enum set"),
        }
    }

    fn unwrap_object_ref(self) -> String {
        match self {
            EvaluatedValue::ObjectRef(s) => s,
            _ => panic!("evaluated value must be object ref"),
        }
    }

    fn unwrap_object_ref_list(self) -> Vec<String> {
        match self {
            EvaluatedValue::ObjectRefList(ss) => ss,
            EvaluatedValue::EmptyList => vec![],
            _ => panic!("evaluated value must be object ref list"),
        }
    }
}

pub(super) fn strip_enum_prefix(s: &str) -> &str {
    s.split_once("::").map(|(_, t)| t).unwrap_or(s)
}
