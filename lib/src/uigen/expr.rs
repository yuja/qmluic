use super::context::ObjectContext;
use super::gadget::{Gadget, GadgetKind, ModelItem, PaletteColorGroup};
use super::property::{self, PropertiesMap};
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::color::Color;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{BinaryOperator, Node, UiBindingValue, UnaryOperator};
use crate::tir;
use crate::typedexpr::{
    self, BuiltinFunctionKind, BuiltinMethodKind, DescribeType, ExpressionVisitor, TypeDesc,
};
use crate::typemap::{
    Class, Enum, NamedType, PrimitiveType, Property, TypeKind, TypeMapError, TypeSpace,
};
use quick_xml::events::{BytesStart, BytesText, Event};
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::num::TryFromIntError;
use thiserror::Error;

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
                // TODO: switch all to TIR path
                let code = tir::build(ctx, *n, ctx.source, diagnostics)?;
                match ty {
                    TypeKind::Just(t) => {
                        // detect type error and dynamic expression first
                        let mut formatter = ExpressionFormatter::new(ctx.doc_type_name);
                        let (res_t, res_expr, _) =
                            typedexpr::walk(ctx, *n, ctx.source, &mut formatter, diagnostics)?;
                        if formatter.property_deps.is_empty() && !formatter.has_method_call {
                            // constant expression can be mapped to .ui value type
                            parse_as_value_type(ctx, t, *n, &code, res_t, res_expr, diagnostics)
                                .map(PropertyValue::Serializable)
                        } else {
                            match code.verify_return_type(ty) {
                                Ok(()) => Some(PropertyValue::Dynamic(code)),
                                Err(tir::TypeError::IncompatibleTypes(expected, actual)) => {
                                    diagnostics.push(Diagnostic::error(
                                n.byte_range(),
                                format!("expression type mismatch (expected: {expected}, actual: {actual})")
                            ));
                                    None
                                }
                                Err(err) => {
                                    diagnostics
                                        .push(Diagnostic::error(n.byte_range(), err.to_string()));
                                    None
                                }
                            }
                        }
                    }
                    TypeKind::Pointer(NamedType::Class(cls))
                        if cls.is_derived_from(&ctx.classes.abstract_item_model) =>
                    {
                        parse_item_model(*n, &code, diagnostics).map(PropertyValue::ItemModel)
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
    res_t: TypeDesc,
    res_expr: String,
    diagnostics: &mut Diagnostics,
) -> Option<Value> {
    match ty {
        NamedType::Class(cls) if cls == &ctx.classes.brush => {
            let color = parse_color_value(node, code, diagnostics).map(Value::Gadget)?;
            let style = SimpleValue::Enum("Qt::SolidPattern".to_owned());
            Some(Value::Gadget(Gadget {
                kind: GadgetKind::Brush,
                attributes: HashMap::from([("brushstyle".to_owned(), style)]),
                properties: HashMap::from([("color".to_owned(), color)]),
            }))
        }
        NamedType::Class(cls) if cls == &ctx.classes.color => {
            parse_color_value(node, code, diagnostics).map(Value::Gadget)
        }
        NamedType::Class(cls) if cls == &ctx.classes.cursor => match &res_t {
            TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(res_en)))
                if is_compatible_enum(res_en, &ctx.classes.cursor_shape) =>
            {
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
        },
        NamedType::Class(cls) if cls == &ctx.classes.key_sequence => {
            let standard_key_en = &ctx.classes.key_sequence_standard_key;
            match &res_t {
                TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(res_en)))
                    if is_compatible_enum(res_en, standard_key_en) =>
                {
                    if standard_key_en.is_flag() {
                        Some(Value::Simple(SimpleValue::Set(res_expr)))
                    } else {
                        Some(Value::Simple(SimpleValue::Enum(res_expr)))
                    }
                }
                TypeDesc::ConstString | &TypeDesc::STRING => {
                    evaluate_as_primitive(PrimitiveType::QString, node, code, diagnostics)
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        node.byte_range(),
                        format!(
                            "expression type mismatch (expected: {} | {}, actual: {})",
                            standard_key_en.qualified_cxx_name(),
                            PrimitiveType::QString.name(),
                            res_t.qualified_name()
                        ),
                    ));
                    None
                }
            }
        }
        NamedType::Class(cls) if cls == &ctx.classes.pixmap => {
            let res = evaluate_code(code).expect("constant expression can be evaluated");
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
        NamedType::Enum(en) => match &res_t {
            TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(res_en)))
                if is_compatible_enum(res_en, en) =>
            {
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
        },
        NamedType::Primitive(p) => evaluate_as_primitive(*p, node, code, diagnostics),
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
    diagnostics: &mut Diagnostics,
) -> Option<Value> {
    let res = evaluate_code(code).expect("constant expression can be evaluated");
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
            res,
        ) => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                format!(
                    "evaluated type mismatch (expected: {}, actual: {})",
                    p.name(),
                    res.type_desc().qualified_name()
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
    typedexpr::walk(ctx, node, ctx.source, &mut ExpressionEvaluator, diagnostics)
}

/// Evaluates TIR code to constant value.
fn evaluate_code(code: &tir::CodeBody) -> Option<EvaluatedValue> {
    use tir::{BasicBlockRef, Operand, Rvalue, Statement, Terminator};

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
        Operand::EnumVariant(_) => None,
        Operand::Local(x) => locals[x.name.0].clone(),
        Operand::NamedObject(_) => None,
    }
}

fn tir_operands_to_evaluated_list(
    locals: &[Option<EvaluatedValue>],
    args: &[tir::Operand],
) -> Option<EvaluatedValue> {
    let ss = args
        .iter()
        .map(|a| {
            if let Some(EvaluatedValue::String(s, k)) =
                tir_operand_to_evaluated_value(locals, a, StringKind::NoTr)
            {
                Some((s, k))
            } else {
                None
            }
        })
        .collect::<Option<Vec<_>>>()?;
    Some(EvaluatedValue::StringList(ss))
}

fn parse_color_value(
    node: Node,
    code: &tir::CodeBody,
    diagnostics: &mut Diagnostics,
) -> Option<Gadget> {
    // TODO: handle Qt::GlobalColor enum
    let res = evaluate_code(code).expect("constant expression can be evaluated");
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
    node: Node,
    code: &tir::CodeBody,
    diagnostics: &mut Diagnostics,
) -> Option<Vec<ModelItem>> {
    let res = match evaluate_code(code) {
        Some(v) => v,
        None => {
            diagnostics.push(Diagnostic::error(
                node.byte_range(),
                "unsupported dynamic binding",
            ));
            return None;
        }
    };
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
    let obj_ref = typedexpr::walk(ctx, node, ctx.source, &mut ObjectRefCollector, diagnostics)?;
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
    let obj_ref = typedexpr::walk(ctx, node, ctx.source, &mut ObjectRefCollector, diagnostics)?;
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
    #[error("unsupported type: {0}")]
    UnsupportedType(String),
    #[error("unsupported literal: {0}")]
    UnsupportedLiteral(&'static str),
    #[error("unsupported reference")]
    UnsupportedReference,
    #[error("unsupported object property resolution")]
    UnsupportedObjectProperty,
    #[error("not a readable property")]
    UnreadableProperty,
    #[error("unsupported function call")]
    UnsupportedFunctionCall,
    #[error("unsupported unary operation on type: {0} <{1}>")]
    UnsupportedUnaryOperationOnType(UnaryOperator, String),
    #[error("unsupported binary operation on types: <{1}> {0} <{2}>")]
    UnsupportedBinaryOperationOnType(BinaryOperator, String, String),
    #[error("unsupported ternary expression")]
    UnsupportedTernaryExpression,
    #[error("condition must be of bool type, but got: {0}")]
    UnsupportedConditionType(String),
    #[error("cannot evaluate as constant")]
    CannotEvaluateAsConstant,
    #[error("cannot deduce type from '{0}' and '{1}'")]
    CannotDeduceType(String, String),
    #[error("integer conversion failed")]
    IntegerConversion(#[from] TryFromIntError),
    #[error("type resolution failed: {0}")]
    TypeResolution(#[from] TypeMapError),
}

/// Evaluates expression tree as arbitrary constant value expression.
///
/// Here we don't follow the JavaScript language model, but try to be stricter.
#[derive(Debug)]
struct ExpressionEvaluator;

#[derive(Clone, Debug, PartialEq)]
enum EvaluatedValue {
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String, StringKind),
    StringList(Vec<(String, StringKind)>),
    EmptyList,
}

impl DescribeType<'_> for EvaluatedValue {
    fn type_desc(&self) -> TypeDesc<'static> {
        match self {
            EvaluatedValue::Bool(_) => TypeDesc::BOOL,
            EvaluatedValue::Integer(_) => TypeDesc::ConstInteger,
            EvaluatedValue::Float(_) => TypeDesc::DOUBLE,
            EvaluatedValue::String(_, StringKind::NoTr) => TypeDesc::ConstString,
            EvaluatedValue::String(_, StringKind::Tr) => TypeDesc::STRING,
            EvaluatedValue::StringList(_) => TypeDesc::STRING_LIST,
            EvaluatedValue::EmptyList => TypeDesc::EmptyList,
        }
    }
}

impl<'a> ExpressionVisitor<'a> for ExpressionEvaluator {
    type Item = EvaluatedValue;
    type Label = ();
    type Error = ExpressionError;

    fn visit_integer(&mut self, value: u64) -> Result<Self::Item, Self::Error> {
        Ok(EvaluatedValue::Integer(value.try_into()?))
    }

    fn visit_float(&mut self, value: f64) -> Result<Self::Item, Self::Error> {
        Ok(EvaluatedValue::Float(value))
    }

    fn visit_string(&mut self, value: String) -> Result<Self::Item, Self::Error> {
        Ok(EvaluatedValue::String(value, StringKind::NoTr))
    }

    fn visit_bool(&mut self, value: bool) -> Result<Self::Item, Self::Error> {
        Ok(EvaluatedValue::Bool(value))
    }

    fn visit_enum(
        &mut self,
        _enum_ty: Enum<'a>,
        _variant: &str,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("enum")) // enum value is unknown
    }

    fn visit_array(&mut self, elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error> {
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

    fn visit_object_ref(
        &mut self,
        _cls: Class<'a>,
        _name: &str,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedReference)
    }

    fn visit_object_property(
        &mut self,
        _object: Self::Item,
        _property: Property<'a>,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedObjectProperty)
    }

    fn visit_object_builtin_method_call(
        &mut self,
        _object: Self::Item,
        _function: BuiltinMethodKind,
        _arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedFunctionCall)
    }

    fn visit_builtin_call(
        &mut self,
        function: BuiltinFunctionKind,
        mut arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        match function {
            BuiltinFunctionKind::Tr if arguments.len() == 1 => match arguments.pop() {
                Some(EvaluatedValue::String(a, StringKind::NoTr)) => {
                    Ok(EvaluatedValue::String(a, StringKind::Tr))
                }
                _ => Err(ExpressionError::UnsupportedFunctionCall),
            },
            BuiltinFunctionKind::Tr => Err(ExpressionError::UnsupportedFunctionCall),
        }
    }

    fn visit_unary_expression(
        &mut self,
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
            EvaluatedValue::Integer(a) => match operator {
                // TODO: handle overflow, etc.
                LogicalNot => Err(type_error()),
                BitwiseNot => Ok(EvaluatedValue::Integer(!a)),
                Minus => Ok(EvaluatedValue::Integer(-a)),
                Plus => Ok(EvaluatedValue::Integer(a)),
                Typeof | Void | Delete => Err(type_error()),
            },
            EvaluatedValue::Float(a) => match operator {
                // TODO: handle overflow, etc.
                LogicalNot => Err(type_error()),
                BitwiseNot => Err(type_error()),
                Minus => Ok(EvaluatedValue::Float(-a)),
                Plus => Ok(EvaluatedValue::Float(a)),
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
        &mut self,
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
            (EvaluatedValue::Integer(l), EvaluatedValue::Integer(r)) => match operator {
                // TODO: handle overflow, etc.
                LogicalAnd | LogicalOr => Err(type_error()),
                // TODO: >>, (unsigned)>>, <<
                RightShift => Err(type_error()),
                UnsignedRightShift => Err(type_error()),
                LeftShift => Err(type_error()),
                BitwiseAnd => Ok(EvaluatedValue::Integer(l & r)),
                BitwiseXor => Ok(EvaluatedValue::Integer(l ^ r)),
                BitwiseOr => Ok(EvaluatedValue::Integer(l | r)),
                Add => Ok(EvaluatedValue::Integer(l + r)),
                Sub => Ok(EvaluatedValue::Integer(l - r)),
                Mul => Ok(EvaluatedValue::Integer(l * r)),
                Div => Ok(EvaluatedValue::Integer(l / r)),
                Rem => Ok(EvaluatedValue::Integer(l % r)),
                Exp => Ok(EvaluatedValue::Integer(l.pow(r.try_into()?))),
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
            (EvaluatedValue::Float(l), EvaluatedValue::Float(r)) => match operator {
                // TODO: handle overflow, etc.
                LogicalAnd | LogicalOr => Err(type_error()),
                RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                BitwiseAnd | BitwiseXor | BitwiseOr => Err(type_error()),
                Add => Ok(EvaluatedValue::Float(l + r)),
                Sub => Ok(EvaluatedValue::Float(l - r)),
                Mul => Ok(EvaluatedValue::Float(l * r)),
                Div => Ok(EvaluatedValue::Float(l / r)),
                Rem => Ok(EvaluatedValue::Float(l % r)),
                Exp => Ok(EvaluatedValue::Float(l.powf(r))),
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

    fn visit_ternary_expression(
        &mut self,
        condition: Self::Item,
        consequence: Self::Item,
        alternative: Self::Item,
        _condition_label: Self::Label,
        _consequence_label: Self::Label,
        _alternative_label: Self::Label,
    ) -> Result<Self::Item, Self::Error> {
        match condition {
            EvaluatedValue::Bool(b) => Ok(if b { consequence } else { alternative }),
            EvaluatedValue::Integer(_)
            | EvaluatedValue::Float(_)
            | EvaluatedValue::String(..)
            | EvaluatedValue::StringList(_)
            | EvaluatedValue::EmptyList => Err(ExpressionError::UnsupportedConditionType(
                condition.type_desc().qualified_name().into(),
            )),
        }
    }

    fn mark_branch_point(&mut self) -> Self::Label {}
}

/// Formats expression tree as arbitrary constant value expression.
///
/// Here we don't strictly follow the JavaScript language model, but try 1:1 mapping.
#[derive(Debug)]
struct ExpressionFormatter<'a> {
    /// Context of `QCoreApplication::translate()`, which is typically a class name.
    tr_context: String,
    /// List of `(obj_expr, property)` accessed from this expression.
    property_deps: Vec<(String, Property<'a>)>,
    /// Whether or not the expression has a object method call which ExpressionEvaluator
    /// doesn't support.
    has_method_call: bool,
}

impl ExpressionFormatter<'_> {
    fn new(tr_context: impl Into<String>) -> Self {
        ExpressionFormatter {
            tr_context: tr_context.into(),
            property_deps: Vec::new(),
            has_method_call: false,
        }
    }
}

impl<'a> DescribeType<'a> for (TypeDesc<'a>, String, u32) {
    fn type_desc(&self) -> TypeDesc<'a> {
        self.0.clone()
    }
}

impl<'a> ExpressionVisitor<'a> for ExpressionFormatter<'a> {
    type Item = (TypeDesc<'a>, String, u32);
    type Label = ();
    type Error = ExpressionError;

    fn visit_integer(&mut self, value: u64) -> Result<Self::Item, Self::Error> {
        Ok((TypeDesc::ConstInteger, value.to_string(), PREC_TERM))
    }

    fn visit_float(&mut self, value: f64) -> Result<Self::Item, Self::Error> {
        Ok((TypeDesc::DOUBLE, format!("{value:e}"), PREC_TERM))
    }

    fn visit_string(&mut self, value: String) -> Result<Self::Item, Self::Error> {
        Ok((TypeDesc::ConstString, format!("{:?}", value), PREC_TERM)) // TODO: escape per C spec)
    }

    fn visit_bool(&mut self, value: bool) -> Result<Self::Item, Self::Error> {
        Ok((
            TypeDesc::BOOL,
            if value {
                "true".to_owned()
            } else {
                "false".to_owned()
            },
            PREC_TERM,
        ))
    }

    fn visit_enum(&mut self, enum_ty: Enum<'a>, variant: &str) -> Result<Self::Item, Self::Error> {
        let res_expr = enum_ty.qualify_cxx_variant_name(variant);
        Ok((
            TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(enum_ty))),
            res_expr,
            PREC_SCOPE,
        ))
    }

    fn visit_array(&mut self, elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error> {
        let mut elem_t: Option<TypeDesc> = None;
        let mut elem_exprs = Vec::with_capacity(elements.len());
        for (t, expr, prec) in elements {
            let (t, expr, prec) = ensure_concrete_string(t, expr, prec);
            elem_t = match elem_t {
                Some(known) => Some(deduce_type(known, t)?),
                None => Some(t),
            };
            elem_exprs.push(maybe_paren(PREC_COMMA, expr, prec));
        }

        let array_t = match elem_t {
            Some(TypeDesc::STRING) => TypeDesc::STRING_LIST,
            Some(TypeDesc::Concrete(TypeKind::Pointer(t))) => {
                TypeDesc::Concrete(TypeKind::PointerList(t))
            }
            Some(t @ (TypeDesc::ConstInteger | TypeDesc::EmptyList | TypeDesc::Concrete(_))) => {
                return Err(ExpressionError::UnsupportedType(format!(
                    "array of {}",
                    t.qualified_name()
                )));
            }
            Some(TypeDesc::ConstString) => unreachable!("must be converted to concrete type"),
            None => TypeDesc::EmptyList,
        };
        // not a term, but would be as strong as a term
        Ok((array_t, format!("{{{}}}", elem_exprs.join(", ")), PREC_TERM))
    }

    fn visit_object_ref(&mut self, cls: Class<'a>, name: &str) -> Result<Self::Item, Self::Error> {
        Ok((
            TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(cls))),
            name.to_owned(),
            PREC_TERM,
        ))
    }

    fn visit_object_property(
        &mut self,
        (_obj_t, obj_expr, obj_prec): Self::Item,
        property: Property<'a>,
    ) -> Result<Self::Item, Self::Error> {
        let res_t = property.value_type().map(TypeDesc::Concrete)?;
        let res_expr = format!(
            "{}->{}()",
            maybe_paren(PREC_MEMBER, obj_expr.clone(), obj_prec),
            property
                .read_func_name()
                .ok_or(ExpressionError::UnreadableProperty)?,
        );
        self.property_deps.push((obj_expr, property));
        Ok((res_t, res_expr, PREC_MEMBER))
    }

    fn visit_object_builtin_method_call(
        &mut self,
        (obj_t, obj_expr, obj_prec): Self::Item,
        function: BuiltinMethodKind,
        arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        self.has_method_call = true;
        match function {
            BuiltinMethodKind::Arg if arguments.len() == 1 => {
                let (obj_t, obj_expr, obj_prec) = ensure_concrete_string(obj_t, obj_expr, obj_prec);
                assert!(obj_t == TypeDesc::STRING);
                let (arg_t, arg_expr, _) = &arguments[0];
                match arg_t {
                    TypeDesc::ConstInteger
                    | TypeDesc::ConstString
                    | TypeDesc::Concrete(TypeKind::Just(NamedType::Primitive(
                        PrimitiveType::Bool
                        | PrimitiveType::Double
                        | PrimitiveType::Int
                        | PrimitiveType::QString
                        | PrimitiveType::Uint,
                    ))) => Ok((
                        TypeDesc::STRING,
                        format!(
                            "{}.arg({})",
                            maybe_paren(PREC_MEMBER, obj_expr, obj_prec),
                            arg_expr
                        ),
                        PREC_MEMBER,
                    )),
                    TypeDesc::EmptyList
                    | TypeDesc::Concrete(TypeKind::Just(
                        NamedType::Class(_)
                        | NamedType::Enum(_)
                        | NamedType::Namespace(_)
                        | NamedType::Primitive(PrimitiveType::QStringList | PrimitiveType::Void)
                        | NamedType::QmlComponent(_),
                    ))
                    | TypeDesc::Concrete(TypeKind::Pointer(_) | TypeKind::PointerList(_)) => {
                        Err(ExpressionError::UnsupportedFunctionCall)
                    }
                }
            }
            BuiltinMethodKind::Arg => Err(ExpressionError::UnsupportedFunctionCall),
        }
    }

    fn visit_builtin_call(
        &mut self,
        function: BuiltinFunctionKind,
        arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        match function {
            BuiltinFunctionKind::Tr if arguments.len() == 1 => {
                if let (TypeDesc::ConstString, expr, _prec) = &arguments[0] {
                    Ok((
                        TypeDesc::STRING,
                        format!(
                            "QCoreApplication::translate({:?}, {})",
                            self.tr_context, expr
                        ),
                        PREC_CALL,
                    ))
                } else {
                    Err(ExpressionError::UnsupportedFunctionCall)
                }
            }
            BuiltinFunctionKind::Tr => Err(ExpressionError::UnsupportedFunctionCall),
        }
    }

    fn visit_unary_expression(
        &mut self,
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
            TypeDesc::BOOL => match operator {
                LogicalNot => Ok((TypeDesc::BOOL, res_expr, res_prec)),
                BitwiseNot => Err(type_error()),
                Minus | Plus => Err(type_error()),
                Typeof | Void | Delete => Err(type_error()),
            },
            t @ (TypeDesc::ConstInteger | TypeDesc::INT | TypeDesc::UINT) => match operator {
                LogicalNot => Err(type_error()),
                BitwiseNot | Minus | Plus => Ok((t, res_expr, res_prec)),
                Typeof | Void | Delete => Err(type_error()),
            },
            t @ TypeDesc::DOUBLE => match operator {
                LogicalNot => Err(type_error()),
                BitwiseNot => Err(type_error()),
                Minus | Plus => Ok((t, res_expr, res_prec)),
                Typeof | Void | Delete => Err(type_error()),
            },
            TypeDesc::ConstString | TypeDesc::STRING => Err(type_error()),
            t @ TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(_))) => match operator {
                LogicalNot => Err(type_error()),
                BitwiseNot => Ok((t, res_expr, res_prec)),
                Minus | Plus => Err(type_error()),
                Typeof | Void | Delete => Err(type_error()),
            },
            _ => Err(type_error()),
        }
    }

    fn visit_binary_expression(
        &mut self,
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
            (TypeDesc::BOOL, TypeDesc::BOOL) => {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Ok((TypeDesc::BOOL, res_expr, res_prec)),
                    RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                    BitwiseAnd | BitwiseXor | BitwiseOr => Ok((TypeDesc::BOOL, res_expr, res_prec)),
                    Add | Sub | Mul | Div | Rem | Exp => Err(type_error()),
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::BOOL, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (
                left_t @ (TypeDesc::ConstInteger | TypeDesc::INT | TypeDesc::UINT),
                right_t @ (TypeDesc::ConstInteger | TypeDesc::INT | TypeDesc::UINT),
            ) => {
                let res_t = deduce_type(left_t, right_t)?;
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
                    BitwiseAnd | BitwiseXor | BitwiseOr => Ok((res_t, res_expr, res_prec)),
                    Add | Sub | Mul | Div | Rem => Ok((res_t, res_expr, res_prec)),
                    Exp => Err(type_error()), // TODO
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::BOOL, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (left_t @ TypeDesc::DOUBLE, right_t @ TypeDesc::DOUBLE) => {
                let res_t = deduce_type(left_t, right_t)?;
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
                    Add | Sub | Mul | Div | Rem => Ok((res_t, res_expr, res_prec)),
                    Exp => Err(type_error()), // TODO
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::BOOL, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (left_t @ TypeDesc::ConstString, right_t @ TypeDesc::ConstString)
                if left_prec == PREC_TERM && right_prec == PREC_TERM =>
            {
                match operator {
                    LogicalAnd | LogicalOr => Err(type_error()),
                    RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                    BitwiseAnd | BitwiseXor | BitwiseOr => Err(type_error()),
                    Add => Ok((
                        TypeDesc::ConstString,
                        left_expr + " " + &right_expr, // concat "<left>" "<right>"
                        PREC_TERM,
                    )),
                    Sub | Mul | Div | Rem | Exp => Err(type_error()),
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => {
                        let (_, left_expr, left_prec) =
                            ensure_concrete_string(left_t, left_expr, left_prec);
                        let (_, right_expr, right_prec) =
                            ensure_concrete_string(right_t, right_expr, right_prec);
                        let res_expr = [
                            &maybe_paren(res_prec, left_expr, left_prec),
                            binary_operator_str(operator),
                            &maybe_paren(res_prec, right_expr, right_prec),
                        ]
                        .concat();
                        Ok((TypeDesc::BOOL, res_expr, res_prec))
                    }
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (
                left_t @ (TypeDesc::ConstString | TypeDesc::STRING),
                right_t @ (TypeDesc::ConstString | TypeDesc::STRING),
            ) => {
                let (_, left_expr, left_prec) =
                    ensure_concrete_string(left_t, left_expr, left_prec);
                let (_, right_expr, right_prec) =
                    ensure_concrete_string(right_t, right_expr, right_prec);
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
                    Add => Ok((TypeDesc::STRING, res_expr, res_prec)),
                    Sub | Mul | Div | Rem | Exp => Err(type_error()),
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::BOOL, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            (
                TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(left_en))),
                TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(right_en))),
            ) if is_compatible_enum(&left_en, &right_en) => {
                let res_expr = [
                    &maybe_paren(res_prec, left_expr, left_prec),
                    binary_operator_str(operator),
                    &maybe_paren(res_prec, right_expr, right_prec),
                ]
                .concat();
                match operator {
                    LogicalAnd | LogicalOr => Err(type_error()),
                    RightShift | UnsignedRightShift | LeftShift => Err(type_error()),
                    BitwiseAnd | BitwiseXor | BitwiseOr => Ok((
                        TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(left_en))),
                        res_expr,
                        res_prec,
                    )),
                    Add | Sub | Mul | Div | Rem | Exp => Err(type_error()),
                    Equal | StrictEqual | NotEqual | StrictNotEqual | LessThan | LessThanEqual
                    | GreaterThan | GreaterThanEqual => Ok((TypeDesc::BOOL, res_expr, res_prec)),
                    NullishCoalesce | Instanceof | In => Err(type_error()),
                }
            }
            _ => Err(type_error()),
        }
    }

    fn visit_ternary_expression(
        &mut self,
        (condition_t, condition_expr, condition_prec): Self::Item,
        (consequence_t, consequence_expr, consequence_prec): Self::Item,
        (alternative_t, alternative_expr, alternative_prec): Self::Item,
        _condition_label: Self::Label,
        _consequence_label: Self::Label,
        _alternative_label: Self::Label,
    ) -> Result<Self::Item, Self::Error> {
        if condition_t != TypeDesc::BOOL {
            return Err(ExpressionError::UnsupportedConditionType(
                condition_t.qualified_name().into(),
            ));
        }
        let (consequence_t, consequence_expr, consequence_prec) =
            ensure_concrete_string(consequence_t, consequence_expr, consequence_prec);
        let (alternative_t, alternative_expr, alternative_prec) =
            ensure_concrete_string(alternative_t, alternative_expr, alternative_prec);
        let res_t = deduce_type(consequence_t, alternative_t)?;
        let res_expr = format!(
            "{} ? {} : {}",
            maybe_paren(PREC_TERNARY, condition_expr, condition_prec),
            maybe_paren(PREC_TERNARY, consequence_expr, consequence_prec),
            maybe_paren(PREC_TERNARY, alternative_expr, alternative_prec),
        );
        Ok((res_t, res_expr, PREC_TERNARY))
    }

    fn mark_branch_point(&mut self) -> Self::Label {}
}

fn is_compatible_enum(left_en: &Enum, right_en: &Enum) -> bool {
    left_en == right_en
        || left_en
            .alias_enum()
            .and_then(|r| r.ok())
            .map_or(false, |en| &en == right_en)
        || right_en
            .alias_enum()
            .and_then(|r| r.ok())
            .map_or(false, |en| &en == left_en)
}

fn deduce_type<'a>(
    left_t: TypeDesc<'a>,
    right_t: TypeDesc<'a>,
) -> Result<TypeDesc<'a>, ExpressionError> {
    match (left_t, right_t) {
        (l, r) if l == r => Ok(l),
        (l @ (TypeDesc::INT | TypeDesc::UINT), TypeDesc::ConstInteger) => Ok(l),
        (TypeDesc::ConstInteger, r @ (TypeDesc::INT | TypeDesc::UINT)) => Ok(r),
        (
            TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(l))),
            TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(r))),
        ) if is_compatible_enum(&l, &r) => {
            Ok(TypeDesc::Concrete(TypeKind::Just(NamedType::Enum(l))))
        }
        (
            TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(l))),
            TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(r))),
        ) => l
            .common_base_class(&r)
            .transpose()?
            .map(|c| TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(c))))
            .ok_or_else(|| {
                ExpressionError::CannotDeduceType(
                    l.qualified_cxx_name().into(),
                    r.qualified_cxx_name().into(),
                )
            }),
        (l, r) => Err(ExpressionError::CannotDeduceType(
            l.qualified_name().into(),
            r.qualified_name().into(),
        )),
    }
}

pub(super) fn strip_enum_prefix(s: &str) -> &str {
    s.split_once("::").map(|(_, t)| t).unwrap_or(s)
}

fn ensure_concrete_string(t: TypeDesc, expr: String, prec: u32) -> (TypeDesc, String, u32) {
    match t {
        TypeDesc::ConstString => (
            TypeDesc::STRING,
            format!("QStringLiteral({})", expr),
            PREC_CALL,
        ),
        t => (t, expr, prec),
    }
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
const PREC_MEMBER: u32 = 2;
const PREC_TERNARY: u32 = 16;
const PREC_COMMA: u32 = 17;

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
            ObjectRef::Just(cls, _) => {
                TypeDesc::Concrete(TypeKind::Pointer(NamedType::Class(cls.clone())))
            }
            ObjectRef::List(cls, _) => {
                TypeDesc::Concrete(TypeKind::PointerList(NamedType::Class(cls.clone())))
            }
            ObjectRef::EmptyList => TypeDesc::EmptyList,
        }
    }
}

impl<'a> ExpressionVisitor<'a> for ObjectRefCollector {
    type Item = ObjectRef<'a>;
    type Label = ();
    type Error = ExpressionError;

    fn visit_integer(&mut self, _value: u64) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("integer"))
    }

    fn visit_float(&mut self, _value: f64) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("float"))
    }

    fn visit_string(&mut self, _value: String) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("string"))
    }

    fn visit_bool(&mut self, _value: bool) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("bool"))
    }

    fn visit_enum(
        &mut self,
        _enum_ty: Enum<'a>,
        _variant: &str,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedLiteral("enum"))
    }

    fn visit_array(&mut self, elements: Vec<Self::Item>) -> Result<Self::Item, Self::Error> {
        let mut base_cls: Option<Class> = None;
        let mut names = Vec::with_capacity(elements.len());
        for r in elements {
            match r {
                ObjectRef::Just(cls, s) => {
                    base_cls = match base_cls {
                        Some(base) if base != cls => {
                            Some(base.common_base_class(&cls).transpose()?.ok_or_else(|| {
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

    fn visit_object_ref(&mut self, cls: Class<'a>, name: &str) -> Result<Self::Item, Self::Error> {
        Ok(ObjectRef::Just(cls, name.to_owned()))
    }

    fn visit_object_property(
        &mut self,
        _object: Self::Item,
        _property: Property<'a>,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedObjectProperty)
    }

    fn visit_object_builtin_method_call(
        &mut self,
        _object: Self::Item,
        _function: BuiltinMethodKind,
        _arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedFunctionCall)
    }

    fn visit_builtin_call(
        &mut self,
        _function: BuiltinFunctionKind,
        _arguments: Vec<Self::Item>,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedFunctionCall)
    }

    fn visit_unary_expression(
        &mut self,
        operator: UnaryOperator,
        argument: Self::Item,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedUnaryOperationOnType(
            operator,
            argument.type_desc().qualified_name().into(),
        ))
    }

    fn visit_binary_expression(
        &mut self,
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

    fn visit_ternary_expression(
        &mut self,
        _condition: Self::Item,
        _consequence: Self::Item,
        _alternative: Self::Item,
        _condition_label: Self::Label,
        _consequence_label: Self::Label,
        _alternative_label: Self::Label,
    ) -> Result<Self::Item, Self::Error> {
        Err(ExpressionError::UnsupportedTernaryExpression)
    }

    fn mark_branch_point(&mut self) -> Self::Label {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Diagnostics;
    use crate::metatype;
    use crate::qmlast::{UiObjectDefinition, UiProgram};
    use crate::qmldoc::UiDocument;
    use crate::typedexpr::{BuiltinFunctionKind, RefKind, RefSpace};
    use crate::typemap::{ModuleData, ModuleId, Namespace, TypeMap};

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
            let foo_meta = metatype::Class {
                class_name: "Foo".to_owned(),
                qualified_class_name: "Foo".to_owned(),
                object: true,
                enums: vec![metatype::Enum::with_values("Bar", ["Bar0", "Bar1", "Bar2"])],
                properties: vec![
                    metatype::Property {
                        name: "checked".to_owned(),
                        r#type: "bool".to_owned(),
                        read: Some("isChecked".to_owned()),
                        write: Some("setChecked".to_owned()),
                        notify: Some("toggled".to_owned()),
                        ..Default::default()
                    },
                    metatype::Property {
                        name: "currentIndex".to_owned(),
                        r#type: "int".to_owned(),
                        read: Some("currentIndex".to_owned()),
                        write: Some("setCurrentIndex".to_owned()),
                        notify: Some("currentIndexChanged".to_owned()),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            };
            type_map
                .get_module_data_mut(&module_id)
                .unwrap()
                .extend([foo_meta, metatype::Class::new("A")]);
            Env {
                doc: UiDocument::parse(format!("A {{ a: {expr_source}}}"), "MyType", None),
                type_map,
                module_id,
            }
        }

        fn node(&self) -> Node {
            let program = UiProgram::from_node(self.doc.root_node(), self.doc.source()).unwrap();
            let obj = UiObjectDefinition::from_node(program.root_object_node(), self.doc.source())
                .unwrap();
            let map = obj.build_binding_map(self.doc.source()).unwrap();
            map.get("a").unwrap().get_node().unwrap()
        }

        fn evaluate(&self) -> EvaluatedValue {
            self.try_evaluate().unwrap()
        }

        fn try_evaluate(&self) -> Result<EvaluatedValue, Diagnostics> {
            let mut diagnostics = Diagnostics::new();
            let ctx = Context {
                type_space: self.type_map.get_module(&self.module_id).unwrap(),
            };
            let node = self.node();
            typedexpr::walk(
                &ctx,
                node,
                self.doc.source(),
                &mut ExpressionEvaluator,
                &mut diagnostics,
            )
            .ok_or(diagnostics)
        }

        fn format(&self) -> (TypeDesc, String, u32) {
            self.try_format().unwrap()
        }

        fn try_format(&self) -> Result<(TypeDesc, String, u32), Diagnostics> {
            let mut diagnostics = Diagnostics::new();
            let ctx = Context {
                type_space: self.type_map.get_module(&self.module_id).unwrap(),
            };
            let node = self.node();
            typedexpr::walk(
                &ctx,
                node,
                self.doc.source(),
                &mut ExpressionFormatter::new("MyClass"),
                &mut diagnostics,
            )
            .ok_or(diagnostics)
        }
    }

    struct Context<'a> {
        type_space: Namespace<'a>,
    }

    impl<'a> RefSpace<'a> for Context<'a> {
        fn get_ref(&self, name: &str) -> Option<Result<RefKind<'a>, TypeMapError>> {
            match name {
                "foo" => match self.type_space.get_type("Foo").unwrap().unwrap() {
                    NamedType::Class(cls) => Some(Ok(RefKind::Object(cls))),
                    _ => panic!("Foo must be of class type"),
                },
                "qsTr" => Some(Ok(RefKind::BuiltinFunction(BuiltinFunctionKind::Tr))),
                _ => self.type_space.get_ref(name),
            }
        }
    }

    fn evaluate_expr(expr_source: &str) -> EvaluatedValue {
        Env::new(expr_source).evaluate()
    }

    fn format_expr(expr_source: &str) -> String {
        let (_, expr, _) = Env::new(expr_source).format();
        expr
    }

    #[test]
    fn evaluate_integer_bit_ops() {
        assert_eq!(
            evaluate_expr("((1 | 2) ^ 5) & ~0"),
            EvaluatedValue::Integer(6)
        );
    }

    #[test]
    fn format_integer_bit_ops() {
        assert_eq!(format_expr("((1 | 2) ^ 5) & ~0"), "((1|2)^5)&~0");
    }

    #[test]
    fn evalute_number_division() {
        assert_eq!(evaluate_expr("3 / 2 * 2"), EvaluatedValue::Integer(2));
        assert_eq!(evaluate_expr("3. / 2. * 2."), EvaluatedValue::Float(3.));
    }

    #[test]
    fn format_number() {
        assert_eq!(format_expr("123"), "123");
        assert_eq!(format_expr("123."), "1.23e2");
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
        assert_eq!(format_expr("'foo' + 'bar'"), r#""foo" "bar""#);
        assert_eq!(
            format_expr("'foo' === 'bar'"),
            r#"QStringLiteral("foo")==QStringLiteral("bar")"#
        );
        assert_eq!(
            format_expr("'foo' + qsTr('bar')"),
            r#"QStringLiteral("foo")+QCoreApplication::translate("MyClass", "bar")"#
        );
    }

    #[test]
    fn format_tr() {
        assert_eq!(
            format_expr("qsTr('foo')"),
            r#"QCoreApplication::translate("MyClass", "foo")"#
        );
    }

    #[test]
    fn format_string_arg() {
        assert_eq!(
            format_expr("'foo %1'.arg('bar')"),
            r#"QStringLiteral("foo %1").arg("bar")"#
        );
        assert_eq!(
            format_expr("qsTr('foo %1').arg(1)"),
            r#"QCoreApplication::translate("MyClass", "foo %1").arg(1)"#
        );
        assert!(Env::new("'foo'.arg([])").try_format().is_err());
    }

    #[test]
    fn format_operator_number() {
        assert_eq!(
            format_expr("foo.currentIndex === 0"),
            "foo->currentIndex()==0"
        );
        assert_eq!(
            format_expr("foo.currentIndex - 1 === 0"),
            "foo->currentIndex()-1==0"
        );
    }

    #[test]
    fn format_empty_array() {
        assert_eq!(format_expr("[]"), "{}");
    }

    #[test]
    fn format_string_array() {
        assert_eq!(
            format_expr("['foo', 'bar']"),
            r#"{QStringLiteral("foo"), QStringLiteral("bar")}"#
        );
    }

    #[test]
    fn format_object_property() {
        assert_eq!(format_expr("!foo.checked"), "!foo->isChecked()",);
    }

    #[test]
    fn evalute_ternary() {
        assert_eq!(evaluate_expr("true ? 1 : 2"), EvaluatedValue::Integer(1));
        assert_eq!(evaluate_expr("false ? 1 : 2"), EvaluatedValue::Integer(2));
    }

    #[test]
    fn format_ternary() {
        assert_eq!(
            format_expr("foo.checked ? 1 : 2"),
            "foo->isChecked() ? 1 : 2"
        );
        assert_eq!(
            format_expr("foo.checked ? 'foo' : qsTr('bar')"),
            r#"foo->isChecked() ? QStringLiteral("foo") : QCoreApplication::translate("MyClass", "bar")"#
        );
    }
}
