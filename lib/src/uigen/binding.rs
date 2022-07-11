use super::property::{DynamicPropertiesMap, PropertyDescDynamicExpression, WithNode};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::{ObjectNode, ObjectTree};
use crate::qtname::FileNameRules;
use crate::tir;
use crate::typedexpr::{BuiltinFunctionKind, BuiltinMethodKind};
use crate::typemap::TypeSpace;
use itertools::Itertools as _;
use std::io;

/// C++ code to set up dynamic property bindings.
#[derive(Clone, Debug)]
pub struct UiSupportCode {
    self_class: String,
    root_class: String,
    ui_class: String,
    quote_includes: Vec<String>,
    bindings: Vec<BindingCode>,
}

impl UiSupportCode {
    pub(super) fn build(
        type_name: &str,
        file_name_rules: &FileNameRules,
        object_tree: &ObjectTree,
        object_properties: &[DynamicPropertiesMap],
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let quote_includes = vec![file_name_rules.type_name_to_ui_cxx_header_name(type_name)];

        let code_translator = CxxCodeBodyTranslator::new(object_tree.root().name(), type_name);
        let mut bindings = Vec::new();
        for (obj_node, properties_map) in object_tree.flat_iter().zip(object_properties) {
            // TODO: exclude pseudo node like QActionSeparator
            bindings.extend(properties_map.iter().sorted_by_key(|&(k, _)| k).filter_map(
                |(_, v)| {
                    BindingCode::build(&code_translator, object_tree, obj_node, v, diagnostics)
                },
            ));
        }

        UiSupportCode {
            self_class: type_name.to_owned(),
            root_class: object_tree.root().class().qualified_cxx_name().into(),
            ui_class: format!("Ui::{}", type_name),
            quote_includes,
            bindings,
        }
    }

    /// Writes C++ header content.
    pub fn write_header<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        // TODO: code style options, factor out code generator?
        writeln!(writer, "#pragma once")?;
        for f in &self.quote_includes {
            writeln!(writer, r###"#include "{f}""###)?;
        }
        writeln!(writer)?;
        writeln!(writer, "namespace UiSupport {{")?;

        writeln!(writer, "class {}", self.self_class)?;
        writeln!(writer, "{{")?;
        writeln!(writer, "public:")?;
        self.write_constructor(writer, "    ")?;
        self.write_setup_function(writer, "    ")?;
        writeln!(writer, "private:")?;
        self.write_binding_functions(writer, "    ")?;
        self.write_fields(writer, "    ")?;
        writeln!(writer, "}};")?;

        writeln!(writer, "}} // namespace UiSupport")
    }

    fn write_constructor<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(
            writer,
            "{indent}{}({} *root, {} *ui): root_(root), ui_(ui) {{}}",
            self.self_class, self.root_class, self.ui_class,
        )?;
        writeln!(writer)
    }

    fn write_setup_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(writer, "{indent}void setup()")?;
        writeln!(writer, "{indent}{{")?;
        for b in &self.bindings {
            writeln!(writer, "{indent}    this->{}();", b.setup_function_name)?;
        }
        for b in &self.bindings {
            writeln!(writer, "{indent}    this->{}();", b.update_function_name)?;
        }
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }

    fn write_binding_functions<W: io::Write>(
        &self,
        writer: &mut W,
        indent: &str,
    ) -> io::Result<()> {
        for b in &self.bindings {
            b.write_setup_function(writer, indent)?;
            b.write_update_function(writer, indent)?;
            b.write_eval_function(writer, indent)?;
        }
        Ok(())
    }

    fn write_fields<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(writer, "{indent}{} *const root_;", self.root_class)?;
        writeln!(writer, "{indent}{} *const ui_;", self.ui_class)?;
        writeln!(writer)
    }
}

/// C++ function and statements for dynamic property binding.
#[derive(Clone, Debug)]
struct BindingCode {
    setup_function_name: String,
    update_function_name: String,
    eval_function_name: String,
    value_type: String,
    sender_signals: Vec<(CxxFieldRef, String)>,
    write_expr: String,
    eval_function_body: Vec<u8>,
}

impl BindingCode {
    fn build(
        code_translator: &CxxCodeBodyTranslator,
        object_tree: &ObjectTree,
        obj_node: ObjectNode,
        prop: &WithNode<PropertyDescDynamicExpression>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        // TODO: get around name conflicts
        let setup_function_name = format!("setup_{}_{}", obj_node.name(), prop.data().desc.name());
        let update_function_name =
            format!("update_{}_{}", obj_node.name(), prop.data().desc.name());
        let eval_function_name = format!("eval_{}_{}", obj_node.name(), prop.data().desc.name());
        let sender_signals = prop
            .data()
            .value
            .property_deps
            .iter()
            .filter_map(|(o, p)| {
                if let Some(f) = p.notify_signal_name() {
                    // TODO: 'o' is theoretically an expression, but we do need a name
                    let sender = CxxFieldRef::build(object_tree, o);
                    let signal = format!("{}::{}", p.object_class().qualified_cxx_name(), f);
                    Some((sender, signal))
                } else {
                    diagnostics.push(Diagnostic::error(
                        prop.node().byte_range(),
                        format!("unobservable property: {}.{}", o, p.name()),
                    ));
                    None
                }
            })
            .collect();
        let write_expr = if let Some(f) = prop.data().desc.write_func_name() {
            format!(
                "{}->{}(this->{}())",
                code_translator
                    .format_named_object_ref(&tir::NamedObjectRef(obj_node.name().to_owned())),
                f,
                eval_function_name
            )
        } else {
            diagnostics.push(Diagnostic::error(
                prop.binding_node().byte_range(),
                "not a writable property",
            ));
            return None;
        };
        let value_type: String = match prop.data().desc.value_type() {
            Ok(ty) => ty.qualified_cxx_name().into(),
            Err(e) => {
                diagnostics.push(Diagnostic::error(
                    prop.binding_node().byte_range(),
                    format!("property type resolution failed: {}", e),
                ));
                return None;
            }
        };
        let mut eval_function_body = Vec::new();
        code_translator
            .translate(&mut eval_function_body, &prop.data().value.code)
            .expect("write to bytes shouldn't fail");
        Some(BindingCode {
            setup_function_name,
            update_function_name,
            eval_function_name,
            value_type,
            sender_signals,
            write_expr,
            eval_function_body,
        })
    }

    fn write_setup_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(writer, "{indent}void {}()", self.setup_function_name)?;
        writeln!(writer, "{indent}{{")?;
        let indent_body = indent.to_owned() + "    ";
        for field in self.sender_signals.iter().map(|(s, _)| s).unique() {
            field.write_local_variable(writer, &indent_body)?;
        }
        writeln!(writer)?;
        for (sender, signal) in self.sender_signals.iter().unique() {
            writeln!(
                writer,
                "{indent_body}QObject::connect({}, &{}, root_, [this]() {{ this->{}(); }});",
                sender.name(),
                signal,
                self.update_function_name
            )?;
        }
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }

    fn write_update_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(writer, "{indent}void {}()", self.update_function_name)?;
        writeln!(writer, "{indent}{{")?;
        // TODO: insert cycle detector
        writeln!(writer, "{indent}    {};", self.write_expr)?;
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }

    fn write_eval_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(
            writer,
            "{indent}{} {}() const",
            self.value_type, self.eval_function_name
        )?;
        writeln!(writer, "{indent}{{")?;
        writer.write_all(&self.eval_function_body)?;
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum CxxFieldRef {
    /// Root object: `{name} = this->root_`
    Root(String),
    /// Ui field: `{name} = this->ui_->{name}`
    Ui(String),
}

impl CxxFieldRef {
    fn build(object_tree: &ObjectTree, name: &str) -> CxxFieldRef {
        if name == object_tree.root().name() {
            CxxFieldRef::Root(name.to_owned())
        } else {
            CxxFieldRef::Ui(name.to_owned())
        }
    }

    fn name(&self) -> &str {
        use CxxFieldRef::*;
        match self {
            Root(name) => name,
            Ui(name) => name,
        }
    }

    fn write_local_variable<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        use CxxFieldRef::*;
        match self {
            Root(name) => writeln!(writer, "{indent}auto *const {name} = this->root_;"),
            Ui(name) => writeln!(writer, "{indent}auto *const {name} = this->ui_->{name};"),
        }
    }
}

#[derive(Clone, Debug)]
struct CxxCodeBodyTranslator {
    root_object_name: tir::NamedObjectRef,
    tr_context: String,
    label_indent: String,
    body_indent: String,
}

impl CxxCodeBodyTranslator {
    fn new(root_object_name: impl Into<String>, tr_context: impl Into<String>) -> Self {
        CxxCodeBodyTranslator {
            root_object_name: tir::NamedObjectRef(root_object_name.into()),
            tr_context: tr_context.into(),
            label_indent: " ".repeat(4),
            body_indent: " ".repeat(8),
        }
    }

    fn translate<W: io::Write>(&self, w: &mut W, code: &tir::CodeBody) -> io::Result<()> {
        self.write_locals(w, &code.locals)?;
        for (i, b) in code.basic_blocks.iter().enumerate() {
            writeln!(
                w,
                "{}{}:",
                self.label_indent,
                self.format_basic_block_ref(tir::BasicBlockRef(i))
            )?;
            self.write_basic_block(w, b)?;
        }
        Ok(())
    }

    fn write_locals<W: io::Write>(&self, w: &mut W, locals: &[tir::Local]) -> io::Result<()> {
        for a in locals {
            writeln!(
                w,
                "{}{} {};",
                self.body_indent,
                a.ty.qualified_cxx_name(),
                self.format_local_ref(a.name)
            )?;
        }
        Ok(())
    }

    fn write_basic_block<W: io::Write>(
        &self,
        w: &mut W,
        block: &tir::BasicBlock,
    ) -> io::Result<()> {
        use tir::Terminator;
        for s in &block.statements {
            self.write_statement(w, s)?;
        }
        match block.terminator() {
            Terminator::Br(x) => writeln!(
                w,
                "{}goto {};",
                self.body_indent,
                self.format_basic_block_ref(*x)
            )?,
            Terminator::BrCond(x, y, z) => {
                writeln!(w, "{}if ({})", self.body_indent, self.format_operand(x))?;
                writeln!(
                    w,
                    "{}    goto {};",
                    self.body_indent,
                    self.format_basic_block_ref(*y)
                )?;
                writeln!(w, "{}else", self.body_indent)?;
                writeln!(
                    w,
                    "{}    goto {};",
                    self.body_indent,
                    self.format_basic_block_ref(*z)
                )?;
            }
            Terminator::Return(x) => {
                writeln!(w, "{}return {};", self.body_indent, self.format_operand(x))?
            }
        }
        Ok(())
    }

    fn write_statement<W: io::Write>(&self, w: &mut W, stmt: &tir::Statement) -> io::Result<()> {
        use tir::Statement;
        match stmt {
            Statement::Assign(l, r) => writeln!(
                w,
                "{}{} = {};",
                self.body_indent,
                self.format_local_ref(*l),
                self.format_rvalue(r)
            ),
        }
    }

    fn format_basic_block_ref(&self, r: tir::BasicBlockRef) -> String {
        format!("b{}", r.0)
    }

    fn format_local_ref(&self, r: tir::LocalRef) -> String {
        format!("a{}", r.0)
    }

    fn format_named_object_ref(&self, r: &tir::NamedObjectRef) -> String {
        if r == &self.root_object_name {
            "this->root_".to_owned()
        } else {
            format!("this->ui_->{}", r.0)
        }
    }

    fn format_rvalue(&self, rv: &tir::Rvalue) -> String {
        use tir::Rvalue;
        match rv {
            Rvalue::Copy(a) => self.format_operand(a),
            Rvalue::UnaryArithOp(op, a) => format!("{}{}", op, self.format_operand(a)),
            Rvalue::UnaryBitwiseOp(op, a) => format!("{}{}", op, self.format_operand(a)),
            Rvalue::UnaryLogicalOp(op, a) => format!("{}{}", op, self.format_operand(a)),
            Rvalue::BinaryArithOp(op, l, r) => format!(
                "{} {} {}",
                self.format_operand(l),
                op,
                self.format_operand(r)
            ),
            Rvalue::BinaryBitwiseOp(op, l, r) => format!(
                "{} {} {}",
                self.format_operand(l),
                op,
                self.format_operand(r)
            ),
            Rvalue::BinaryLogicalOp(op, l, r) => format!(
                "{} {} {}",
                self.format_operand(l),
                op,
                self.format_operand(r)
            ),
            Rvalue::ComparisonOp(op, l, r) => format!(
                "{} {} {}",
                self.format_operand(l),
                op,
                self.format_operand(r)
            ),
            Rvalue::CallBuiltinFunction(f, args) => match f {
                BuiltinFunctionKind::Tr => format!(
                    "QCoreApplication::translate({:?}, {})",
                    self.tr_context,
                    args.iter().map(|a| self.format_operand(a)).join(", ")
                ),
            },
            Rvalue::CallBuiltinMethod(obj, f, args) => match f {
                BuiltinMethodKind::Arg => format!(
                    "{}.arg({})",
                    self.format_operand(obj),
                    args.iter().map(|a| self.format_operand(a)).join(", ")
                ),
            },
            Rvalue::ReadProperty(obj, prop) => {
                format!(
                    "{}->{}()",
                    self.format_operand(obj),
                    prop.read_func_name()
                        .expect("unreadable property must be rejected by TIR builder")
                )
            }
            Rvalue::MakeList(xs) => {
                format!(
                    "{{{}}}",
                    xs.iter().map(|a| self.format_operand(a)).join(", ")
                )
            }
        }
    }

    fn format_operand(&self, a: &tir::Operand) -> String {
        use tir::{ConstantValue, Operand};
        match a {
            Operand::Constant(x) => match x {
                ConstantValue::Bool(v) => {
                    if *v {
                        "true".to_owned()
                    } else {
                        "false".to_owned()
                    }
                }
                ConstantValue::Integer(v) => v.to_string(),
                ConstantValue::Float(v) => format!("{v:e}"),
                ConstantValue::CString(v) => format!("{v:?}"), // TODO: escape per C spec)
                ConstantValue::QString(v) => format!("QStringLiteral({v:?})"),
                ConstantValue::EmptyList => "{}".to_owned(),
            },
            Operand::EnumVariant(x) => x.cxx_expression(),
            Operand::Local(x) => self.format_local_ref(x.name),
            Operand::NamedObject(x) => self.format_named_object_ref(&x.name),
        }
    }
}
