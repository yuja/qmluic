use super::property::{DynamicPropertiesMap, PropertyDescDynamicExpression, WithNode};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::{ObjectNode, ObjectTree};
use crate::qtname::{self, FileNameRules, UniqueNameGenerator};
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

        let mut name_gen = UniqueNameGenerator::new();
        let code_translator = CxxCodeBodyTranslator::new(object_tree.root().name(), type_name);
        let mut bindings = Vec::new();
        for (obj_node, properties_map) in object_tree.flat_iter().zip(object_properties) {
            // TODO: exclude pseudo node like QActionSeparator
            bindings.extend(properties_map.iter().sorted_by_key(|&(k, _)| k).filter_map(
                |(_, v)| {
                    let prefix = qtname::to_ascii_capitalized(obj_node.name())
                        + &qtname::to_ascii_capitalized(v.data().desc.name());
                    let name = name_gen.generate(&prefix);
                    BindingCode::build(&code_translator, name, obj_node, v, diagnostics)
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
            writeln!(writer, "{indent}    this->{}();", b.setup_function_name())?;
        }
        for b in &self.bindings {
            writeln!(writer, "{indent}    this->{}();", b.update_function_name())?;
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
    function_name_suffix: String,
    value_type: String,
    sender_signals: Vec<(String, String)>,
    write_method: String,
    eval_function_body: Vec<u8>,
}

impl BindingCode {
    fn build(
        code_translator: &CxxCodeBodyTranslator,
        function_name_suffix: String,
        obj_node: ObjectNode,
        prop: &WithNode<PropertyDescDynamicExpression>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let sender_signals = code_translator.collect_sender_signals(&prop.data().code, diagnostics);
        let write_method = if let Some(f) = prop.data().desc.write_func_name() {
            format!(
                "{}->{}",
                code_translator
                    .format_named_object_ref(&tir::NamedObjectRef(obj_node.name().to_owned())),
                f,
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
            .translate(&mut eval_function_body, &prop.data().code)
            .expect("write to bytes shouldn't fail");
        Some(BindingCode {
            function_name_suffix,
            value_type,
            sender_signals,
            write_method,
            eval_function_body,
        })
    }

    fn setup_function_name(&self) -> String {
        format!("setup{}", self.function_name_suffix)
    }

    fn update_function_name(&self) -> String {
        format!("update{}", self.function_name_suffix)
    }

    fn eval_function_name(&self) -> String {
        format!("eval{}", self.function_name_suffix)
    }

    fn write_setup_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(writer, "{indent}void {}()", self.setup_function_name())?;
        writeln!(writer, "{indent}{{")?;
        for (sender, signal) in self.sender_signals.iter().unique() {
            writeln!(
                writer,
                "{indent}    QObject::connect({}, &{}, root_, [this]() {{ this->{}(); }});",
                sender,
                signal,
                self.update_function_name()
            )?;
        }
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }

    fn write_update_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(writer, "{indent}void {}()", self.update_function_name())?;
        writeln!(writer, "{indent}{{")?;
        // TODO: insert cycle detector
        writeln!(
            writer,
            "{indent}    {}(this->{}());",
            self.write_method,
            self.eval_function_name()
        )?;
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }

    fn write_eval_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(
            writer,
            "{indent}{} {}() const",
            self.value_type,
            self.eval_function_name()
        )?;
        writeln!(writer, "{indent}{{")?;
        writer.write_all(&self.eval_function_body)?;
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
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

    fn collect_sender_signals(
        &self,
        code: &tir::CodeBody,
        diagnostics: &mut Diagnostics,
    ) -> Vec<(String, String)> {
        use tir::{Operand, Rvalue, Statement};
        let mut sender_signals = Vec::new();
        for s in code.basic_blocks.iter().flat_map(|b| &b.statements) {
            match s {
                Statement::Assign(_, r) => {
                    if let Rvalue::ReadProperty(a, prop) = r {
                        match a {
                            Operand::NamedObject(obj) => {
                                if let Some(f) = prop.notify_signal_name() {
                                    let sender = self.format_named_object_ref(&obj.name);
                                    let signal = format!(
                                        "{}::{}",
                                        prop.object_class().qualified_cxx_name(),
                                        f
                                    );
                                    sender_signals.push((sender, signal));
                                } else {
                                    diagnostics.push(Diagnostic::error(
                                        a.byte_range(),
                                        format!("unobservable property: {}", prop.name()),
                                    ));
                                }
                            }
                            Operand::Local(_) => {
                                diagnostics.push(Diagnostic::error(
                                    a.byte_range(),
                                    "chained object property is not supported",
                                ));
                            }
                            Operand::Constant(_) | Operand::EnumVariant(_) => {
                                panic!("invald read_property: {r:?}");
                            }
                        }
                    }
                }
            }
        }
        sender_signals
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
            Rvalue::UnaryOp(op, a) => format!("{}{}", op, self.format_operand(a)),
            Rvalue::BinaryOp(op, l, r) => format!(
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
            Operand::Constant(x) => match &x.value {
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
