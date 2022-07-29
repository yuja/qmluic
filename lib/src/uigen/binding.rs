use super::expr;
use super::objcode::{CallbackCode, ObjectCodeMap, PropertyCode, PropertyCodeKind};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::{ObjectNode, ObjectTree};
use crate::opcode::{BuiltinFunctionKind, BuiltinMethodKind};
use crate::qtname::{self, FileNameRules, UniqueNameGenerator};
use crate::tir;
use crate::typedexpr::DescribeType as _;
use crate::typemap::{Class, TypeKind, TypeSpace};
use itertools::Itertools as _;
use std::collections::HashMap;
use std::io;

/// C++ code to set up dynamic property bindings.
#[derive(Clone, Debug)]
pub struct UiSupportCode {
    self_class: String,
    root_class: String,
    ui_class: String,
    quote_includes: Vec<String>,
    bindings: Vec<CxxBinding>,
    callbacks: Vec<CxxCallback>,
}

impl UiSupportCode {
    pub(super) fn build(
        type_name: &str,
        file_name_rules: &FileNameRules,
        object_tree: &ObjectTree,
        object_code_maps: &[ObjectCodeMap],
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let quote_includes = vec![file_name_rules.type_name_to_ui_cxx_header_name(type_name)];

        let mut name_gen = UniqueNameGenerator::new();
        let binding_code_translator = CxxCodeBodyTranslator::new(
            object_tree.root().name(),
            type_name,
            CxxCodeReturnKind::Value,
        );
        let callback_code_translator = CxxCodeBodyTranslator::new(
            object_tree.root().name(),
            type_name,
            CxxCodeReturnKind::Void,
        );
        let mut bindings = Vec::new();
        let mut callbacks = Vec::new();
        for (obj_node, code_map) in object_tree.flat_iter().zip(object_code_maps) {
            // TODO: exclude pseudo node like QActionSeparator
            let receiver = binding_code_translator
                .format_named_object_ref(&tir::NamedObjectRef(obj_node.name().to_owned()));
            let dyn_props = code_map
                .properties()
                .iter()
                .filter(|(_, p)| !p.is_evaluated_constant())
                .sorted_by_key(|&(k, _)| k);
            bindings.extend(dyn_props.filter_map(|(_, property_code)| {
                let prefix = qtname::to_ascii_capitalized(obj_node.name())
                    + &qtname::to_ascii_capitalized(property_code.desc().name());
                let name = name_gen.generate(&prefix);
                let value_function = match property_code.kind() {
                    PropertyCodeKind::Expr(ty, code) => {
                        expr::verify_code_return_type(property_code.node(), code, ty, diagnostics)?;
                        CxxBindingValueFunction::Expr(CxxEvalExprFunction::build(
                            &binding_code_translator,
                            &name,
                            ty,
                            code,
                            diagnostics,
                        ))
                    }
                    PropertyCodeKind::GadgetMap(cls, map) => {
                        CxxBindingValueFunction::GadgetMap(CxxEvalGadgetMapFunction::build(
                            &binding_code_translator,
                            &mut name_gen,
                            &name,
                            cls,
                            map,
                            diagnostics,
                        ))
                    }
                    PropertyCodeKind::ObjectMap(..) => {
                        diagnostics.push(Diagnostic::error(
                            property_code.node().byte_range(),
                            "nested dynamic binding is not supported",
                        ));
                        return None;
                    }
                };
                let update = CxxUpdateBinding::build(property_code, value_function, diagnostics)?;
                Some(CxxBinding::new(name, &receiver, update))
            }));

            callbacks.extend(
                code_map
                    .callbacks()
                    .iter()
                    .sorted_by_key(|c| c.desc().name())
                    .map(|callback_code| {
                        let prefix = qtname::to_ascii_capitalized(obj_node.name())
                            + &qtname::to_ascii_capitalized(callback_code.desc().name());
                        let name = name_gen.generate(&prefix);
                        CxxCallback::build(&callback_code_translator, name, obj_node, callback_code)
                    }),
            );
        }

        UiSupportCode {
            self_class: type_name.to_owned(),
            root_class: object_tree.root().class().qualified_cxx_name().into(),
            ui_class: format!("Ui::{}", type_name),
            quote_includes,
            bindings,
            callbacks,
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
        self.write_binding_index(writer, "    ")?;
        self.write_binding_functions(writer, "    ")?;
        self.write_callback_functions(writer, "    ")?;
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
        for c in &self.callbacks {
            writeln!(writer, "{indent}    this->{}();", c.setup_function_name())?;
        }
        for b in &self.bindings {
            writeln!(writer, "{indent}    this->{}();", b.update_function_name())?;
        }
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }

    fn write_binding_index<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(writer, "{indent}enum class BindingIndex : unsigned {{")?;
        for b in &self.bindings {
            writeln!(writer, "{indent}    {},", b.name())?;
        }
        writeln!(writer, "{indent}}};")?;
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
            b.write_value_function(writer, indent)?;
        }
        Ok(())
    }

    fn write_callback_functions<W: io::Write>(
        &self,
        writer: &mut W,
        indent: &str,
    ) -> io::Result<()> {
        for c in &self.callbacks {
            c.write_setup_function(writer, indent)?;
            c.write_callback_function(writer, indent)?;
        }
        Ok(())
    }

    fn write_fields<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(writer, "{indent}{} *const root_;", self.root_class)?;
        writeln!(writer, "{indent}{} *const ui_;", self.ui_class)?;
        if !self.bindings.is_empty() {
            // don't emit 0-sized array
            writeln!(writer, "#ifndef QT_NO_DEBUG")?;
            writeln!(
                writer,
                "{indent}quint32 bindingGuard_[{}] = {{0}};",
                (self.bindings.len() + 31) / 32
            )?;
            writeln!(writer, "#endif")?;
        }
        writeln!(writer)
    }
}

/// C++ function and statements for dynamic property binding.
#[derive(Clone, Debug)]
struct CxxBinding {
    function_name_suffix: String,
    receiver: String,
    update: CxxUpdateBinding,
}

impl CxxBinding {
    fn new(
        function_name_suffix: String,
        receiver: impl Into<String>,
        update: CxxUpdateBinding,
    ) -> Self {
        CxxBinding {
            function_name_suffix,
            receiver: receiver.into(),
            update,
        }
    }

    fn name(&self) -> &str {
        &self.function_name_suffix
    }

    fn setup_function_name(&self) -> String {
        format!("setup{}", self.function_name_suffix)
    }

    fn update_function_name(&self) -> String {
        format!("update{}", self.function_name_suffix)
    }

    fn write_setup_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(writer, "{indent}void {}()", self.setup_function_name())?;
        writeln!(writer, "{indent}{{")?;
        for (sender, signal) in self.update.value_function.sender_signals().unique() {
            writeln!(
                writer,
                "{indent}    QObject::connect({}, &{}, this->root_, [this]() {{ this->{}(); }});",
                sender,
                signal,
                self.update_function_name()
            )?;
        }
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }

    fn write_update_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        let index = format!("static_cast<unsigned>(BindingIndex::{})", self.name());
        let guard = "this->bindingGuard_[index >> 5]";
        let mask = "(1U << (index & 0x1f))";
        writeln!(writer, "{indent}void {}()", self.update_function_name())?;
        writeln!(writer, "{indent}{{")?;
        writeln!(writer, "#ifndef QT_NO_DEBUG")?;
        writeln!(writer, "{indent}    constexpr unsigned index = {index};")?;
        writeln!(
            writer,
            "{indent}    Q_ASSERT_X(!({guard} & {mask}), __func__, {what:?});",
            what = "binding loop detected"
        )?;
        writeln!(writer, "{indent}    {guard} |= {mask};")?;
        writeln!(writer, "#endif")?;
        writeln!(
            writer,
            "{indent}    {};",
            self.update.format_expression(&self.receiver, "->")
        )?;
        writeln!(writer, "#ifndef QT_NO_DEBUG")?;
        writeln!(writer, "{indent}    {guard} &= ~{mask};")?;
        writeln!(writer, "#endif")?;
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }

    fn write_value_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        self.update.value_function.write_function(writer, indent)
    }
}

/// C++ expression to update dynamic property binding.
#[derive(Clone, Debug)]
struct CxxUpdateBinding {
    read: String,
    write: String,
    value_function: CxxBindingValueFunction,
}

impl CxxUpdateBinding {
    fn build(
        property_code: &PropertyCode,
        value_function: CxxBindingValueFunction,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let read = if let Some(f) = property_code.desc().read_func_name() {
            f.to_owned()
        } else {
            diagnostics.push(Diagnostic::error(
                property_code.binding_node().byte_range(),
                "not a readable property",
            ));
            return None;
        };
        let write = if let Some(f) = property_code.desc().write_func_name() {
            f.to_owned()
        } else {
            diagnostics.push(Diagnostic::error(
                property_code.binding_node().byte_range(),
                "not a writable property",
            ));
            return None;
        };
        Some(CxxUpdateBinding {
            read,
            write,
            value_function,
        })
    }

    fn format_expression(&self, receiver: &str, op: &str) -> String {
        match &self.value_function {
            CxxBindingValueFunction::Expr(f) => {
                format!(
                    "{}{}{}(this->{}())",
                    receiver,
                    op,
                    self.write,
                    f.function_name()
                )
            }
            CxxBindingValueFunction::GadgetMap(f) => {
                format!(
                    "{}{}{}(this->{}({}{}{}()))",
                    receiver,
                    op,
                    self.write,
                    f.function_name(),
                    receiver,
                    op,
                    self.read
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
enum CxxBindingValueFunction {
    Expr(CxxEvalExprFunction),
    GadgetMap(CxxEvalGadgetMapFunction),
}

impl CxxBindingValueFunction {
    fn sender_signals(&self) -> Box<dyn Iterator<Item = &(String, String)> + '_> {
        match self {
            CxxBindingValueFunction::Expr(f) => Box::new(f.sender_signals()),
            CxxBindingValueFunction::GadgetMap(f) => Box::new(f.sender_signals()),
        }
    }

    fn write_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        match self {
            CxxBindingValueFunction::Expr(f) => f.write_function(writer, indent),
            CxxBindingValueFunction::GadgetMap(f) => f.write_function(writer, indent),
        }
    }
}

/// C++ function to evaluate binding expression.
#[derive(Clone, Debug)]
struct CxxEvalExprFunction {
    name: String,
    value_type: String,
    sender_signals: Vec<(String, String)>,
    body: Vec<u8>,
}

impl CxxEvalExprFunction {
    fn build(
        code_translator: &CxxCodeBodyTranslator,
        name: impl Into<String>,
        value_ty: &TypeKind,
        code: &tir::CodeBody,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let mut code = code.clone();
        let sender_signals = code_translator.collect_sender_signals(&mut code, diagnostics);
        let mut body = Vec::new();
        code_translator
            .translate(&mut body, &code)
            .expect("write to bytes shouldn't fail");
        CxxEvalExprFunction {
            name: name.into(),
            value_type: value_ty.qualified_cxx_name().into(),
            sender_signals,
            body,
        }
    }

    fn function_name(&self) -> String {
        format!("eval{}", self.name)
    }

    fn sender_signals(&self) -> impl Iterator<Item = &(String, String)> {
        self.sender_signals.iter()
    }

    fn write_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(
            writer,
            "{indent}{} {}() const",
            self.value_type,
            self.function_name()
        )?;
        writeln!(writer, "{indent}{{")?;
        writer.write_all(&self.body)?;
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }
}

/// C++ function to evaluate gadget map binding expressions.
#[derive(Clone, Debug)]
struct CxxEvalGadgetMapFunction {
    name: String,
    value_type: String,
    bindings: Vec<CxxUpdateBinding>,
}

impl CxxEvalGadgetMapFunction {
    fn build(
        code_translator: &CxxCodeBodyTranslator,
        name_gen: &mut UniqueNameGenerator,
        name: impl Into<String>,
        value_cls: &Class,
        properties_map: &HashMap<&str, PropertyCode>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let name = name.into();
        let bindings = properties_map
            .iter()
            .sorted_by_key(|&(k, _)| k)
            .filter_map(|(_, property_code)| {
                let sub_name = name_gen.generate(
                    name.clone() + &qtname::to_ascii_capitalized(property_code.desc().name()),
                );
                let value_function = match property_code.kind() {
                    PropertyCodeKind::Expr(ty, code) => {
                        expr::verify_code_return_type(property_code.node(), code, ty, diagnostics)?;
                        CxxBindingValueFunction::Expr(CxxEvalExprFunction::build(
                            code_translator,
                            sub_name,
                            ty,
                            code,
                            diagnostics,
                        ))
                    }
                    PropertyCodeKind::GadgetMap(cls, map) => {
                        CxxBindingValueFunction::GadgetMap(CxxEvalGadgetMapFunction::build(
                            code_translator,
                            name_gen,
                            sub_name,
                            cls,
                            map,
                            diagnostics,
                        ))
                    }
                    PropertyCodeKind::ObjectMap(..) => {
                        diagnostics.push(Diagnostic::error(
                            property_code.node().byte_range(),
                            "nested dynamic binding is not supported",
                        ));
                        return None;
                    }
                };
                CxxUpdateBinding::build(property_code, value_function, diagnostics)
            })
            .collect();

        CxxEvalGadgetMapFunction {
            name,
            value_type: value_cls.qualified_cxx_name().into(),
            bindings,
        }
    }

    fn function_name(&self) -> String {
        format!("eval{}", self.name)
    }

    fn sender_signals(&self) -> impl Iterator<Item = &(String, String)> {
        self.bindings
            .iter()
            .flat_map(|b| b.value_function.sender_signals())
    }

    fn write_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(
            writer,
            "{indent}{} {}({} a) const",
            self.value_type,
            self.function_name(),
            self.value_type
        )?;
        writeln!(writer, "{indent}{{")?;
        for b in &self.bindings {
            writeln!(writer, "{indent}    {};", b.format_expression("a", "."))?;
        }
        writeln!(writer, "{indent}    return a;")?;
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)?;

        for b in &self.bindings {
            b.value_function.write_function(writer, indent)?;
        }
        Ok(())
    }
}

/// C++ function and statements for callback connection.
#[derive(Clone, Debug)]
struct CxxCallback {
    name: String,
    sender: String,
    signal: String,
    callback_function_body: Vec<u8>,
}

impl CxxCallback {
    fn build(
        code_translator: &CxxCodeBodyTranslator,
        name: String,
        obj_node: ObjectNode,
        callback_code: &CallbackCode,
    ) -> Self {
        let sender = code_translator
            .format_named_object_ref(&tir::NamedObjectRef(obj_node.name().to_owned()));
        let signal = format!(
            "{}::{}",
            callback_code.desc().object_class().qualified_cxx_name(),
            callback_code.desc().name()
        );
        let mut callback_function_body = Vec::new();
        code_translator
            .translate(&mut callback_function_body, callback_code.code())
            .expect("write to bytes shouldn't fail");
        CxxCallback {
            name,
            sender,
            signal,
            callback_function_body,
        }
    }

    fn setup_function_name(&self) -> String {
        format!("setup{}", self.name)
    }

    fn callback_function_name(&self) -> String {
        format!("on{}", self.name)
    }

    fn write_setup_function<W: io::Write>(&self, writer: &mut W, indent: &str) -> io::Result<()> {
        writeln!(writer, "{indent}void {}()", self.setup_function_name())?;
        writeln!(writer, "{indent}{{")?;
        writeln!(
            writer,
            "{indent}    QObject::connect({}, &{}, this->root_, [this]() {{ this->{}(); }});",
            self.sender,
            self.signal,
            self.callback_function_name()
        )?;
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }

    fn write_callback_function<W: io::Write>(
        &self,
        writer: &mut W,
        indent: &str,
    ) -> io::Result<()> {
        writeln!(writer, "{indent}void {}()", self.callback_function_name())?;
        writeln!(writer, "{indent}{{")?;
        writer.write_all(&self.callback_function_body)?;
        writeln!(writer, "{indent}}}")?;
        writeln!(writer)
    }
}

#[derive(Clone, Debug)]
struct CxxCodeBodyTranslator {
    root_object_name: tir::NamedObjectRef,
    tr_context: String,
    return_kind: CxxCodeReturnKind,
    label_indent: String,
    body_indent: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CxxCodeReturnKind {
    Value,
    Void,
}

impl CxxCodeBodyTranslator {
    fn new(
        root_object_name: impl Into<String>,
        tr_context: impl Into<String>,
        return_kind: CxxCodeReturnKind,
    ) -> Self {
        CxxCodeBodyTranslator {
            root_object_name: tir::NamedObjectRef(root_object_name.into()),
            tr_context: tr_context.into(),
            return_kind,
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
        code: &mut tir::CodeBody,
        diagnostics: &mut Diagnostics,
    ) -> Vec<(String, String)> {
        tir::analyze_code_property_dependency(code, diagnostics);
        code.static_property_deps
            .iter()
            .map(|(obj, prop)| {
                let sender = self.format_named_object_ref(obj);
                let class = prop.object_class().qualified_cxx_name();
                let signal = prop
                    .notify_signal_name()
                    .expect("static dependency property must be observable");
                (sender, format!("{class}::{signal}"))
            })
            .collect()
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
        use tir::{Operand, Terminator};
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
            Terminator::Return(Operand::Void(_)) => writeln!(w, "{}return;", self.body_indent)?,
            Terminator::Return(x) => match self.return_kind {
                CxxCodeReturnKind::Value => {
                    writeln!(w, "{}return {};", self.body_indent, self.format_operand(x))?;
                }
                CxxCodeReturnKind::Void => {
                    writeln!(
                        w,
                        "{}static_cast<void>({});",
                        self.body_indent,
                        self.format_operand(x)
                    )?;
                    writeln!(w, "{}return;", self.body_indent)?;
                }
            },
            Terminator::Unreachable => writeln!(w, "{}Q_UNREACHABLE();", self.body_indent)?,
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
            Statement::Exec(r) => writeln!(w, "{}{};", self.body_indent, self.format_rvalue(r)),
            Statement::ObserveProperty(..) => todo!(),
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
                    "{}{}arg({})",
                    self.format_operand(obj),
                    member_access_op(obj),
                    args.iter().map(|a| self.format_operand(a)).join(", ")
                ),
            },
            Rvalue::CallMethod(obj, meth, args) => {
                format!(
                    "{}{}{}({})",
                    self.format_operand(obj),
                    member_access_op(obj),
                    meth.name(),
                    args.iter().map(|a| self.format_operand(a)).join(", ")
                )
            }
            Rvalue::ReadProperty(obj, prop) => {
                format!(
                    "{}{}{}()",
                    self.format_operand(obj),
                    member_access_op(obj),
                    prop.read_func_name()
                        .expect("unreadable property must be rejected by TIR builder")
                )
            }
            Rvalue::WriteProperty(obj, prop, r) => {
                format!(
                    "{}{}{}({})",
                    self.format_operand(obj),
                    member_access_op(obj),
                    prop.write_func_name()
                        .expect("unwritable property must be rejected by TIR builder"),
                    self.format_operand(r)
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
            Operand::Void(_) => "void()".to_owned(),
        }
    }
}

fn member_access_op(a: &tir::Operand) -> &'static str {
    if a.type_desc().is_pointer() {
        "->"
    } else {
        "."
    }
}
