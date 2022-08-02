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
use std::io::{self, Write as _};

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
        let mut w = CodeWriter::new(writer);
        writeln!(w, "#pragma once")?;
        for f in &self.quote_includes {
            writeln!(w, r###"#include "{f}""###)?;
        }
        writeln!(w)?;
        writeln!(w, "namespace UiSupport {{")?;

        writeln!(w, "class {}", self.self_class)?;
        writeln!(w, "{{")?;
        writeln!(w, "public:")?;
        self.write_constructor(&mut w.indented())?;
        self.write_setup_function(&mut w.indented())?;
        writeln!(w, "private:")?;
        self.write_binding_index(&mut w.indented())?;
        self.write_binding_functions(&mut w.indented())?;
        self.write_callback_functions(&mut w.indented())?;
        self.write_fields(&mut w.indented())?;
        writeln!(w, "}};")?;

        writeln!(w, "}} // namespace UiSupport")
    }

    fn write_constructor<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        writeln!(
            w,
            "{}({} *root, {} *ui): root_(root), ui_(ui) {{}}",
            self.self_class, self.root_class, self.ui_class,
        )?;
        writeln!(w)
    }

    fn write_setup_function<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        writeln!(w, "void setup()")?;
        writeln!(w, "{{")?;
        w.indent();
        for b in &self.bindings {
            writeln!(w, "this->{}();", b.setup_function_name())?;
        }
        for c in &self.callbacks {
            writeln!(w, "this->{}();", c.setup_function_name())?;
        }
        for b in &self.bindings {
            writeln!(w, "this->{}();", b.update_function_name())?;
        }
        w.unindent();
        writeln!(w, "}}")?;
        writeln!(w)
    }

    fn write_binding_index<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        writeln!(w, "enum class BindingIndex : unsigned {{")?;
        w.indent();
        for b in &self.bindings {
            writeln!(w, "{},", b.name())?;
        }
        w.unindent();
        writeln!(w, "}};")?;
        writeln!(w)
    }

    fn write_binding_functions<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        for b in &self.bindings {
            b.write_setup_function(w)?;
            b.write_update_function(w)?;
            b.write_value_function(w)?;
        }
        Ok(())
    }

    fn write_callback_functions<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        for c in &self.callbacks {
            c.write_setup_function(w)?;
            c.write_callback_function(w)?;
        }
        Ok(())
    }

    fn write_fields<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        writeln!(w, "struct PropertyObserver")?;
        writeln!(w, "{{")?;
        w.indent();
        writeln!(w, "QMetaObject::Connection connection;")?;
        writeln!(w, "QObject *object = nullptr;")?;
        w.unindent();
        writeln!(w, "}};")?;
        writeln!(w)?;

        writeln!(w, "{} *const root_;", self.root_class)?;
        writeln!(w, "{} *const ui_;", self.ui_class)?;
        for b in &self.bindings {
            b.write_field(w)?;
        }
        if !self.bindings.is_empty() {
            // don't emit 0-sized array
            writeln!(w, "#ifndef QT_NO_DEBUG")?;
            writeln!(
                w,
                "quint32 bindingGuard_[{}] = {{0}};",
                (self.bindings.len() + 31) / 32
            )?;
            writeln!(w, "#endif")?;
        }
        writeln!(w)
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

    fn write_setup_function<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        writeln!(w, "void {}()", self.setup_function_name())?;
        writeln!(w, "{{")?;
        w.indent();
        for (sender, signal) in self.update.value_function.sender_signals().unique() {
            writeln!(
                w,
                "QObject::connect({}, &{}, this->root_, [this]() {{ this->{}(); }});",
                sender,
                signal,
                self.update_function_name()
            )?;
        }
        w.unindent();
        writeln!(w, "}}")?;
        writeln!(w)
    }

    fn write_update_function<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        let index = format!("static_cast<unsigned>(BindingIndex::{})", self.name());
        let guard = "this->bindingGuard_[index >> 5]";
        let mask = "(1U << (index & 0x1f))";
        writeln!(w, "void {}()", self.update_function_name())?;
        writeln!(w, "{{")?;
        writeln!(w, "#ifndef QT_NO_DEBUG")?;
        w.indent();
        writeln!(w, "constexpr unsigned index = {index};")?;
        writeln!(
            w,
            "Q_ASSERT_X(!({guard} & {mask}), __func__, {what:?});",
            what = "binding loop detected"
        )?;
        writeln!(w, "{guard} |= {mask};")?;
        writeln!(w, "#endif")?;
        writeln!(
            w,
            "{};",
            self.update.format_expression(&self.receiver, "->")
        )?;
        writeln!(w, "#ifndef QT_NO_DEBUG")?;
        writeln!(w, "{guard} &= ~{mask};")?;
        writeln!(w, "#endif")?;
        w.unindent();
        writeln!(w, "}}")?;
        writeln!(w)
    }

    fn write_value_function<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        self.update
            .value_function
            .write_function(w, &self.update_function_name())
    }

    fn write_field<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        self.update.value_function.write_field(w)
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

    fn write_function<W: io::Write>(
        &self,
        w: &mut CodeWriter<W>,
        update_function_name: &str,
    ) -> io::Result<()> {
        match self {
            CxxBindingValueFunction::Expr(f) => f.write_function(w, update_function_name),
            CxxBindingValueFunction::GadgetMap(f) => f.write_function(w, update_function_name),
        }
    }

    fn write_field<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        match self {
            CxxBindingValueFunction::Expr(f) => f.write_field(w),
            CxxBindingValueFunction::GadgetMap(f) => f.write_field(w),
        }
    }
}

/// C++ function to evaluate binding expression.
#[derive(Clone, Debug)]
struct CxxEvalExprFunction {
    name: String,
    value_type: String,
    sender_signals: Vec<(String, String)>,
    property_observer_count: usize,
    body: Vec<u8>,
}

impl CxxEvalExprFunction {
    fn build(
        code_translator: &CxxCodeBodyTranslator,
        name: impl Into<String>,
        value_ty: &TypeKind,
        code: &tir::CodeBody,
    ) -> Self {
        let sender_signals = code
            .static_property_deps
            .iter()
            .map(|(obj, prop)| {
                let sender = code_translator.format_named_object_ref(obj);
                let class = prop.object_class().qualified_cxx_name();
                let signal = prop
                    .notify_signal_name()
                    .expect("static dependency property must be observable");
                (sender, format!("{class}::{signal}"))
            })
            .collect();
        assert_eq!(code.parameter_count, 0);
        let mut body = Vec::new();
        let mut writer = CodeWriter::new(&mut body);
        writer.set_indent_level(1);
        code_translator
            .translate(&mut writer, code)
            .expect("write to bytes shouldn't fail");
        CxxEvalExprFunction {
            name: name.into(),
            value_type: value_ty.qualified_cxx_name().into(),
            sender_signals,
            property_observer_count: code.property_observer_count,
            body,
        }
    }

    fn function_name(&self) -> String {
        format!("eval{}", self.name)
    }

    fn property_observer_name(&self) -> String {
        format!("observed{}_", self.name)
    }

    fn sender_signals(&self) -> impl Iterator<Item = &(String, String)> {
        self.sender_signals.iter()
    }

    fn write_function<W: io::Write>(
        &self,
        w: &mut CodeWriter<W>,
        update_function_name: &str,
    ) -> io::Result<()> {
        writeln!(w, "{} {}()", self.value_type, self.function_name())?;
        writeln!(w, "{{")?;
        if self.property_observer_count > 0 {
            let mut w = w.indented();
            writeln!(w, "auto &observed = {};", self.property_observer_name())?;
            writeln!(
                w,
                "const auto update = [this]() {{ this->{}(); }};",
                update_function_name
            )?;
        }
        w.get_mut().write_all(&self.body)?;
        writeln!(w, "}}")?;
        writeln!(w)
    }

    fn write_field<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        if self.property_observer_count > 0 {
            // don't emit 0-sized array
            writeln!(
                w,
                "PropertyObserver {}[{}];",
                self.property_observer_name(),
                self.property_observer_count
            )?;
        }
        Ok(())
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

    fn write_function<W: io::Write>(
        &self,
        w: &mut CodeWriter<W>,
        update_function_name: &str,
    ) -> io::Result<()> {
        writeln!(
            w,
            "{} {}({} a)",
            self.value_type,
            self.function_name(),
            self.value_type
        )?;
        writeln!(w, "{{")?;
        w.indent();
        for b in &self.bindings {
            writeln!(w, "{};", b.format_expression("a", "."))?;
        }
        writeln!(w, "return a;")?;
        w.unindent();
        writeln!(w, "}}")?;
        writeln!(w)?;

        for b in &self.bindings {
            b.value_function.write_function(w, update_function_name)?;
        }
        Ok(())
    }

    fn write_field<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        for b in &self.bindings {
            b.value_function.write_field(w)?;
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
        let code = callback_code.code();
        assert_eq!(code.parameter_count, 0); // TODO
        let mut callback_function_body = Vec::new();
        let mut writer = CodeWriter::new(&mut callback_function_body);
        writer.set_indent_level(1);
        code_translator
            .translate(&mut writer, code)
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

    fn write_setup_function<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        writeln!(w, "void {}()", self.setup_function_name())?;
        writeln!(w, "{{")?;
        writeln!(
            w.indented(),
            "QObject::connect({}, &{}, this->root_, [this]() {{ this->{}(); }});",
            self.sender,
            self.signal,
            self.callback_function_name()
        )?;
        writeln!(w, "}}")?;
        writeln!(w)
    }

    fn write_callback_function<W: io::Write>(&self, w: &mut CodeWriter<W>) -> io::Result<()> {
        writeln!(w, "void {}()", self.callback_function_name())?;
        writeln!(w, "{{")?;
        w.get_mut().write_all(&self.callback_function_body)?;
        writeln!(w, "}}")?;
        writeln!(w)
    }
}

#[derive(Clone, Debug)]
struct CxxCodeBodyTranslator {
    root_object_name: tir::NamedObjectRef,
    tr_context: String,
    return_kind: CxxCodeReturnKind,
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
        }
    }

    fn translate<W: io::Write>(
        &self,
        w: &mut CodeWriter<W>,
        code: &tir::CodeBody,
    ) -> io::Result<()> {
        self.write_locals(&mut w.indented(), &code.locals)?;
        for (i, b) in code.basic_blocks.iter().enumerate() {
            writeln!(w, "{}:", self.format_basic_block_ref(tir::BasicBlockRef(i)))?;
            self.write_basic_block(&mut w.indented(), b)?;
        }
        Ok(())
    }

    fn write_locals<W: io::Write>(
        &self,
        w: &mut CodeWriter<W>,
        locals: &[tir::Local],
    ) -> io::Result<()> {
        for a in locals {
            writeln!(
                w,
                "{} {};",
                a.ty.qualified_cxx_name(),
                self.format_local_ref(a.name)
            )?;
        }
        Ok(())
    }

    fn write_basic_block<W: io::Write>(
        &self,
        w: &mut CodeWriter<W>,
        block: &tir::BasicBlock,
    ) -> io::Result<()> {
        use tir::{Operand, Terminator};
        for s in &block.statements {
            self.write_statement(w, s)?;
        }
        match block.terminator() {
            Terminator::Br(x) => writeln!(w, "goto {};", self.format_basic_block_ref(*x))?,
            Terminator::BrCond(x, y, z) => {
                writeln!(w, "if ({})", self.format_operand(x))?;
                writeln!(w.indented(), "goto {};", self.format_basic_block_ref(*y))?;
                writeln!(w, "else")?;
                writeln!(w.indented(), "goto {};", self.format_basic_block_ref(*z))?;
            }
            Terminator::Return(Operand::Void(_)) => writeln!(w, "return;")?,
            Terminator::Return(x) => match self.return_kind {
                CxxCodeReturnKind::Value => {
                    writeln!(w, "return {};", self.format_operand(x))?;
                }
                CxxCodeReturnKind::Void => {
                    writeln!(w, "static_cast<void>({});", self.format_operand(x))?;
                    writeln!(w, "return;")?;
                }
            },
            Terminator::Unreachable => writeln!(w, "Q_UNREACHABLE();")?,
        }
        Ok(())
    }

    fn write_statement<W: io::Write>(
        &self,
        w: &mut CodeWriter<W>,
        stmt: &tir::Statement,
    ) -> io::Result<()> {
        use tir::Statement;
        match stmt {
            Statement::Assign(l, r) => writeln!(
                w,
                "{} = {};",
                self.format_local_ref(*l),
                self.format_rvalue(r)
            ),
            Statement::Exec(r) => writeln!(w, "{};", self.format_rvalue(r)),
            Statement::ObserveProperty(h, l, prop) => {
                // Note that minimizing the signal/slot connections is NOT the goal of the dynamic
                // subscription. Uninteresting connection may be left if this statement is out
                // of the execution path.
                let observer = self.format_property_observer_ref(*h);
                let sender = self.format_local_ref(*l);
                // observer.object may point to deleted object, where new object could be
                // allocated. observer.connection should be invalidated in such cases.
                writeln!(
                    w,
                    "if (Q_UNLIKELY(!{}.connection || {}.object != {})) {{",
                    observer, observer, sender
                )?;
                w.indent();
                writeln!(w, "QObject::disconnect({}.connection);", observer)?;
                writeln!(
                    w,
                    "{}.connection = QObject::connect({}, &{}::{}, this->root_, update);",
                    observer,
                    sender,
                    prop.object_class().qualified_cxx_name(),
                    prop.notify_signal_name()
                        .expect("property must be observable"),
                )?;
                writeln!(w, "{}.object = {};", observer, sender)?;
                w.unindent();
                writeln!(w, "}}")
            }
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

    fn format_property_observer_ref(&self, r: tir::PropertyObserverRef) -> String {
        format!("observed[{}]", r.0)
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

#[derive(Debug)]
struct CodeWriter<'w, W: io::Write> {
    inner: &'w mut W,
    indent_level: u32,
    line_start: bool,
}

impl<'w, W: io::Write> CodeWriter<'w, W> {
    pub fn new(inner: &'w mut W) -> Self {
        CodeWriter {
            inner,
            indent_level: 0,
            line_start: true,
        }
    }

    pub fn indented(&mut self) -> CodeWriter<W> {
        CodeWriter {
            inner: self.inner,
            indent_level: self.indent_level + 1,
            line_start: true,
        }
    }

    pub fn set_indent_level(&mut self, level: u32) {
        self.indent_level = level;
    }

    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    pub fn unindent(&mut self) {
        self.indent_level -= 1;
    }

    pub fn get_mut(&mut self) -> &mut W {
        self.inner
    }
}

impl<'w, W: io::Write> io::Write for CodeWriter<'w, W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // assumes buf doesn't contain '\n' in the middle
        let was_line_start = self.line_start;
        self.line_start = false;
        if was_line_start && !(buf.starts_with(b"\n") || buf.starts_with(b"#")) {
            for _ in 0..self.indent_level {
                self.inner.write_all(b"    ")?;
            }
        }
        let n = self.inner.write(buf)?;
        if n == buf.len() {
            self.line_start = buf.ends_with(b"\n");
        }
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}
