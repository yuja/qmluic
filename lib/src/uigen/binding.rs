use super::property::{DynamicPropertiesMap, PropertyDescDynamicExpression, WithNode};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::{ObjectNode, ObjectTree};
use crate::qtname::FileNameRules;
use crate::typemap::TypeSpace;
use itertools::Itertools as _;
use std::io;
use std::iter;

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

        let mut bindings = Vec::new();
        for (obj_node, properties_map) in object_tree.flat_iter().zip(object_properties) {
            // TODO: exclude pseudo node like QActionSeparator
            bindings.extend(
                properties_map
                    .iter()
                    .sorted_by_key(|&(k, _)| k)
                    .filter_map(|(_, v)| BindingCode::build(object_tree, obj_node, v, diagnostics)),
            );
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
    receiver: CxxFieldRef,
    sender_signals: Vec<(CxxFieldRef, String)>,
    write_expr: String,
}

impl BindingCode {
    fn build(
        object_tree: &ObjectTree,
        obj_node: ObjectNode,
        prop: &WithNode<PropertyDescDynamicExpression>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        // TODO: get around name conflicts
        let setup_function_name = format!("setup_{}_{}", obj_node.name(), prop.data().desc.name());
        let update_function_name =
            format!("update_{}_{}", obj_node.name(), prop.data().desc.name());
        let receiver = CxxFieldRef::build(object_tree, obj_node.name());
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
            format!("{}->{}({})", obj_node.name(), f, prop.data().value.expr)
        } else {
            diagnostics.push(Diagnostic::error(
                prop.binding_node().byte_range(),
                "not a writable property",
            ));
            return None;
        };
        Some(BindingCode {
            setup_function_name,
            update_function_name,
            receiver,
            sender_signals,
            write_expr,
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
        let indent_body = indent.to_owned() + "    ";
        // TODO: insert cycle detector
        for field in iter::once(&self.receiver)
            .chain(self.sender_signals.iter().map(|(s, _)| s))
            .unique()
        {
            field.write_local_variable(writer, &indent_body)?;
        }
        writeln!(writer)?;
        writeln!(writer, "{indent_body}{};", self.write_expr)?;
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
