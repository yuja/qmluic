use super::core::{
    BasicBlock, CodeBody, ConstantValue, Local, NamedObjectRef, Operand, Rvalue, Statement,
    Terminator,
};
use crate::typedexpr::DescribeType;
use crate::typemap::Property;
use itertools::Itertools as _;
use std::io;

/// Prints `CodeBody` in human-readable format.
pub fn dump_code_body<W: io::Write>(w: &mut W, code: &CodeBody) -> io::Result<()> {
    dump_locals(w, &code.locals)?;
    for (i, b) in code.basic_blocks.iter().enumerate() {
        writeln!(w, ".{}:", i)?;
        dump_basic_block(w, b)?;
    }
    if !code.static_property_deps.is_empty() {
        writeln!(w, "static_property_deps:")?;
        dump_static_property_deps(w, &code.static_property_deps)?;
    }
    Ok(())
}

fn dump_locals<W: io::Write>(w: &mut W, locals: &[Local]) -> io::Result<()> {
    for a in locals {
        writeln!(w, "    %{}: {}", a.name.0, a.ty.qualified_cxx_name())?;
    }
    Ok(())
}

fn dump_static_property_deps<W: io::Write>(
    w: &mut W,
    deps: &[(NamedObjectRef, Property)],
) -> io::Result<()> {
    for (n, prop) in deps {
        writeln!(w, "    [{}], {:?}", n.0, prop.name())?;
    }
    Ok(())
}

fn dump_basic_block<W: io::Write>(w: &mut W, block: &BasicBlock) -> io::Result<()> {
    for s in &block.statements {
        dump_statement(w, s)?;
    }
    match block.terminator() {
        Terminator::Br(x) => writeln!(w, "    br .{}", x.0),
        Terminator::BrCond(x, y, z) => {
            writeln!(w, "    br_cond {}, .{}, .{}", format_operand(x), y.0, z.0)
        }
        Terminator::Return(x) => writeln!(w, "    return {}", format_operand(x)),
        Terminator::Unreachable => writeln!(w, "    unreachable"),
    }
}

fn dump_statement<W: io::Write>(w: &mut W, stmt: &Statement) -> io::Result<()> {
    match stmt {
        Statement::Assign(l, r) => writeln!(w, "    %{} = {}", l.0, format_rvalue(r)),
        Statement::Exec(r) => writeln!(w, "    {}", format_rvalue(r)),
        Statement::ObserveProperty(h, l, prop) => writeln!(
            w,
            "    ^{} = observe_property %{}, {:?}",
            h.0,
            l.0,
            prop.name()
        ),
    }
}

fn format_rvalue(rv: &Rvalue) -> String {
    match rv {
        Rvalue::Copy(a) => format!("copy {}", format_operand(a)),
        Rvalue::UnaryOp(op, a) => format!("unary_op '{}', {}", op, format_operand(a)),
        Rvalue::BinaryOp(op, l, r) => format!(
            "binary_op '{}', {}, {}",
            op,
            format_operand(l),
            format_operand(r)
        ),
        Rvalue::StaticCast(ty, a) => format!(
            "static_cast '{}', {}",
            ty.qualified_cxx_name(),
            format_operand(a)
        ),
        Rvalue::CallBuiltinFunction(f, args) => {
            format!(
                "call_builtin_function {:?}, {{{}}}",
                f,
                args.iter().map(format_operand).join(", ")
            )
        }
        Rvalue::CallBuiltinMethod(obj, f, args) => {
            format!(
                "call_builtin_method {}, {:?}, {{{}}}",
                format_operand(obj),
                f,
                args.iter().map(format_operand).join(", ")
            )
        }
        Rvalue::CallMethod(obj, meth, args) => {
            format!(
                "call_method {}, {:?}, {{{}}}",
                format_operand(obj),
                meth.name(),
                args.iter().map(format_operand).join(", ")
            )
        }
        Rvalue::ReadProperty(obj, prop) => {
            format!("read_property {}, {:?}", format_operand(obj), prop.name())
        }
        Rvalue::WriteProperty(obj, prop, r) => {
            format!(
                "write_property {}, {:?}, {}",
                format_operand(obj),
                prop.name(),
                format_operand(r)
            )
        }
        Rvalue::MakeList(xs) => {
            format!("make_list {{{}}}", xs.iter().map(format_operand).join(", "))
        }
    }
}

fn format_operand(a: &Operand) -> String {
    match a {
        Operand::Constant(x) => match &x.value {
            ConstantValue::Bool(v) => format!("{:?}: {}", v, a.type_desc().qualified_name()),
            ConstantValue::Integer(v) => format!("{:?}: {}", v, a.type_desc().qualified_name()),
            ConstantValue::Float(v) => format!("{:?}: {}", v, a.type_desc().qualified_name()),
            ConstantValue::CString(v) => format!("{:?}: {}", v, a.type_desc().qualified_name()),
            ConstantValue::QString(v) => format!("{:?}: {}", v, a.type_desc().qualified_name()),
            ConstantValue::EmptyList => format!("{{}}: {}", a.type_desc().qualified_name()),
        },
        Operand::EnumVariant(x) => format!(
            "'{}': {}",
            x.cxx_expression(),
            a.type_desc().qualified_name()
        ),
        Operand::Local(x) => format!("%{}: {}", x.name.0, a.type_desc().qualified_name()),
        Operand::NamedObject(x) => format!("[{}]: {}", x.name.0, a.type_desc().qualified_name()),
        Operand::Void(_) => format!("_: {}", a.type_desc().qualified_name()),
    }
}
