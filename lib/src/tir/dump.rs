use super::{BasicBlock, CodeBody, ConstantValue, Local, Operand, Rvalue, Statement, Terminator};
use crate::typedexpr::DescribeType;
use std::io;

/// Prints `CodeBody` in human-readable format.
pub fn dump_code_body<W: io::Write>(w: &mut W, code: &CodeBody) -> io::Result<()> {
    dump_locals(w, &code.locals)?;
    for (i, b) in code.basic_blocks.iter().enumerate() {
        writeln!(w, ".{}:", i)?;
        dump_basic_block(w, b)?;
    }
    Ok(())
}

fn dump_locals<W: io::Write>(w: &mut W, locals: &[Local]) -> io::Result<()> {
    for a in locals {
        writeln!(w, "    %{}: {}", a.name.0, a.ty.qualified_cxx_name())?;
    }
    Ok(())
}

fn dump_basic_block<W: io::Write>(w: &mut W, block: &BasicBlock) -> io::Result<()> {
    for s in &block.statements {
        dump_statement(w, s)?;
    }
    match block.terminator() {
        Terminator::Return(x) => writeln!(w, "    return {}", format_operand(x)),
    }
}

fn dump_statement<W: io::Write>(w: &mut W, stmt: &Statement) -> io::Result<()> {
    match stmt {
        Statement::Assign(l, r) => writeln!(w, "    %{} = {}", l.0, format_rvalue(r)),
    }
}

fn format_rvalue(rv: &Rvalue) -> String {
    match rv {
        Rvalue::BinaryArithOp(op, l, r) => format!(
            "binary_arith_op '{}', {}, {}",
            op,
            format_operand(l),
            format_operand(r)
        ),
        Rvalue::ReadProperty(obj, prop) => {
            format!("read_property {}, {:?}", format_operand(obj), prop.name())
        }
    }
}

fn format_operand(a: &Operand) -> String {
    match a {
        Operand::Constant(x) => match x {
            ConstantValue::Bool(v) => format!("{:?}: {}", v, a.type_desc().qualified_name()),
            ConstantValue::Integer(v) => format!("{:?}: {}", v, a.type_desc().qualified_name()),
            ConstantValue::Float(v) => format!("{:?}: {}", v, a.type_desc().qualified_name()),
            ConstantValue::CString(v) => format!("{:?}: {}", v, a.type_desc().qualified_name()),
            ConstantValue::QString(v) => format!("{:?}: {}", v, a.type_desc().qualified_name()),
        },
        Operand::Local(x) => format!("%{}: {}", x.name.0, a.type_desc().qualified_name()),
        Operand::NamedObject(x) => format!("[{}]: {}", x.name.0, a.type_desc().qualified_name()),
    }
}
