use clap::Parser;
use qmluic::qmlast::*;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::process;

const INDENT_WIDTH: usize = 2;

#[derive(Parser, Clone, Debug, Eq, PartialEq)]
struct Args {
    /// File to parse
    file: PathBuf,
    /// Print syntax node as S-expression of CST node
    #[clap(long)]
    sexp: bool,
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let doc = UiDocument::with_source(fs::read_to_string(args.file)?);
    if doc.has_syntax_error() {
        print_syntax_errors(&doc)?;
        process::exit(1);
    }

    let opts = DumpOptions { sexp: args.sexp };
    let mut errors = Vec::new();
    dump_program(doc.root_node(), doc.source(), &opts, &mut errors);
    if !errors.is_empty() {
        for e in &errors {
            print_parse_error(&doc, &e)?;
        }
        process::exit(1);
    }

    Ok(())
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct DumpOptions {
    sexp: bool,
}

fn dump_program<'tree, 'source>(
    node: Node<'tree>,
    source: &'source str,
    opts: &DumpOptions,
    errors: &mut Vec<ParseError<'tree>>,
) {
    let program = match UiProgram::from_node(node, source) {
        Ok(x) => x,
        Err(e) => {
            errors.push(e);
            dump_node(node, opts, 0);
            return;
        }
    };
    println!("=== Object ids ===");
    for (id, &node) in program.object_id_map() {
        println!("{}: {}", id, format_node(node, opts));
    }
    println!();

    println!("=== Object definition ===");
    dump_object_definition(program.root_object_node(), source, opts, errors, 0);
}

fn dump_object_definition<'tree, 'source>(
    node: Node<'tree>,
    source: &'source str,
    opts: &DumpOptions,
    errors: &mut Vec<ParseError<'tree>>,
    depth: usize,
) {
    let obj = match UiObjectDefinition::from_node(node, source) {
        Ok(x) => x,
        Err(e) => {
            errors.push(e);
            dump_node(node, opts, depth);
            return;
        }
    };
    println!(
        "{:indent$}{} {{",
        "",
        obj.type_name(),
        indent = INDENT_WIDTH * depth
    );
    println!(
        "{:indent$}id: {:?}",
        "",
        obj.object_id(),
        indent = INDENT_WIDTH * (depth + 1)
    );
    dump_attached_type_map(obj.attached_type_map(), source, opts, errors, depth + 1);
    dump_binding_map(obj.binding_map(), source, opts, errors, depth + 1);
    for &n in obj.child_object_nodes() {
        dump_object_definition(n, source, opts, errors, depth + 1);
    }
    println!("{:indent$}}}", "", indent = INDENT_WIDTH * depth);
}

fn dump_binding_map<'tree, 'source>(
    map: &UiBindingMap<'tree, 'source>,
    source: &'source str,
    opts: &DumpOptions,
    errors: &mut Vec<ParseError<'tree>>,
    depth: usize,
) {
    for (name, value) in map {
        match value {
            UiBindingValue::Node(n) => {
                println!(
                    "{:indent$}{}: {}",
                    "",
                    name,
                    format_expression(*n, source, opts, errors),
                    indent = INDENT_WIDTH * depth
                );
            }
            UiBindingValue::Map(_, m) => {
                println!("{:indent$}{}: {{", "", name, indent = INDENT_WIDTH * depth);
                dump_binding_map(m, source, opts, errors, depth + 1);
                println!("{:indent$}}}", "", indent = INDENT_WIDTH * depth);
            }
        }
    }
}

fn dump_attached_type_map<'tree, 'source>(
    map: &UiAttachedTypeBindingMap<'tree, 'source>,
    source: &'source str,
    opts: &DumpOptions,
    errors: &mut Vec<ParseError<'tree>>,
    depth: usize,
) {
    for (name, m) in map {
        println!("{:indent$}{}: {{", "", name, indent = INDENT_WIDTH * depth);
        dump_binding_map(m, source, opts, errors, depth + 1);
        println!("{:indent$}}}", "", indent = INDENT_WIDTH * depth);
    }
}

fn format_expression<'tree, 'source>(
    node: Node<'tree>,
    source: &'source str,
    opts: &DumpOptions,
    errors: &mut Vec<ParseError<'tree>>,
) -> String {
    let expr = match Expression::from_node(node, source) {
        Ok(x) => x,
        Err(e) => {
            errors.push(e);
            return format_node(node, opts);
        }
    };
    match expr {
        Expression::Identifier(n) => n.to_string(),
        Expression::Number(v) => v.to_string(),
        Expression::String(s) => format!("{s:?}"),
        Expression::Bool(b) => format!("{b:?}"),
        Expression::Array(xs) => {
            let formatted_items: Vec<_> = xs
                .iter()
                .map(|&n| format_expression(n, source, opts, errors))
                .collect();
            format!("[{}]", formatted_items.join(", "))
        }
        Expression::MemberExpression(x) => {
            let formatted_obj = format_expression(x.object, source, opts, errors);
            format!("{}.{}", formatted_obj, x.property)
        }
        Expression::CallExpression(x) => {
            let formatted_func = format_expression(x.function, source, opts, errors);
            let formatted_args: Vec<_> = x
                .arguments
                .iter()
                .map(|&n| format_expression(n, source, opts, errors))
                .collect();
            format!("{}({})", formatted_func, formatted_args.join(", "))
        }
        Expression::UnaryExpression(x) => {
            let formatted_arg = format_expression(x.argument, source, opts, errors);
            format!("{}({})", x.operator, formatted_arg)
        }
        Expression::BinaryExpression(x) => {
            let formatted_left = format_expression(x.left, source, opts, errors);
            let formatted_right = format_expression(x.right, source, opts, errors);
            format!("({}) {} ({})", formatted_left, x.operator, formatted_right)
        }
    }
}

fn format_node<'tree>(node: Node<'tree>, opts: &DumpOptions) -> String {
    if opts.sexp {
        node.to_sexp()
    } else {
        format!("{:?}", node)
    }
}

fn dump_node<'tree>(node: Node<'tree>, opts: &DumpOptions, depth: usize) {
    println!(
        "{:indent$}{}",
        "",
        format_node(node, opts),
        indent = INDENT_WIDTH * depth
    );
}

fn print_syntax_errors(doc: &UiDocument) -> io::Result<()> {
    for e in &doc.collect_syntax_errors::<Vec<_>>() {
        print_parse_error(doc, e)?;
    }
    Ok(())
}

fn print_parse_error(doc: &UiDocument, error: &ParseError) -> io::Result<()> {
    use ariadne::{Color, Label, Report, ReportKind, Source};
    let start_char_index = doc.source()[..error.start_byte()].chars().count();
    let end_char_index = start_char_index + doc.source()[error.byte_range()].chars().count();
    let report = Report::build(ReportKind::Error, (), start_char_index)
        .with_message(error)
        .with_label(Label::new(start_char_index..end_char_index).with_color(Color::Yellow))
        .finish();
    report.eprint(Source::from(doc.source()))
}
