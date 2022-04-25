use clap::Parser;
use qmluic::qml::{
    Node, ParseError, UiAttachedTypeBindingMap, UiBindingMap, UiBindingValue, UiDocument,
    UiObjectDefinition, UiProgram,
};
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
    match dump_doc(&doc, &opts) {
        Ok(()) => {}
        Err(e) => {
            print_parse_error(&doc, &e)?;
            process::exit(1);
        }
    }

    Ok(())
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct DumpOptions {
    sexp: bool,
}

fn dump_doc<'tree>(doc: &'tree UiDocument, opts: &DumpOptions) -> Result<(), ParseError<'tree>> {
    dump_program(
        &UiProgram::from_node(doc.root_node(), doc.source())?,
        doc.source(),
        opts,
    )
}

fn dump_program<'tree, 'source>(
    program: &UiProgram<'tree, 'source>,
    source: &'source str,
    opts: &DumpOptions,
) -> Result<(), ParseError<'tree>> {
    println!("=== Object ids ===");
    for (id, &node) in program.object_id_map() {
        println!("{}: {}", id, format_node(node, opts));
    }
    println!();

    println!("=== Object definition ===");
    dump_object_definition(
        &UiObjectDefinition::from_node(program.root_object_node(), source)?,
        source,
        opts,
        0,
    )?;

    Ok(())
}

fn dump_object_definition<'tree, 'source>(
    obj: &UiObjectDefinition<'tree, 'source>,
    source: &'source str,
    opts: &DumpOptions,
    depth: usize,
) -> Result<(), ParseError<'tree>> {
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
    dump_attached_type_map(obj.attached_type_map(), source, opts, depth + 1)?;
    dump_binding_map(obj.binding_map(), source, opts, depth + 1)?;
    for &n in obj.child_object_nodes() {
        dump_object_definition(
            &UiObjectDefinition::from_node(n, source)?,
            source,
            opts,
            depth + 1,
        )?;
    }
    println!("{:indent$}}}", "", indent = INDENT_WIDTH * depth);
    Ok(())
}

fn dump_binding_map<'tree, 'source>(
    map: &UiBindingMap<'tree, 'source>,
    source: &'source str,
    opts: &DumpOptions,
    depth: usize,
) -> Result<(), ParseError<'tree>> {
    for (name, value) in map {
        match value {
            UiBindingValue::Node(n) => {
                println!(
                    "{:indent$}{}: {}",
                    "",
                    name,
                    format_node(*n, opts),
                    indent = INDENT_WIDTH * depth
                );
            }
            UiBindingValue::Map(m) => {
                println!("{:indent$}{}: {{", "", name, indent = INDENT_WIDTH * depth);
                dump_binding_map(m, source, opts, depth + 1)?;
                println!("{:indent$}}}", "", indent = INDENT_WIDTH * depth);
            }
        }
    }
    Ok(())
}

fn dump_attached_type_map<'tree, 'source>(
    map: &UiAttachedTypeBindingMap<'tree, 'source>,
    source: &'source str,
    opts: &DumpOptions,
    depth: usize,
) -> Result<(), ParseError<'tree>> {
    for (name, m) in map {
        println!("{:indent$}{}: {{", "", name, indent = INDENT_WIDTH * depth);
        dump_binding_map(m, source, opts, depth + 1)?;
        println!("{:indent$}}}", "", indent = INDENT_WIDTH * depth);
    }
    Ok(())
}

fn format_node<'tree>(node: Node<'tree>, opts: &DumpOptions) -> String {
    if opts.sexp {
        node.to_sexp()
    } else {
        format!("{:?}", node)
    }
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
