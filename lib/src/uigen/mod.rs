//! Qt user interface XML (.ui) generator.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectTree;
use crate::qmlast::{UiImportSource, UiProgram};
use crate::qmldir;
use crate::qmldoc::UiDocument;
use crate::typemap::{ImportedModuleSpace, ModuleId, TypeMap};
use itertools::Itertools as _;

mod context;
mod expr;
mod form;
mod gadget;
mod layout;
mod object;
mod property;
mod xmlutil;

pub use self::context::*; // re-export
pub use self::expr::*; // re-export
pub use self::form::*; // re-export
pub use self::gadget::*; // re-export
pub use self::layout::*; // re-export
pub use self::object::*; // re-export

pub type XmlError = quick_xml::Error;
pub type XmlResult<T> = quick_xml::Result<T>;
pub type XmlWriter<W> = quick_xml::Writer<W>;

/// Builds `UiForm` from the given `doc`.
pub fn build(
    base_ctx: &BuildContext,
    doc: &UiDocument,
    diagnostics: &mut Diagnostics,
) -> Option<UiForm> {
    let program = diagnostics.consume_err(UiProgram::from_node(doc.root_node(), doc.source()))?;
    let type_space = make_doc_module_space(doc, &program, base_ctx.type_map, diagnostics);
    let object_tree = ObjectTree::build(
        program.root_object_node(),
        doc.source(),
        &type_space,
        diagnostics,
    )?;
    let ctx = BuildDocContext::new(doc, type_space, object_tree, base_ctx);
    let root_object = UiObject::build(&ctx, ctx.object_tree.root(), diagnostics);
    let custom_widgets = ctx
        .object_tree
        .flat_iter()
        .filter_map(|n| n.is_custom_type().then(|| n.class().clone()))
        .unique()
        .filter_map(|cls| CustomWidget::from_class(&cls, &base_ctx.file_name_rules))
        .collect();
    Some(UiForm {
        class: doc.type_name().map(|s| s.to_owned()),
        root_object,
        custom_widgets,
    })
}

fn make_doc_module_space<'a>(
    doc: &UiDocument,
    program: &UiProgram,
    type_map: &'a TypeMap,
    diagnostics: &mut Diagnostics,
) -> ImportedModuleSpace<'a> {
    let mut module_space = ImportedModuleSpace::new(type_map);
    if let Some(p) = doc.path().and_then(|p| p.parent()) {
        // QML files in the base directory should be available by default
        let id = ModuleId::Directory(qmldir::normalize_path(p).into());
        if !module_space.import_module(id) {
            diagnostics.push(Diagnostic::error(0..0, "directory module not found"));
        }
    }

    for imp in program.imports() {
        if imp.alias().is_some() {
            diagnostics.push(Diagnostic::error(
                imp.node().byte_range(),
                "aliased import is not supported",
            ));
            continue;
        }
        // TODO: warn that version field is ignored
        // TODO: anchor diagnostic message onto imp.source() node
        let id = match imp.source() {
            UiImportSource::Identifier(x) => {
                ModuleId::Named(x.to_string(doc.source()).into_owned().into())
            }
            UiImportSource::String(x) => {
                if let Some(p) = doc.path().and_then(|p| p.parent()) {
                    ModuleId::Directory(qmldir::normalize_path(p.join(&x)).into())
                } else {
                    diagnostics.push(Diagnostic::error(
                        imp.node().byte_range(),
                        "cannot resolve directory path against inline QML document",
                    ));
                    continue;
                }
            }
        };
        if !module_space.import_module(id) {
            diagnostics.push(Diagnostic::error(
                imp.node().byte_range(),
                "module not found",
            ));
        }
    }

    module_space
}
