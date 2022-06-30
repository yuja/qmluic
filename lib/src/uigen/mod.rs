//! Qt user interface XML (.ui) generator.

use self::property::PropertiesMap;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectTree;
use crate::qmlast::{UiImportSource, UiProgram};
use crate::qmldir;
use crate::qmldoc::UiDocument;
use crate::typemap::{ImportedModuleSpace, ModuleId, TypeMap};

mod binding;
mod context;
mod expr;
mod form;
mod gadget;
mod layout;
mod object;
mod property;
mod xmlutil;

pub use self::binding::*; // re-export
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
    let mut object_properties =
        build_object_properties(doc, &type_space, &object_tree, base_ctx, diagnostics);
    let dynamic_object_properties: Vec<_> = object_properties
        .iter_mut()
        .map(property::extract_dynamic_properties)
        .collect();

    let ctx = BuildDocContext::new(doc, &type_space, &object_tree, &object_properties, base_ctx);
    let form = UiForm::build(&ctx, object_tree.root(), diagnostics);

    match base_ctx.dynamic_binding_handling {
        DynamicBindingHandling::Omit => {}
        DynamicBindingHandling::Reject => {
            for v in dynamic_object_properties.iter().flat_map(|m| m.values()) {
                diagnostics.push(Diagnostic::error(
                    v.node().byte_range(),
                    "unsupported dynamic binding",
                ));
            }
        }
    }

    Some(form)
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

fn build_object_properties<'a, 't>(
    doc: &UiDocument,
    type_space: &ImportedModuleSpace<'a>,
    object_tree: &ObjectTree<'a, 't>,
    base_ctx: &BuildContext<'a>,
    diagnostics: &mut Diagnostics,
) -> Vec<PropertiesMap<'a, 't>> {
    object_tree
        .flat_iter()
        .map(|obj_node| {
            let cls = obj_node.class();
            let ctx = ObjectContext::new(doc, type_space, object_tree, base_ctx);
            let binding_map = diagnostics
                .consume_err(obj_node.obj().build_binding_map(ctx.source))
                .unwrap_or_default();
            property::collect_properties_with_node(&ctx, cls, &binding_map, diagnostics)
        })
        .collect()
}
