//! Qt user interface XML (.ui) generator.

use self::objcode::ObjectCodeMap;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectTree;
use crate::qmlast::{UiImportSource, UiProgram};
use crate::qmldir;
use crate::qmldoc::UiDocument;
use crate::typemap::{ImportedModuleSpace, ModuleId, ModuleIdBuf, TypeMap};

mod binding;
mod context;
mod expr;
mod form;
mod gadget;
mod layout;
mod objcode;
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

pub type XmlWriter<W> = quick_xml::Writer<W>;

/// Builds `UiForm` (and `UiSupportCode`) from the given `doc`.
///
/// If `DynamicBindingHandling::Generate`, `UiSupportCode` is returned no matter if
/// the given document contains dynamic bindings or not.
pub fn build(
    base_ctx: &BuildContext,
    doc: &UiDocument,
    diagnostics: &mut Diagnostics,
) -> Option<(UiForm, Option<UiSupportCode>)> {
    let program = diagnostics.consume_err(UiProgram::from_node(doc.root_node(), doc.source()))?;
    let type_space = make_doc_module_space(doc, &program, base_ctx.type_map, diagnostics);
    let object_tree = ObjectTree::build(
        program.root_object_node(),
        doc.source(),
        &type_space,
        diagnostics,
    )?;
    let object_code_maps =
        build_object_code_maps(doc, &type_space, &object_tree, base_ctx, diagnostics);

    let ctx = BuildDocContext::new(doc, &type_space, &object_tree, &object_code_maps, base_ctx);
    let form = UiForm::build(&ctx, object_tree.root(), diagnostics);

    for p in object_code_maps
        .iter()
        .flat_map(|cm| cm.all_attached_properties().values().map(|(_, pm)| pm))
        .flat_map(|pm| pm.values())
        .filter(|p| !p.is_evaluated_constant())
    {
        diagnostics.push(Diagnostic::error(
            p.binding_node().byte_range(),
            "unused or unsupported dynamic binding to attached property",
        ));
    }

    let ui_support = match base_ctx.dynamic_binding_handling {
        DynamicBindingHandling::Omit => None,
        DynamicBindingHandling::Generate => Some(UiSupportCode::build(
            doc.type_name(),
            &base_ctx.file_name_rules,
            &object_tree,
            &object_code_maps,
            diagnostics,
        )),
        DynamicBindingHandling::Reject => {
            for p in object_code_maps
                .iter()
                .flat_map(|m| m.properties().values())
                .filter(|p| !p.is_evaluated_constant())
            {
                if p.desc().is_writable() {
                    diagnostics.push(Diagnostic::error(
                        p.node().byte_range(),
                        "unsupported dynamic binding",
                    ));
                } else {
                    diagnostics.push(Diagnostic::error(
                        p.binding_node().byte_range(),
                        "not a writable property",
                    ));
                }
            }

            for c in object_code_maps.iter().flat_map(|cm| cm.callbacks()) {
                diagnostics.push(Diagnostic::error(
                    c.binding_node().byte_range(),
                    "signal callback cannot be translated without dynamic binding",
                ));
            }

            None
        }
    };

    Some((form, ui_support))
}

fn make_doc_module_space<'a>(
    doc: &UiDocument,
    program: &UiProgram,
    type_map: &'a TypeMap,
    diagnostics: &mut Diagnostics,
) -> ImportedModuleSpace<'a> {
    let mut module_space = ImportedModuleSpace::new(type_map);
    assert!(module_space.import_module(ModuleId::Builtins));
    if let Some(p) = doc.path().and_then(|p| p.parent()) {
        // QML files in the base directory should be available by default
        let id = ModuleIdBuf::Directory(qmldir::normalize_path(p));
        if !module_space.import_module(id.as_ref()) {
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
        if imp.version().is_some() {
            diagnostics.push(Diagnostic::warning(
                imp.node().byte_range(),
                "import version is ignored",
            ));
        }
        // TODO: anchor diagnostic message onto imp.source() node
        let id = match imp.source() {
            UiImportSource::Identifier(x) => ModuleIdBuf::Named(x.to_string(doc.source()).into()),
            UiImportSource::String(x) => {
                if let Some(p) = doc.path().and_then(|p| p.parent()) {
                    ModuleIdBuf::Directory(qmldir::normalize_path(p.join(x)))
                } else {
                    diagnostics.push(Diagnostic::error(
                        imp.node().byte_range(),
                        "cannot resolve directory path against inline QML document",
                    ));
                    continue;
                }
            }
        };
        if !module_space.import_module(id.as_ref()) {
            diagnostics.push(Diagnostic::error(
                imp.node().byte_range(),
                "module not found",
            ));
        }
    }

    module_space
}

fn build_object_code_maps<'a, 't, 's>(
    doc: &'s UiDocument,
    type_space: &'s ImportedModuleSpace<'a>,
    object_tree: &'s ObjectTree<'a, 't>,
    base_ctx: &'s BuildContext<'a>,
    diagnostics: &mut Diagnostics,
) -> Vec<ObjectCodeMap<'a, 't, 's>> {
    object_tree
        .flat_iter()
        .map(|obj_node| {
            let ctx = ObjectContext::new(doc, type_space, object_tree, obj_node, base_ctx);
            ObjectCodeMap::build(&ctx, obj_node, diagnostics)
        })
        .collect()
}
