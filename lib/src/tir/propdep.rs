use super::core::{CodeBody, NamedObjectRef, Operand, Rvalue, Statement};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::typedexpr::DescribeType as _;

/// Analyzes TIR code to collect object/property dependencies and insert observe statements.
pub fn analyze_code_property_dependency(code: &mut CodeBody<'_>, diagnostics: &mut Diagnostics) {
    for i in 0..code.basic_blocks.len() {
        analyze_block(code, i, diagnostics);
    }
}

fn analyze_block(code: &mut CodeBody<'_>, block_index: usize, diagnostics: &mut Diagnostics) {
    // if a variable comes in from another basic block, simply take it as dynamic
    let mut locals: Vec<Option<&NamedObjectRef>> = vec![None; code.locals.len()];
    let block = &code.basic_blocks[block_index];
    let mut to_observe = Vec::new();
    for (line, stmt) in block.statements.iter().enumerate() {
        match stmt {
            Statement::Assign(_, r) | Statement::Exec(r) => match r {
                Rvalue::ReadProperty(a, prop)
                    if a.type_desc().is_pointer() && !prop.is_constant() =>
                {
                    match prop.notify_signal().transpose() {
                        Ok(Some(signal)) => match a {
                            Operand::NamedObject(x) => {
                                code.static_property_deps.push((x.name.clone(), signal));
                            }
                            Operand::Local(x) => {
                                if let Some(n) = locals[x.name.0] {
                                    code.static_property_deps.push((n.clone(), signal));
                                } else {
                                    // could be deduplicated by (local, generation, signal) if needed
                                    to_observe.push((line, x.name, signal));
                                }
                            }
                            Operand::Constant(_) | Operand::EnumVariant(_) | Operand::Void(_) => {
                                panic!("invald read_property: {r:?}");
                            }
                        },
                        Ok(None) => diagnostics.push(Diagnostic::error(
                            a.byte_range(),
                            format!("unobservable property: {}", prop.name()),
                        )),
                        Err(e) => diagnostics.push(Diagnostic::error(
                            a.byte_range(),
                            format!("type resolution failed: {e}"),
                        )),
                    }
                }
                _ => {}
            },
            Statement::ObserveProperty(..) => {}
        }

        // update locals *after* inspecting rvalue
        match stmt {
            Statement::Assign(l, r) => {
                locals[l.0] = match r {
                    // track object reference
                    Rvalue::Copy(a) => match a {
                        Operand::Local(x) => locals[x.name.0],
                        Operand::NamedObject(x) => Some(&x.name),
                        Operand::Constant(_) | Operand::EnumVariant(_) | Operand::Void(_) => None,
                    },
                    // otherwise reset to unknown state
                    _ => None,
                };
            }
            Statement::Exec(_) | Statement::ObserveProperty(..) => {}
        }
    }

    // allocate observers in order (just for code readability)
    let mut observers = Vec::new();
    observers.resize_with(to_observe.len(), || code.alloc_property_observer());

    // we could teach caller about the observed properties without mutating the code body,
    // but having the statement makes it easy to test and debug.
    let block = &mut code.basic_blocks[block_index];
    for ((line, obj, signal), h) in to_observe.into_iter().zip(observers).rev() {
        block
            .statements
            .insert(line, Statement::ObserveProperty(h, obj, signal));
    }
}
