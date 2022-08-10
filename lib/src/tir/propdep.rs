use super::core::{CodeBody, NamedObjectRef, Operand, Rvalue, Statement};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::typedexpr::DescribeType as _;
use crate::typemap::{Property, TypeMapError};

/// Analyzes TIR code to collect object/property dependencies and insert observe statements.
pub fn analyze_code_property_dependency<'a>(
    code: &mut CodeBody<'a>,
    diagnostics: &mut Diagnostics,
) {
    for i in 0..code.basic_blocks.len() {
        analyze_block(code, i, diagnostics);
    }
}

fn analyze_block<'a>(code: &mut CodeBody<'a>, block_index: usize, diagnostics: &mut Diagnostics) {
    // if a variable comes in from another basic block, simply take it as dynamic
    let mut locals: Vec<Option<&NamedObjectRef>> = vec![None; code.locals.len()];
    let block = &code.basic_blocks[block_index];
    let mut to_observe = Vec::new();
    for (line, stmt) in block.statements.iter().enumerate() {
        match stmt {
            Statement::Assign(_, r) | Statement::Exec(r) => match r {
                Rvalue::ReadProperty(a, prop) if a.type_desc().is_pointer() => {
                    // ensure signal function and its parameter types can be resolved
                    // (otherwise connection couldn't be generated)
                    match is_valid_observable_property(prop) {
                        Ok(true) => match a {
                            Operand::NamedObject(x) => {
                                code.static_property_deps
                                    .push((x.name.clone(), prop.clone()));
                            }
                            Operand::Local(x) => {
                                if let Some(n) = locals[x.name.0] {
                                    code.static_property_deps.push((n.clone(), prop.clone()));
                                } else {
                                    // could be deduplicated by (local, generation, prop) if needed
                                    to_observe.push((line, x.name, prop.clone()));
                                }
                            }
                            Operand::Constant(_) | Operand::EnumVariant(_) | Operand::Void(_) => {
                                panic!("invald read_property: {r:?}");
                            }
                        },
                        Ok(false) => diagnostics.push(Diagnostic::error(
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
    for ((line, obj, prop), h) in to_observe.into_iter().zip(observers).rev() {
        block
            .statements
            .insert(line, Statement::ObserveProperty(h, obj, prop));
    }
}

fn is_valid_observable_property(prop: &Property) -> Result<bool, TypeMapError> {
    match prop.notify_signal().transpose()? {
        Some(_) => Ok(true),
        None => Ok(false),
    }
}
