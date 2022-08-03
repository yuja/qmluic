use super::core::{CodeBody, NamedObjectRef, Operand, Rvalue, Statement};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::typedexpr::DescribeType as _;

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
                    if !prop.is_notifiable() {
                        diagnostics.push(Diagnostic::error(
                            a.byte_range(),
                            format!("unobservable property: {}", prop.name()),
                        ));
                    } else {
                        match a {
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
                        }
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

#[cfg(test)]
mod tests {
    use super::super::testenv::*;
    use super::*;

    fn analyze_code<'a>(code: &mut CodeBody<'a>) {
        let mut diagnostics = Diagnostics::new();
        analyze_code_property_dependency(code, &mut diagnostics);
        assert!(!diagnostics.has_error());
    }

    #[test]
    fn direct_static_deps_simple() {
        let env = Env::new();
        let mut code = env.build(
            r###"{
                !foo.checked
            }"###,
        );

        analyze_code(&mut code);
        insta::assert_snapshot!(dump_code(&code), @r###"
            %0: bool
            %1: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            %1 = unary_op '!', %0: bool
            return %1: bool
        static_property_deps:
            [foo], "checked"
        "###);
    }

    #[test]
    fn direct_static_deps_branched() {
        let env = Env::new();
        let mut code = env.build(
            r###"{
                foo.checked ? foo2.text : foo3.text
            }"###,
        );

        analyze_code(&mut code);
        insta::assert_snapshot!(dump_code(&code), @r###"
            %0: bool
            %1: QString
            %2: QString
            %3: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = read_property [foo2]: Foo*, "text"
            %3 = copy %1: QString
            br .3
        .2:
            %2 = read_property [foo3]: Foo*, "text"
            %3 = copy %2: QString
            br .3
        .3:
            return %3: QString
        static_property_deps:
            [foo], "checked"
            [foo2], "text"
            [foo3], "text"
        "###);
    }

    #[test]
    fn indirect_static_deps_branch_local() {
        let env = Env::new();
        let mut code = env.build(
            r###"{
                if (foo.checked) {
                    let w = foo2;
                    return w.text
                } else {
                    let w = foo3;
                    return w.text;
                }
            }"###,
        );

        analyze_code(&mut code);
        insta::assert_snapshot!(dump_code(&code), @r###"
            %0: bool
            %1: Foo*
            %2: QString
            %3: Foo*
            %4: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .3
        .1:
            %1 = copy [foo2]: Foo*
            %2 = read_property %1: Foo*, "text"
            return %2: QString
        .2:
            unreachable
        .3:
            %3 = copy [foo3]: Foo*
            %4 = read_property %3: Foo*, "text"
            return %4: QString
        .4:
            unreachable
        .5:
            unreachable
        static_property_deps:
            [foo], "checked"
            [foo2], "text"
            [foo3], "text"
        "###);
    }

    #[test]
    fn indirect_static_deps_reassigned() {
        let env = Env::new();
        let mut code = env.build(
            r###"{
                let w = foo;
                let s = w.text;
                w = foo2;
                s + w.text
            }"###,
        );

        analyze_code(&mut code);
        insta::assert_snapshot!(dump_code(&code), @r###"
            %0: Foo*
            %1: QString
            %2: QString
            %3: QString
            %4: QString
        .0:
            %0 = copy [foo]: Foo*
            %1 = read_property %0: Foo*, "text"
            %2 = copy %1: QString
            %0 = copy [foo2]: Foo*
            %3 = read_property %0: Foo*, "text"
            %4 = binary_op '+', %2: QString, %3: QString
            return %4: QString
        static_property_deps:
            [foo], "text"
            [foo2], "text"
        "###);
    }

    #[test]
    fn dynamic_deps_branchy() {
        let env = Env::new();
        let mut code = env.build(
            r###"{
                (foo.checked ? foo2 : foo3).text
            }"###,
        );

        analyze_code(&mut code);
        insta::assert_snapshot!(dump_code(&code), @r###"
            %0: bool
            %1: Foo*
            %2: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = copy [foo2]: Foo*
            br .3
        .2:
            %1 = copy [foo3]: Foo*
            br .3
        .3:
            ^0 = observe_property %1, "text"
            %2 = read_property %1: Foo*, "text"
            return %2: QString
        static_property_deps:
            [foo], "checked"
        "###);
        assert_eq!(code.property_observer_count, 1);
    }

    #[test]
    fn dynamic_deps_reassigned() {
        let env = Env::new();
        let mut code = env.build(
            r###"{
                let w = foo.checked ? foo2 : foo3;
                let s = w.text;
                w = foo2.checked ? foo3 : foo4;
                return s + w.text;
            }"###,
        );

        analyze_code(&mut code);
        insta::assert_snapshot!(dump_code(&code), @r###"
            %0: bool
            %1: Foo*
            %2: Foo*
            %3: QString
            %4: QString
            %5: bool
            %6: Foo*
            %7: QString
            %8: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = copy [foo2]: Foo*
            br .3
        .2:
            %1 = copy [foo3]: Foo*
            br .3
        .3:
            %2 = copy %1: Foo*
            ^0 = observe_property %2, "text"
            %3 = read_property %2: Foo*, "text"
            %4 = copy %3: QString
            %5 = read_property [foo2]: Foo*, "checked"
            br_cond %5: bool, .4, .5
        .4:
            %6 = copy [foo3]: Foo*
            br .6
        .5:
            %6 = copy [foo4]: Foo*
            br .6
        .6:
            %2 = copy %6: Foo*
            ^1 = observe_property %2, "text"
            %7 = read_property %2: Foo*, "text"
            %8 = binary_op '+', %4: QString, %7: QString
            return %8: QString
        .7:
            unreachable
        static_property_deps:
            [foo], "checked"
            [foo2], "checked"
        "###);
        assert_eq!(code.property_observer_count, 2);
    }

    #[test]
    fn dynamic_deps_multiple() {
        let env = Env::new();
        let mut code = env.build(
            r###"{
                let w1 = foo.checked ? foo2 : foo3;
                let w2 = foo2.checked ? foo3 : foo4;
                return w1.text + w2.text;
            }"###,
        );

        analyze_code(&mut code);
        insta::assert_snapshot!(dump_code(&code), @r###"
            %0: bool
            %1: Foo*
            %2: Foo*
            %3: bool
            %4: Foo*
            %5: Foo*
            %6: QString
            %7: QString
            %8: QString
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            br_cond %0: bool, .1, .2
        .1:
            %1 = copy [foo2]: Foo*
            br .3
        .2:
            %1 = copy [foo3]: Foo*
            br .3
        .3:
            %2 = copy %1: Foo*
            %3 = read_property [foo2]: Foo*, "checked"
            br_cond %3: bool, .4, .5
        .4:
            %4 = copy [foo3]: Foo*
            br .6
        .5:
            %4 = copy [foo4]: Foo*
            br .6
        .6:
            %5 = copy %4: Foo*
            ^0 = observe_property %2, "text"
            %6 = read_property %2: Foo*, "text"
            ^1 = observe_property %5, "text"
            %7 = read_property %5: Foo*, "text"
            %8 = binary_op '+', %6: QString, %7: QString
            return %8: QString
        .7:
            unreachable
        static_property_deps:
            [foo], "checked"
            [foo2], "checked"
        "###);
        assert_eq!(code.property_observer_count, 2);
    }
}
