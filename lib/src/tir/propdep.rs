use super::core::{BasicBlock, CodeBody, NamedObjectRef, Operand, Rvalue, Statement};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::typedexpr::DescribeType as _;
use crate::typemap::Property;

/// Analyzes TIR code to collect object/property dependencies.
///
/// Returns a list of object/properties which can be observed statically.
pub fn analyze_code_property_dependency<'a>(
    code: &CodeBody<'a>,
    diagnostics: &mut Diagnostics,
) -> Vec<(NamedObjectRef, Property<'a>)> {
    let mut static_deps = Vec::new();
    for block in &code.basic_blocks {
        analyze_block(&mut static_deps, code, block, diagnostics);
    }
    static_deps
}

fn analyze_block<'a>(
    static_deps: &mut Vec<(NamedObjectRef, Property<'a>)>,
    code: &CodeBody<'a>,
    block: &BasicBlock<'a>,
    diagnostics: &mut Diagnostics,
) {
    // if a variable comes in from another basic block, simply take it as dynamic
    let mut locals: Vec<Option<&NamedObjectRef>> = vec![None; code.locals.len()];
    for stmt in &block.statements {
        match stmt {
            Statement::Assign(_, r) | Statement::Exec(r) => match r {
                Rvalue::ReadProperty(a, prop) if a.type_desc().is_pointer() => {
                    let mut push_notifiable = |name: &NamedObjectRef, prop: &Property<'a>| {
                        if prop.is_notifiable() {
                            static_deps.push((name.clone(), prop.clone()));
                        } else {
                            diagnostics.push(Diagnostic::error(
                                a.byte_range(),
                                format!("unobservable property: {}", prop.name()),
                            ));
                        }
                    };
                    match a {
                        Operand::NamedObject(x) => push_notifiable(&x.name, prop),
                        Operand::Local(x) => {
                            if let Some(n) = locals[x.name.0] {
                                push_notifiable(n, prop);
                            } else {
                                // TODO: handle dynamic dependency
                                diagnostics.push(Diagnostic::error(
                                    a.byte_range(),
                                    "chained object property is not supported",
                                ));
                            }
                        }
                        Operand::Constant(_) | Operand::EnumVariant(_) | Operand::Void(_) => {
                            panic!("invald read_property: {r:?}");
                        }
                    }
                }
                _ => {}
            },
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
            Statement::Exec(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::dump;
    use super::super::testenv::*;
    use super::*;

    fn analyze_code<'a>(code: &CodeBody<'a>) -> Vec<(NamedObjectRef, Property<'a>)> {
        let mut diagnostics = Diagnostics::new();
        let static_deps = analyze_code_property_dependency(&code, &mut diagnostics);
        assert!(!diagnostics.has_error());
        static_deps
    }

    fn dump_code(code: &CodeBody) -> String {
        let mut buf = Vec::new();
        dump::dump_code_body(&mut buf, &code).unwrap();
        String::from_utf8(buf).unwrap()
    }

    #[test]
    fn direct_static_deps_simple() {
        let env = Env::new();
        let code = env.build(
            r###"{
                !foo.checked
            }"###,
        );

        let static_deps = analyze_code(&code);
        insta::assert_snapshot!(dump_code(&code), @r###"
            %0: bool
            %1: bool
        .0:
            %0 = read_property [foo]: Foo*, "checked"
            %1 = unary_op '!', %0: bool
            return %1: bool
        "###);

        assert_eq!(static_deps.len(), 1);
        assert_eq!(static_deps[0].0 .0, "foo");
        assert_eq!(static_deps[0].1.name(), "checked");
    }

    #[test]
    fn direct_static_deps_branched() {
        let env = Env::new();
        let code = env.build(
            r###"{
                foo.checked ? foo2.text : foo3.text
            }"###,
        );

        let static_deps = analyze_code(&code);
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
        "###);

        assert_eq!(static_deps.len(), 3);
        assert_eq!(static_deps[0].0 .0, "foo");
        assert_eq!(static_deps[0].1.name(), "checked");
        assert_eq!(static_deps[1].0 .0, "foo2");
        assert_eq!(static_deps[1].1.name(), "text");
        assert_eq!(static_deps[2].0 .0, "foo3");
        assert_eq!(static_deps[2].1.name(), "text");
    }

    #[test]
    fn indirect_static_deps_branch_local() {
        let env = Env::new();
        let code = env.build(
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

        let static_deps = analyze_code(&code);
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
        "###);

        assert_eq!(static_deps.len(), 3);
        assert_eq!(static_deps[0].0 .0, "foo");
        assert_eq!(static_deps[0].1.name(), "checked");
        assert_eq!(static_deps[1].0 .0, "foo2");
        assert_eq!(static_deps[1].1.name(), "text");
        assert_eq!(static_deps[2].0 .0, "foo3");
        assert_eq!(static_deps[2].1.name(), "text");
    }

    #[test]
    fn indirect_static_deps_reassigned() {
        let env = Env::new();
        let code = env.build(
            r###"{
                let w = foo;
                let s = w.text;
                w = foo2;
                s + w.text
            }"###,
        );

        let static_deps = analyze_code(&code);
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
        "###);

        assert_eq!(static_deps.len(), 2);
        assert_eq!(static_deps[0].0 .0, "foo");
        assert_eq!(static_deps[0].1.name(), "text");
        assert_eq!(static_deps[1].0 .0, "foo2");
        assert_eq!(static_deps[1].1.name(), "text");
    }
}
