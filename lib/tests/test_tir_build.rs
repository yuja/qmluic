use self::tir_testenv::*;

pub mod tir_testenv;

#[test]
fn string_literal() {
    insta::assert_snapshot!(dump("'foo'"), @r###"
    .0:
        return "foo": QString
    "###);
}

#[test]
fn enum_ref() {
    insta::assert_snapshot!(dump("Foo.Bar0"), @r###"
    .0:
        return 'Foo::Bar0': Foo::Bar
    "###);
}

#[test]
fn untyped_null_literal() {
    insta::assert_snapshot!(dump("null"), @r###"
    .0:
        return nullptr: nullptr_t
    "###);
}

#[test]
fn empty_array_literal() {
    insta::assert_snapshot!(dump("[]"), @r###"
    .0:
        return {}: list
    "###);
}

#[test]
fn string_array_literal() {
    insta::assert_snapshot!(dump("['foo', 'bar']"), @r###"
        %0: QStringList
    .0:
        %0 = make_list 'QStringList', {"foo": QString, "bar": QString}
        return %0: QStringList
    "###);
}

#[test]
fn object_array_literal() {
    insta::assert_snapshot!(dump("[foo, foo2, foo_sub as Foo, null]"), @r###"
        %0: Foo*
        %1: QList<Foo*>
    .0:
        %0 = copy [foo_sub]: FooSub*
        %1 = make_list 'QList<Foo*>', {[foo]: Foo*, [foo2]: Foo*, %0: Foo*, nullptr: nullptr_t}
        return %1: QList<Foo*>
    "###);
}

#[test]
fn object_array_literal_no_implicit_upcast() {
    let env = Env::new();
    assert!(env.try_build("[foo, foo_sub]").is_err());
}

#[test]
fn dynamic_array_literal() {
    insta::assert_snapshot!(dump("[foo.text, foo2.text]"), @r###"
        %0: QString
        %1: QString
        %2: QStringList
    .0:
        %0 = read_property [foo]: Foo*, "text"
        %1 = read_property [foo2]: Foo*, "text"
        %2 = make_list 'QStringList', {%0: QString, %1: QString}
        return %2: QStringList
    "###);
}

#[test]
fn incompatible_array_literal() {
    let env = Env::new();
    assert!(env.try_build("[foo, 'bar']").is_err());
}

#[test]
fn local_declaration_with_literal() {
    insta::assert_snapshot!(dump("{ let s = 'hello'; s }"), @r###"
        %0: QString
    .0:
        %0 = copy "hello": QString
        return %0: QString
    "###);
}

#[test]
fn local_declaration_with_property() {
    insta::assert_snapshot!(dump("{ let s = foo.checked; s }"), @r###"
        %0: bool
        %1: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        %1 = copy %0: bool
        return %1: bool
    "###);
}

#[test]
fn local_declaration_with_ternary() {
    insta::assert_snapshot!(dump("{ let s = foo.checked ? 1 : 2; s }"), @r###"
        %0: bool
        %1: int
        %2: int
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = copy 1: integer
        br .3
    .2:
        %1 = copy 2: integer
        br .3
    .3:
        %2 = copy %1: int
        return %2: int
    "###);
}

#[test]
fn local_declaration_in_nested_block() {
    insta::assert_snapshot!(dump(r###"{
        let a = 'outer';
        {
            let a = a + 'inner';
            foo.text = a;
        }
        foo2.text = a;
    }"###), @r###"
        %0: QString
        %1: QString
        %2: QString
    .0:
        %0 = copy "outer": QString
        %1 = binary_op '+', %0: QString, "inner": QString
        %2 = copy %1: QString
        write_property [foo]: Foo*, "text", %2: QString
        write_property [foo2]: Foo*, "text", %0: QString
        return _: void
    "###);
}

#[test]
fn local_declaration_without_value_but_type_annotation() {
    insta::assert_snapshot!(dump("{ let s: int; s = 0 }"), @r###"
        %0: int
    .0:
        %0 = copy 0: integer
        return _: void
    "###);
}

#[test]
fn local_declaration_with_value_and_type_annotation() {
    insta::assert_snapshot!(dump("{ let s: uint = 0 }"), @r###"
        %0: uint
    .0:
        %0 = copy 0: integer
        return _: void
    "###);
}

#[test]
fn local_declaration_with_null_and_type_annotation() {
    insta::assert_snapshot!(dump("{ let s: Foo = null }"), @r###"
        %0: Foo*
    .0:
        %0 = copy nullptr: nullptr_t
        return _: void
    "###);
}

#[test]
fn local_declaration_with_null_but_no_type_annotation() {
    let env = Env::new();
    assert!(env.try_build("{ let a = null }").is_err());
}

#[test]
fn local_declaration_with_void() {
    let env = Env::new();
    assert!(env.try_build("{ let a = foo.done() }").is_err());
}

#[test]
fn local_declaration_type_mismatch() {
    let env = Env::new();
    assert!(env.try_build("{ let a: int = 'whatever') }").is_err());
}

#[test]
fn const_declaration_without_value_but_type_annotation() {
    let env = Env::new();
    assert!(env.try_build("{ const a: int }").is_err());
}

#[test]
fn local_assignment_branched() {
    insta::assert_snapshot!(dump(r###"{
        let a = '';
        if (foo.checked) {
            a = foo.text;
        } else if (foo2.checked) {
            a = foo2.text;
        }
        foo3.text = a;
    }"###), @r###"
        %0: QString
        %1: bool
        %2: QString
        %3: bool
        %4: QString
    .0:
        %0 = copy "": QString
        %1 = read_property [foo]: Foo*, "checked"
        br_cond %1: bool, .1, .2
    .1:
        %2 = read_property [foo]: Foo*, "text"
        %0 = copy %2: QString
        br .5
    .2:
        %3 = read_property [foo2]: Foo*, "checked"
        br_cond %3: bool, .3, .4
    .3:
        %4 = read_property [foo2]: Foo*, "text"
        %0 = copy %4: QString
        br .4
    .4:
        br .5
    .5:
        write_property [foo3]: Foo*, "text", %0: QString
        return _: void
    "###);
}

#[test]
fn local_assignment_type_mismatch() {
    let env = Env::new();
    assert!(env.try_build("{ let a = true; a = foo.text }").is_err());
}

#[test]
fn const_declaration() {
    insta::assert_snapshot!(dump("{ const s = 'hello'; s }"), @r###"
        %0: QString
    .0:
        %0 = copy "hello": QString
        return %0: QString
    "###);
}

#[test]
fn const_reassignment() {
    let env = Env::new();
    assert!(env.try_build("{ const a = 0; a = 1 }").is_err());
}

#[test]
fn local_declaration_with_object_then_read_property() {
    insta::assert_snapshot!(dump("{ let o = foo; o.checked }"), @r###"
        %0: Foo*
        %1: bool
    .0:
        %0 = copy [foo]: Foo*
        %1 = read_property %0: Foo*, "checked"
        return %1: bool
    "###);
}

#[test]
fn local_declaration_with_object_then_write_property() {
    insta::assert_snapshot!(dump("{ let o = foo; o.checked = true }"), @r###"
        %0: Foo*
    .0:
        %0 = copy [foo]: Foo*
        write_property %0: Foo*, "checked", true: bool
        return _: void
    "###);
}

#[test]
fn named_object_ref() {
    insta::assert_snapshot!(dump("foo"), @r###"
    .0:
        return [foo]: Foo*
    "###);
}

#[test]
fn read_object_property() {
    insta::assert_snapshot!(dump("foo.checked"), @r###"
        %0: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        return %0: bool
    "###);
}

#[test]
fn write_object_property() {
    insta::assert_snapshot!(dump("foo.checked = true"), @r###"
    .0:
        write_property [foo]: Foo*, "checked", true: bool
        return _: void
    "###);
}

#[test]
fn read_list_subscript() {
    insta::assert_snapshot!(dump("foo.stringList[0]"), @r###"
        %0: QStringList
        %1: QString
    .0:
        %0 = read_property [foo]: Foo*, "stringList"
        %1 = read_subscript %0: QStringList, 0: integer
        return %1: QString
    "###);
}

#[test]
fn write_list_subscript() {
    insta::assert_snapshot!(dump("{ let a = foo.stringList; a[0] = 'baz'; }"), @r###"
        %0: QStringList
        %1: QStringList
    .0:
        %0 = read_property [foo]: Foo*, "stringList"
        %1 = copy %0: QStringList
        write_subscript %1: QStringList, 0: integer, "baz": string
        return _: void
    "###);
}

#[test]
fn write_list_subscript_incompatible_type() {
    let env = Env::new();
    assert!(env.try_build("{ let a = ['foo']; a[0] = 0; }").is_err());
}

#[test]
fn write_list_subscript_rvalue() {
    let env = Env::new();
    assert!(env.try_build("{ foo.stringList[0] = 'baz'; }").is_err());
}

#[test]
fn call_object_method() {
    insta::assert_snapshot!(dump("foo.done(1)"), @r###"
    .0:
        call_method [foo]: Foo*, "done", {1: integer}
        return _: void
    "###);
}

#[test]
fn call_string_arg_method() {
    insta::assert_snapshot!(dump("'Hello %1'.arg('world')"), @r###"
        %0: QString
    .0:
        %0 = call_method "Hello %1": QString, "arg", {"world": QString}
        return %0: QString
    "###);
}

#[test]
fn call_string_is_empty_method() {
    insta::assert_snapshot!(dump("'Hello %1'.isEmpty()"), @r###"
        %0: bool
    .0:
        %0 = call_method "Hello %1": QString, "isEmpty", {}
        return %0: bool
    "###);
}

#[test]
fn call_array_is_empty_method() {
    insta::assert_snapshot!(dump("[0, 1, 2].isEmpty()"), @r###"
        %0: QList<int>
        %1: bool
    .0:
        %0 = make_list 'QList<int>', {0: integer, 1: integer, 2: integer}
        %1 = call_method %0: QList<int>, "isEmpty", {}
        return %1: bool
    "###);
}

#[test]
fn call_tr_function() {
    insta::assert_snapshot!(dump("qsTr('Hello')"), @r###"
        %0: QString
    .0:
        %0 = call_builtin_function Tr, {"Hello": string}
        return %0: QString
    "###);
}

#[test]
fn call_tr_function_on_dynamic_string() {
    let env = Env::new();
    assert!(env.try_build("qsTr(foo.text)").is_err());
}

#[test]
fn constant_number_arithmetic() {
    insta::assert_snapshot!(dump("(-1 + 2 * 3) / +4"), @r###"
    .0:
        return 1: integer
    "###);
    insta::assert_snapshot!(dump("(-1. + 2. * 3.) / +4."), @r###"
    .0:
        return 1.25: double
    "###);
}

#[test]
fn constant_string_concatenation() {
    insta::assert_snapshot!(dump("'foo' + 'bar'"), @r###"
    .0:
        return "foobar": QString
    "###);
}

#[test]
fn constant_bool_bitwise() {
    insta::assert_snapshot!(dump("false ^ true | false"), @r###"
    .0:
        return true: bool
    "###);
}

#[test]
fn dynamic_bool_bitwise() {
    insta::assert_snapshot!(dump("foo.checked ^ foo2.checked | foo3.checked"), @r###"
        %0: bool
        %1: bool
        %2: bool
        %3: bool
        %4: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        %1 = read_property [foo2]: Foo*, "checked"
        %2 = binary_op '^', %0: bool, %1: bool
        %3 = read_property [foo3]: Foo*, "checked"
        %4 = binary_op '|', %2: bool, %3: bool
        return %4: bool
    "###);
}

#[test]
fn dynamic_integer_arithmetic() {
    insta::assert_snapshot!(dump("-foo.currentIndex + 1"), @r###"
        %0: int
        %1: int
        %2: int
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        %1 = unary_op '-', %0: int
        %2 = binary_op '+', %1: int, 1: integer
        return %2: int
    "###);
}

#[test]
fn dynamic_string_concatenation() {
    insta::assert_snapshot!(dump("'Hello ' + foo.text"), @r###"
        %0: QString
        %1: QString
    .0:
        %0 = read_property [foo]: Foo*, "text"
        %1 = binary_op '+', "Hello ": QString, %0: QString
        return %1: QString
    "###);
}

#[test]
fn constant_integer_bitwise() {
    insta::assert_snapshot!(dump("((1 ^ 3) | 4) & ~0"), @r###"
    .0:
        return 6: integer
    "###);
}

#[test]
fn constant_enum_bitwise() {
    insta::assert_snapshot!(dump("(Foo.Bar0 ^ Foo.Bar1 | Foo.Bar2) & ~Foo.Bar3"), @r###"
        %0: Foo::Bar
        %1: Foo::Bar
        %2: Foo::Bar
        %3: Foo::Bar
    .0:
        %0 = binary_op '^', 'Foo::Bar0': Foo::Bar, 'Foo::Bar1': Foo::Bar
        %1 = binary_op '|', %0: Foo::Bar, 'Foo::Bar2': Foo::Bar
        %2 = unary_op '~', 'Foo::Bar3': Foo::Bar
        %3 = binary_op '&', %1: Foo::Bar, %2: Foo::Bar
        return %3: Foo::Bar
    "###);
}

#[test]
fn dynamic_integer_bitwise() {
    insta::assert_snapshot!(dump("~foo.currentIndex & foo2.currentIndex"), @r###"
        %0: int
        %1: int
        %2: int
        %3: int
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        %1 = unary_op '~', %0: int
        %2 = read_property [foo2]: Foo*, "currentIndex"
        %3 = binary_op '&', %1: int, %2: int
        return %3: int
    "###);
}

#[test]
fn constant_bool_logical() {
    insta::assert_snapshot!(dump("!true && true || false"), @r###"
        %0: bool
        %1: bool
    .0:
        %0 = copy false: bool
        br_cond false: bool, .1, .2
    .1:
        %0 = copy true: bool
        br .2
    .2:
        %1 = copy true: bool
        br_cond %0: bool, .4, .3
    .3:
        %1 = copy false: bool
        br .4
    .4:
        return %1: bool
    "###);
}

#[test]
fn dynamic_bool_logical() {
    insta::assert_snapshot!(dump("!foo.checked && foo2.checked || foo3.checked"), @r###"
        %0: bool
        %1: bool
        %2: bool
        %3: bool
        %4: bool
        %5: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        %1 = unary_op '!', %0: bool
        %3 = copy false: bool
        br_cond %1: bool, .1, .2
    .1:
        %2 = read_property [foo2]: Foo*, "checked"
        %3 = copy %2: bool
        br .2
    .2:
        %5 = copy true: bool
        br_cond %3: bool, .4, .3
    .3:
        %4 = read_property [foo3]: Foo*, "checked"
        %5 = copy %4: bool
        br .4
    .4:
        return %5: bool
    "###);
}

#[test]
fn dynamic_bool_logical_and_in_ternary_condition() {
    insta::assert_snapshot!(dump("foo !== null && foo.checked ? foo.text : ''"), @r###"
        %0: bool
        %1: bool
        %2: bool
        %3: QString
        %4: QString
    .0:
        %0 = binary_op '!=', [foo]: Foo*, nullptr: nullptr_t
        %2 = copy false: bool
        br_cond %0: bool, .1, .2
    .1:
        %1 = read_property [foo]: Foo*, "checked"
        %2 = copy %1: bool
        br .2
    .2:
        br_cond %2: bool, .3, .4
    .3:
        %3 = read_property [foo]: Foo*, "text"
        %4 = copy %3: QString
        br .5
    .4:
        %4 = copy "": QString
        br .5
    .5:
        return %4: QString
    "###);
}

#[test]
fn const_literal_comparison() {
    insta::assert_snapshot!(dump("1 == 1"), @r###"
    .0:
        return true: bool
    "###);
    insta::assert_snapshot!(dump("1 > 2"), @r###"
    .0:
        return false: bool
    "###);
    insta::assert_snapshot!(dump("'bar' <= 'baz'"), @r###"
    .0:
        return true: bool
    "###);
}

#[test]
fn const_enum_comparison() {
    insta::assert_snapshot!(dump("Foo.Bar1 == Foo.Bar2"), @r###"
        %0: bool
    .0:
        %0 = binary_op '==', 'Foo::Bar1': Foo::Bar, 'Foo::Bar2': Foo::Bar
        return %0: bool
    "###);
}

#[test]
fn dynamic_integer_comparison() {
    insta::assert_snapshot!(dump("foo.currentIndex > 0"), @r###"
        %0: int
        %1: bool
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        %1 = binary_op '>', %0: int, 0: integer
        return %1: bool
    "###);
}

#[test]
fn dynamic_string_comparison() {
    insta::assert_snapshot!(dump("'yoda' != foo.text"), @r###"
        %0: QString
        %1: bool
    .0:
        %0 = read_property [foo]: Foo*, "text"
        %1 = binary_op '!=', "yoda": QString, %0: QString
        return %1: bool
    "###);
}

#[test]
fn untyped_null_literal_comparison() {
    insta::assert_snapshot!(dump("null == null"), @r###"
    .0:
        return true: bool
    "###);
}

#[test]
fn pointer_comparison() {
    insta::assert_snapshot!(dump("foo == foo2"), @r###"
        %0: bool
    .0:
        %0 = binary_op '==', [foo]: Foo*, [foo2]: Foo*
        return %0: bool
    "###);
}

#[test]
fn pointer_to_null_literal_comparison() {
    insta::assert_snapshot!(dump("foo == null"), @r###"
        %0: bool
    .0:
        %0 = binary_op '==', [foo]: Foo*, nullptr: nullptr_t
        return %0: bool
    "###);
}

#[test]
fn pointer_comparison_no_implicit_upcast() {
    let env = Env::new();
    assert!(env.try_build("foo == foo_sub").is_err());
}

#[test]
fn pointer_comparison_with_explicit_upcast() {
    insta::assert_snapshot!(dump("foo == (foo_sub as Foo)"), @r###"
        %0: Foo*
        %1: bool
    .0:
        %0 = copy [foo_sub]: FooSub*
        %1 = binary_op '==', [foo]: Foo*, %0: Foo*
        return %1: bool
    "###);
}

#[test]
fn incompatible_pointer_comparison() {
    let env = Env::new();
    assert!(env.try_build("foo == bar").is_err());
}

#[test]
fn type_cast_noop() {
    insta::assert_snapshot!(dump("true as bool"), @r###"
    .0:
        return true: bool
    "###);
}

#[test]
fn type_cast_noop_string() {
    insta::assert_snapshot!(dump("'foo' as QString"), @r###"
    .0:
        return "foo": QString
    "###);
}

#[test]
fn type_cast_implicit_integer() {
    insta::assert_snapshot!(dump("1 as uint"), @r###"
        %0: uint
    .0:
        %0 = copy 1: integer
        return %0: uint
    "###);
}

#[test]
fn type_cast_integer_literal_as_double() {
    insta::assert_snapshot!(dump("1 as double"), @r###"
        %0: double
    .0:
        %0 = static_cast 'double', 1: integer
        return %0: double
    "###);
}

#[test]
fn type_cast_double_as_integer() {
    insta::assert_snapshot!(dump("1e3 as uint"), @r###"
        %0: uint
    .0:
        %0 = static_cast 'uint', 1000.0: double
        return %0: uint
    "###);
}

#[test]
fn type_cast_dynamic_integer_as_uint() {
    insta::assert_snapshot!(dump("foo.currentIndex as uint"), @r###"
        %0: int
        %1: uint
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        %1 = static_cast 'uint', %0: int
        return %1: uint
    "###);
}

#[test]
fn type_cast_dynamic_bool_as_int() {
    insta::assert_snapshot!(dump("foo.checked as int"), @r###"
        %0: bool
        %1: int
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        %1 = static_cast 'int', %0: bool
        return %1: int
    "###);
}

#[test]
fn type_cast_enum_as_int() {
    insta::assert_snapshot!(dump("Foo.Bar0 as int"), @r###"
        %0: int
    .0:
        %0 = static_cast 'int', 'Foo::Bar0': Foo::Bar
        return %0: int
    "###);
}

#[test]
fn type_cast_literal_as_void() {
    insta::assert_snapshot!(dump("0 as void"), @r###"
    .0:
        static_cast 'void', 0: integer
        return _: void
    "###);
}

#[test]
fn type_cast_dynamic_expr_as_void() {
    insta::assert_snapshot!(dump("foo.text as void"), @r###"
        %0: QString
    .0:
        %0 = read_property [foo]: Foo*, "text"
        static_cast 'void', %0: QString
        return _: void
    "###);
}

#[test]
fn type_cast_variant_as_int() {
    insta::assert_snapshot!(dump("foo.currentData as int"), @r###"
        %0: QVariant
        %1: int
    .0:
        %0 = read_property [foo]: Foo*, "currentData"
        %1 = variant_cast 'int', %0: QVariant
        return %1: int
    "###);
}

#[test]
fn type_cast_invalid() {
    let env = Env::new();
    assert!(env.try_build("'' as Foo").is_err());
}

#[test]
fn ternary_simple() {
    insta::assert_snapshot!(dump("foo.checked ? 1 : 2"), @r###"
        %0: bool
        %1: int
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = copy 1: integer
        br .3
    .2:
        %1 = copy 2: integer
        br .3
    .3:
        return %1: int
    "###);
}

#[test]
fn ternary_string_literal() {
    insta::assert_snapshot!(dump("foo.checked ? 'yes' : 'no'"), @r###"
        %0: bool
        %1: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = copy "yes": QString
        br .3
    .2:
        %1 = copy "no": QString
        br .3
    .3:
        return %1: QString
    "###);
}

#[test]
fn ternary_no_implicit_upcast() {
    let env = Env::new();
    assert!(env.try_build("foo.checked ? foo : foo_sub").is_err());
}

#[test]
fn ternary_with_explicit_upcast() {
    insta::assert_snapshot!(dump("foo.checked ? foo : (foo_sub as Foo)"), @r###"
        %0: bool
        %1: Foo*
        %2: Foo*
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %2 = copy [foo]: Foo*
        br .3
    .2:
        %1 = copy [foo_sub]: FooSub*
        %2 = copy %1: Foo*
        br .3
    .3:
        return %2: Foo*
    "###);
}

#[test]
fn ternary_dynamic_result() {
    insta::assert_snapshot!(dump("foo.checked ? foo.text : foo2.text"), @r###"
        %0: bool
        %1: QString
        %2: QString
        %3: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = read_property [foo]: Foo*, "text"
        %3 = copy %1: QString
        br .3
    .2:
        %2 = read_property [foo2]: Foo*, "text"
        %3 = copy %2: QString
        br .3
    .3:
        return %3: QString
    "###);
}

#[test]
fn ternary_nested_condition() {
    insta::assert_snapshot!(
        dump("(foo.checked ? foo2.checked : foo3.checked) ? foo2.text : foo3.text"), @r###"
        %0: bool
        %1: bool
        %2: bool
        %3: bool
        %4: QString
        %5: QString
        %6: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = read_property [foo2]: Foo*, "checked"
        %3 = copy %1: bool
        br .3
    .2:
        %2 = read_property [foo3]: Foo*, "checked"
        %3 = copy %2: bool
        br .3
    .3:
        br_cond %3: bool, .4, .5
    .4:
        %4 = read_property [foo2]: Foo*, "text"
        %6 = copy %4: QString
        br .6
    .5:
        %5 = read_property [foo3]: Foo*, "text"
        %6 = copy %5: QString
        br .6
    .6:
        return %6: QString
    "###);
}

#[test]
fn ternary_nested_consequence() {
    insta::assert_snapshot!(
        dump("foo.checked ? (foo2.checked ? foo.text : foo2.text) : foo3.text"), @r###"
        %0: bool
        %1: bool
        %2: QString
        %3: QString
        %4: QString
        %5: QString
        %6: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .5
    .1:
        %1 = read_property [foo2]: Foo*, "checked"
        br_cond %1: bool, .2, .3
    .2:
        %2 = read_property [foo]: Foo*, "text"
        %4 = copy %2: QString
        br .4
    .3:
        %3 = read_property [foo2]: Foo*, "text"
        %4 = copy %3: QString
        br .4
    .4:
        %6 = copy %4: QString
        br .6
    .5:
        %5 = read_property [foo3]: Foo*, "text"
        %6 = copy %5: QString
        br .6
    .6:
        return %6: QString
    "###);
}

#[test]
fn ternary_nested_alternative() {
    insta::assert_snapshot!(
        dump("foo.checked ? foo.text : (foo2.checked ? foo2.text : foo3.text)"), @r###"
        %0: bool
        %1: QString
        %2: bool
        %3: QString
        %4: QString
        %5: QString
        %6: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = read_property [foo]: Foo*, "text"
        %6 = copy %1: QString
        br .6
    .2:
        %2 = read_property [foo2]: Foo*, "checked"
        br_cond %2: bool, .3, .4
    .3:
        %3 = read_property [foo2]: Foo*, "text"
        %5 = copy %3: QString
        br .5
    .4:
        %4 = read_property [foo3]: Foo*, "text"
        %5 = copy %4: QString
        br .5
    .5:
        %6 = copy %5: QString
        br .6
    .6:
        return %6: QString
    "###);
}

#[test]
fn ternary_concatenation() {
    insta::assert_snapshot!(
        dump("(foo.checked ? foo.text : foo2.text) + (foo3.checked ? foo3.text : foo4.text)"), @r###"
        %0: bool
        %1: QString
        %2: QString
        %3: QString
        %4: bool
        %5: QString
        %6: QString
        %7: QString
        %8: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = read_property [foo]: Foo*, "text"
        %3 = copy %1: QString
        br .3
    .2:
        %2 = read_property [foo2]: Foo*, "text"
        %3 = copy %2: QString
        br .3
    .3:
        %4 = read_property [foo3]: Foo*, "checked"
        br_cond %4: bool, .4, .5
    .4:
        %5 = read_property [foo3]: Foo*, "text"
        %7 = copy %5: QString
        br .6
    .5:
        %6 = read_property [foo4]: Foo*, "text"
        %7 = copy %6: QString
        br .6
    .6:
        %8 = binary_op '+', %3: QString, %7: QString
        return %8: QString
    "###);
}

#[test]
fn multiple_statements() {
    insta::assert_snapshot!(
        dump("{ qsTr('hello'); foo.done(0); 'discarded'; foo.done(1); 'world' }"), @r###"
        %0: QString
    .0:
        %0 = call_builtin_function Tr, {"hello": string}
        call_method [foo]: Foo*, "done", {0: integer}
        call_method [foo]: Foo*, "done", {1: integer}
        return "world": QString
    "###);
}

#[test]
fn empty_statement() {
    insta::assert_snapshot!(
        dump(";"), @r###"
    .0:
        return _: void
    "###);
}

#[test]
fn if_statement_literal() {
    insta::assert_snapshot!(dump("if (foo.checked) { 'yes' }"), @r###"
        %0: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        return "yes": QString
    .2:
        return _: void
    "###);
}

#[test]
fn if_else_statement_literal() {
    insta::assert_snapshot!(dump("if (foo.checked) { 'yes' } else { 'no' }"), @r###"
        %0: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        return "yes": QString
    .2:
        return "no": QString
    .3:
        unreachable
    "###);
}

#[test]
fn if_else_statement_literal_incompatible_completion() {
    insta::assert_snapshot!(dump("if (foo.checked) { 'yes' } else { false }"), @r###"
        %0: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        return "yes": QString
    .2:
        return false: bool
    .3:
        unreachable
    "###);
}

#[test]
fn if_statement_call() {
    insta::assert_snapshot!(dump("if (foo.checked) { foo.done(0) }"), @r###"
        %0: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        call_method [foo]: Foo*, "done", {0: integer}
        return _: void
    .2:
        return _: void
    "###);
}

#[test]
fn if_else_statement_call() {
    insta::assert_snapshot!(
        dump("if (foo.checked) { foo.done(0); 'yes' } else { foo.done(1); 'no' }"), @r###"
        %0: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        call_method [foo]: Foo*, "done", {0: integer}
        return "yes": QString
    .2:
        call_method [foo]: Foo*, "done", {1: integer}
        return "no": QString
    .3:
        unreachable
    "###);
}

#[test]
fn if_else_statement_call_incompatible_completion() {
    insta::assert_snapshot!(
        dump("if (foo.checked) { foo.done(0); 'yes' } else { foo.done(1); false }"), @r###"
        %0: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        call_method [foo]: Foo*, "done", {0: integer}
        return "yes": QString
    .2:
        call_method [foo]: Foo*, "done", {1: integer}
        return false: bool
    .3:
        unreachable
    "###);
}

#[test]
fn if_else_statement_chained() {
    insta::assert_snapshot!(
        dump(r###"
        if (foo.checked) {
            foo.text
        } else if (foo2.checked) {
            foo2.text
        } else {
            foo3.text
        }
        "###), @r###"
        %0: bool
        %1: QString
        %2: bool
        %3: QString
        %4: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = read_property [foo]: Foo*, "text"
        return %1: QString
    .2:
        %2 = read_property [foo2]: Foo*, "checked"
        br_cond %2: bool, .3, .4
    .3:
        %3 = read_property [foo2]: Foo*, "text"
        return %3: QString
    .4:
        %4 = read_property [foo3]: Foo*, "text"
        return %4: QString
    .5:
        unreachable
    .6:
        unreachable
    "###);
}

#[test]
fn if_statement_ternary_in_condition() {
    insta::assert_snapshot!(
        dump("if (foo.checked ? foo2.checked : foo3.checked) { foo.done(0) }"), @r###"
        %0: bool
        %1: bool
        %2: bool
        %3: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = read_property [foo2]: Foo*, "checked"
        %3 = copy %1: bool
        br .3
    .2:
        %2 = read_property [foo3]: Foo*, "checked"
        %3 = copy %2: bool
        br .3
    .3:
        br_cond %3: bool, .4, .5
    .4:
        call_method [foo]: Foo*, "done", {0: integer}
        return _: void
    .5:
        return _: void
    "###);
}

#[test]
fn if_else_statement_ternary_in_condition() {
    insta::assert_snapshot!(
        dump("if (foo.checked ? foo2.checked : foo3.checked) { foo.done(0) } else { foo.done(1) }"),
        @r###"
        %0: bool
        %1: bool
        %2: bool
        %3: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = read_property [foo2]: Foo*, "checked"
        %3 = copy %1: bool
        br .3
    .2:
        %2 = read_property [foo3]: Foo*, "checked"
        %3 = copy %2: bool
        br .3
    .3:
        br_cond %3: bool, .4, .5
    .4:
        call_method [foo]: Foo*, "done", {0: integer}
        return _: void
    .5:
        call_method [foo]: Foo*, "done", {1: integer}
        return _: void
    .6:
        unreachable
    "###);
}

#[test]
fn if_else_statement_empty_body() {
    insta::assert_snapshot!(
        dump("if (foo.checked) {} else {}"),
        @r###"
        %0: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        return _: void
    .2:
        return _: void
    .3:
        unreachable
    "###);
}

#[test]
fn return_nothing() {
    insta::assert_snapshot!(dump("{ return; }"), @r###"
    .0:
        return _: void
    .1:
        unreachable
    "###);
}

#[test]
fn return_literal() {
    insta::assert_snapshot!(dump("{ return 'hello'; }"), @r###"
    .0:
        return "hello": QString
    .1:
        unreachable
    "###);
}

#[test]
fn return_dynamic_expression() {
    insta::assert_snapshot!(dump("{ return foo.checked; }"), @r###"
        %0: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        return %0: bool
    .1:
        unreachable
    "###);
}

#[test]
fn return_if() {
    insta::assert_snapshot!(
        dump(r###"{
        if (foo.checked)
            return foo.text;
        if (foo2.checked)
            return foo2.text;
        foo3.text
    }"###), @r###"
        %0: bool
        %1: QString
        %2: bool
        %3: QString
        %4: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .3
    .1:
        %1 = read_property [foo]: Foo*, "text"
        return %1: QString
    .2:
        br .3
    .3:
        %2 = read_property [foo2]: Foo*, "checked"
        br_cond %2: bool, .4, .6
    .4:
        %3 = read_property [foo2]: Foo*, "text"
        return %3: QString
    .5:
        br .6
    .6:
        %4 = read_property [foo3]: Foo*, "text"
        return %4: QString
    "###);
}

#[test]
fn return_if_else_commplete() {
    insta::assert_snapshot!(
        dump(r###"{
        if (foo.checked) {
            return foo.text;
        } else if (foo2.checked) {
            return foo2.text;
        } else {
            return foo3.text;
        }
    }"###), @r###"
        %0: bool
        %1: QString
        %2: bool
        %3: QString
        %4: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .3
    .1:
        %1 = read_property [foo]: Foo*, "text"
        return %1: QString
    .2:
        unreachable
    .3:
        %2 = read_property [foo2]: Foo*, "checked"
        br_cond %2: bool, .4, .6
    .4:
        %3 = read_property [foo2]: Foo*, "text"
        return %3: QString
    .5:
        unreachable
    .6:
        %4 = read_property [foo3]: Foo*, "text"
        return %4: QString
    .7:
        unreachable
    .8:
        unreachable
    .9:
        unreachable
    "###);
}

#[test]
fn return_if_else_partial() {
    insta::assert_snapshot!(
        dump(r###"{
        if (foo.checked) {
            return foo.text;
        } else if (foo2.checked) {
            foo2.text;
        } else {
            return foo3.text;
        }
    }"###), @r###"
        %0: bool
        %1: QString
        %2: bool
        %3: QString
        %4: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .3
    .1:
        %1 = read_property [foo]: Foo*, "text"
        return %1: QString
    .2:
        unreachable
    .3:
        %2 = read_property [foo2]: Foo*, "checked"
        br_cond %2: bool, .4, .5
    .4:
        %3 = read_property [foo2]: Foo*, "text"
        return %3: QString
    .5:
        %4 = read_property [foo3]: Foo*, "text"
        return %4: QString
    .6:
        unreachable
    .7:
        unreachable
    .8:
        unreachable
    "###);
}

#[test]
fn return_if_else_partial_and_trailing_code() {
    insta::assert_snapshot!(
        dump(r###"{
        if (foo.checked) {
            return foo.text;
        } else if (foo2.checked) {
            foo2.text;
        } else {
            return foo3.text;
        }
        foo4.text;
    }"###), @r###"
        %0: bool
        %1: QString
        %2: bool
        %3: QString
        %4: QString
        %5: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .3
    .1:
        %1 = read_property [foo]: Foo*, "text"
        return %1: QString
    .2:
        br .8
    .3:
        %2 = read_property [foo2]: Foo*, "checked"
        br_cond %2: bool, .4, .5
    .4:
        %3 = read_property [foo2]: Foo*, "text"
        br .7
    .5:
        %4 = read_property [foo3]: Foo*, "text"
        return %4: QString
    .6:
        br .7
    .7:
        br .8
    .8:
        %5 = read_property [foo4]: Foo*, "text"
        return %5: QString
    "###);
}

#[test]
fn return_with_trailing_garbage() {
    insta::assert_snapshot!(
        dump(r###"{
        if (foo.checked) {
            return foo.text;
            return;
        } else {
            { return foo2.text; }
            foo3.text;
        }
        foo4.text;
    }"###), @r###"
        %0: bool
        %1: QString
        %2: QString
        %3: QString
        %4: QString
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .4
    .1:
        %1 = read_property [foo]: Foo*, "text"
        return %1: QString
    .2:
        return _: void
    .3:
        br .6
    .4:
        %2 = read_property [foo2]: Foo*, "text"
        return %2: QString
    .5:
        %3 = read_property [foo3]: Foo*, "text"
        br .6
    .6:
        %4 = read_property [foo4]: Foo*, "text"
        return %4: QString
    "###);
}

#[test]
fn switch_basic() {
    insta::assert_snapshot!(
        dump(r###"{
        let s = foo.text;
        switch (s) {
        case "foo2":
            foo2.text;
            break;
        case "foo3":
            foo3.text;
            break;
        default:
            foo.text;
        }
    }"###), @r###"
        %0: QString
        %1: QString
        %2: bool
        %3: QString
        %4: bool
        %5: QString
        %6: QString
    .0:
        %0 = read_property [foo]: Foo*, "text"
        %1 = copy %0: QString
        br .2
    .1:
        unreachable
    .2:
        %2 = binary_op '==', %1: QString, "foo2": QString
        br_cond %2: bool, .3, .5
    .3:
        %3 = read_property [foo2]: Foo*, "text"
        return %3: QString
    .4:
        br .6
    .5:
        %4 = binary_op '==', %1: QString, "foo3": QString
        br_cond %4: bool, .6, .8
    .6:
        %5 = read_property [foo3]: Foo*, "text"
        return %5: QString
    .7:
        br .8
    .8:
        %6 = read_property [foo]: Foo*, "text"
        return %6: QString
    .9:
        unreachable
    "###);
}

#[test]
fn switch_default_only() {
    insta::assert_snapshot!(
        dump(r###"{
        switch (foo.currentIndex) {
        default:
            foo.done(0);
        }
        foo.done(-1);
    }"###), @r###"
        %0: int
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        br .2
    .1:
        br .3
    .2:
        call_method [foo]: Foo*, "done", {0: integer}
        br .3
    .3:
        call_method [foo]: Foo*, "done", {-1: integer}
        return _: void
    "###);
}

#[test]
fn switch_case_only() {
    insta::assert_snapshot!(
        dump(r###"{
        switch (foo.currentIndex) {
        case 1:
            foo.done(1);
            break;
        }
        foo.done(-1);
    }"###), @r###"
        %0: int
        %1: bool
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        br .2
    .1:
        br .5
    .2:
        %1 = binary_op '==', %0: int, 1: integer
        br_cond %1: bool, .3, .5
    .3:
        call_method [foo]: Foo*, "done", {1: integer}
        br .1
    .4:
        br .5
    .5:
        call_method [foo]: Foo*, "done", {-1: integer}
        return _: void
    "###);
}

#[test]
fn switch_default_first_fall_through() {
    insta::assert_snapshot!(
        dump(r###"{
        switch (foo.currentIndex) {
        default:
            foo.done(0);
        case 1:
            foo.done(1);
        case 2:
            foo.done(2);
        }
        foo.done(-1);
    }"###), @r###"
        %0: int
        %1: bool
        %2: bool
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        br .2
    .1:
        br .7
    .2:
        %1 = binary_op '==', %0: int, 1: integer
        br_cond %1: bool, .3, .4
    .3:
        call_method [foo]: Foo*, "done", {1: integer}
        br .5
    .4:
        %2 = binary_op '==', %0: int, 2: integer
        br_cond %2: bool, .5, .6
    .5:
        call_method [foo]: Foo*, "done", {2: integer}
        br .7
    .6:
        call_method [foo]: Foo*, "done", {0: integer}
        br .3
    .7:
        call_method [foo]: Foo*, "done", {-1: integer}
        return _: void
    "###);
}

#[test]
fn switch_default_mid_fall_through() {
    insta::assert_snapshot!(
        dump(r###"{
        switch (foo.currentIndex) {
        case 1:
            foo.done(1);
        default:
            foo.done(0);
        case 2:
            foo.done(2);
        }
        foo.done(-1);
    }"###), @r###"
        %0: int
        %1: bool
        %2: bool
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        br .2
    .1:
        br .7
    .2:
        %1 = binary_op '==', %0: int, 1: integer
        br_cond %1: bool, .3, .4
    .3:
        call_method [foo]: Foo*, "done", {1: integer}
        br .6
    .4:
        %2 = binary_op '==', %0: int, 2: integer
        br_cond %2: bool, .5, .6
    .5:
        call_method [foo]: Foo*, "done", {2: integer}
        br .7
    .6:
        call_method [foo]: Foo*, "done", {0: integer}
        br .5
    .7:
        call_method [foo]: Foo*, "done", {-1: integer}
        return _: void
    "###);
}

#[test]
fn switch_default_end_fall_through() {
    insta::assert_snapshot!(
        dump(r###"{
        switch (foo.currentIndex) {
        case 1:
            foo.done(1);
        case 2:
            foo.done(2);
        default:
            foo.done(0);
        }
        foo.done(-1);
    }"###), @r###"
        %0: int
        %1: bool
        %2: bool
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        br .2
    .1:
        br .7
    .2:
        %1 = binary_op '==', %0: int, 1: integer
        br_cond %1: bool, .3, .4
    .3:
        call_method [foo]: Foo*, "done", {1: integer}
        br .5
    .4:
        %2 = binary_op '==', %0: int, 2: integer
        br_cond %2: bool, .5, .6
    .5:
        call_method [foo]: Foo*, "done", {2: integer}
        br .6
    .6:
        call_method [foo]: Foo*, "done", {0: integer}
        br .7
    .7:
        call_method [foo]: Foo*, "done", {-1: integer}
        return _: void
    "###);
}

#[test]
fn switch_empty_fall_through() {
    insta::assert_snapshot!(
        dump(r###"{
        switch (foo.currentIndex) {
        case 1:
        case 2:
            foo.done(1);
            break;
        case 3:
            foo.done(3);
            break;
        }
        foo.done(-1);
    }"###), @r###"
        %0: int
        %1: bool
        %2: bool
        %3: bool
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        br .2
    .1:
        br .10
    .2:
        %1 = binary_op '==', %0: int, 1: integer
        br_cond %1: bool, .3, .4
    .3:
        br .5
    .4:
        %2 = binary_op '==', %0: int, 2: integer
        br_cond %2: bool, .5, .7
    .5:
        call_method [foo]: Foo*, "done", {1: integer}
        br .1
    .6:
        br .8
    .7:
        %3 = binary_op '==', %0: int, 3: integer
        br_cond %3: bool, .8, .10
    .8:
        call_method [foo]: Foo*, "done", {3: integer}
        br .1
    .9:
        br .10
    .10:
        call_method [foo]: Foo*, "done", {-1: integer}
        return _: void
    "###);
}

#[test]
fn switch_return() {
    insta::assert_snapshot!(
        dump(r###"{
        switch (foo.currentIndex) {
        case 0:
            return "0";
        case 1:
            return "1";
        default:
            return "default";
        }
        "unreachable"
    }"###), @r###"
        %0: int
        %1: bool
        %2: bool
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        br .2
    .1:
        br .10
    .2:
        %1 = binary_op '==', %0: int, 0: integer
        br_cond %1: bool, .3, .5
    .3:
        return "0": QString
    .4:
        br .6
    .5:
        %2 = binary_op '==', %0: int, 1: integer
        br_cond %2: bool, .6, .8
    .6:
        return "1": QString
    .7:
        br .8
    .8:
        return "default": QString
    .9:
        br .10
    .10:
        return "unreachable": QString
    "###);
}

#[test]
fn switch_conditional_break() {
    insta::assert_snapshot!(
        dump(r###"{
        switch (foo.currentIndex) {
        case 2:
            if (foo2.checked)
                break;
            return "2";
        case 3:
            if (foo3.checked)
                break;
            return "3";
        default:
            if (foo.checked)
                break;
            return "default";
        }
        "break"
    }"###), @r###"
        %0: int
        %1: bool
        %2: bool
        %3: bool
        %4: bool
        %5: bool
    .0:
        %0 = read_property [foo]: Foo*, "currentIndex"
        br .2
    .1:
        br .19
    .2:
        %1 = binary_op '==', %0: int, 2: integer
        br_cond %1: bool, .3, .8
    .3:
        %2 = read_property [foo2]: Foo*, "checked"
        br_cond %2: bool, .4, .6
    .4:
        br .1
    .5:
        br .6
    .6:
        return "2": QString
    .7:
        br .9
    .8:
        %3 = binary_op '==', %0: int, 3: integer
        br_cond %3: bool, .9, .14
    .9:
        %4 = read_property [foo3]: Foo*, "checked"
        br_cond %4: bool, .10, .12
    .10:
        br .1
    .11:
        br .12
    .12:
        return "3": QString
    .13:
        br .14
    .14:
        %5 = read_property [foo]: Foo*, "checked"
        br_cond %5: bool, .15, .17
    .15:
        br .1
    .16:
        br .17
    .17:
        return "default": QString
    .18:
        br .19
    .19:
        return "break": QString
    "###);
}

#[test]
fn switch_ternary_in_left_value() {
    insta::assert_snapshot!(
        dump(r###"{
        switch (foo.checked ? foo.currentIndex : foo2.currentIndex) {
        case 1:
            foo.done(1);
            break;
        case 2:
            foo.done(2);
            break;
        }
        foo.done(-1);
    }"###), @r###"
        %0: bool
        %1: int
        %2: int
        %3: int
        %4: bool
        %5: bool
    .0:
        %0 = read_property [foo]: Foo*, "checked"
        br_cond %0: bool, .1, .2
    .1:
        %1 = read_property [foo]: Foo*, "currentIndex"
        %3 = copy %1: int
        br .3
    .2:
        %2 = read_property [foo2]: Foo*, "currentIndex"
        %3 = copy %2: int
        br .3
    .3:
        br .5
    .4:
        br .11
    .5:
        %4 = binary_op '==', %3: int, 1: integer
        br_cond %4: bool, .6, .8
    .6:
        call_method [foo]: Foo*, "done", {1: integer}
        br .4
    .7:
        br .9
    .8:
        %5 = binary_op '==', %3: int, 2: integer
        br_cond %5: bool, .9, .11
    .9:
        call_method [foo]: Foo*, "done", {2: integer}
        br .4
    .10:
        br .11
    .11:
        call_method [foo]: Foo*, "done", {-1: integer}
        return _: void
    "###);
}

#[test]
fn callback_block() {
    let env = Env::new();
    let code = env.build_callback(
        r###"{
            foo.done(0)
        }"###,
    );
    insta::assert_snapshot!(dump_code(&code), @r###"
    .0:
        call_method [foo]: Foo*, "done", {0: integer}
        return _: void
    "###);
    assert_eq!(code.parameter_count, 0);
}

#[test]
fn callback_expr() {
    let env = Env::new();
    let code = env.build_callback("foo.done(0)");
    insta::assert_snapshot!(dump_code(&code), @r###"
    .0:
        call_method [foo]: Foo*, "done", {0: integer}
        return _: void
    "###);
    assert_eq!(code.parameter_count, 0);
}

#[test]
fn callback_function_no_arg() {
    let env = Env::new();
    let code = env.build_callback(
        r###"function() {
            foo.done(0)
        }"###,
    );
    insta::assert_snapshot!(dump_code(&code), @r###"
    .0:
        call_method [foo]: Foo*, "done", {0: integer}
        return _: void
    "###);
    assert_eq!(code.parameter_count, 0);
}

#[test]
fn callback_function_with_arg() {
    let env = Env::new();
    let code = env.build_callback(
        r###"function(obj: Foo, code: int) {
            obj.done(code);
        }"###,
    );
    insta::assert_snapshot!(dump_code(&code), @r###"
        %0: Foo*
        %1: int
    .0:
        call_method %0: Foo*, "done", {%1: int}
        return _: void
    "###);
    assert_eq!(code.parameter_count, 2);
}

#[test]
fn callback_arrow_function_no_arg_no_block() {
    let env = Env::new();
    let code = env.build_callback("() => foo.done(0)");
    insta::assert_snapshot!(dump_code(&code), @r###"
    .0:
        call_method [foo]: Foo*, "done", {0: integer}
        return _: void
    "###);
    assert_eq!(code.parameter_count, 0);
}

#[test]
fn callback_function_parenthesized() {
    let env = Env::new();
    assert!(env.try_build_callback("(function() {})").is_err());
}
