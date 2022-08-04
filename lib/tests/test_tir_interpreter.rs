use self::tir_testenv::*;
use qmluic::tir;
use qmluic::tir::interpret::{EvaluatedValue, StringKind};

pub mod tir_testenv;

fn try_eval(expr_source: &str) -> Option<EvaluatedValue> {
    let env = Env::new();
    let code = env.build(expr_source);
    tir::evaluate_code(&code)
}

fn eval(expr_source: &str) -> EvaluatedValue {
    try_eval(expr_source).unwrap()
}

#[test]
fn integer_math() {
    assert_eq!(eval("1 - 2 * (-3)").unwrap_integer(), 7);
    assert_eq!(eval("7 / 3").unwrap_integer(), 2);
}

#[test]
fn string() {
    assert_eq!(
        eval("'foo' + 'bar'").unwrap_string(),
        ("foobar".to_owned(), StringKind::NoTr)
    );
    assert_eq!(
        eval("qsTr('foo' + 'bar')").unwrap_string(),
        ("foobar".to_owned(), StringKind::Tr)
    );
    assert!(try_eval("qsTr('foo') + qsTr('bar')").is_none());
    assert_eq!(
        eval("['foo', qsTr('bar')]").unwrap_string_list(),
        [
            ("foo".to_owned(), StringKind::NoTr),
            ("bar".to_owned(), StringKind::Tr)
        ]
    );
}

#[test]
fn enum_set() {
    assert_eq!(eval("Foo.Bar0").unwrap_enum_set(), ["Foo::Bar0"]);
    assert_eq!(
        eval("Foo.Bar0 | Foo.Bar1").unwrap_enum_set(),
        ["Foo::Bar0", "Foo::Bar1"]
    );
    assert!(try_eval("Foo.Bar0 & Foo.Bar1").is_none());
}

#[test]
fn object_ref() {
    assert_eq!(eval("foo").unwrap_object_ref(), "foo");
    assert_eq!(
        eval("[foo, foo2]").into_object_ref_list().unwrap(),
        ["foo", "foo2"]
    );
}
