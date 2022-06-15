use self::common::TestEnv;
use std::str;

pub mod common;

#[test]
fn test_simple_file() {
    let env = TestEnv::prepare();
    env.write_dedent(
        "Simple.qml",
        r###"
        import qmluic.QtWidgets
        QDialog {}
        "###,
    );

    let a = env.generate_ui_cmd(["Simple.qml"]).assert().success();
    insta::assert_snapshot!(str::from_utf8(&a.get_output().stderr).unwrap(), @r###"
    processing Simple.qml
    "###);

    insta::assert_snapshot!(env.read_to_string("simple.ui"), @r###"
    <ui version="4.0">
     <class>Simple</class>
     <widget class="QDialog">
     </widget>
    </ui>
    "###);
}

#[test]
fn test_property_error() {
    let env = TestEnv::prepare();
    env.write_dedent(
        "PropError.qml",
        r###"
        import qmluic.QtWidgets
        QDialog {
            unknown: ""
        }
        "###,
    );

    let a = env.generate_ui_cmd(["PropError.qml"]).assert().failure();
    insta::assert_snapshot!(env.replace_base_path(str::from_utf8(&a.get_output().stderr).unwrap()), @r###"
    processing PropError.qml
    error: unknown property of class 'QDialog': unknown
      ┌─ $BASE_PATH/PropError.qml:3:5
      │
    3 │     unknown: ""
      │     ^^^^^^^^^^^ unknown property of class 'QDialog': unknown

    "###);

    assert!(!env.join("properror.ui").exists());
}
