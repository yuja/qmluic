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
     <widget class="QDialog" name="dialog">
     </widget>
    </ui>
    "###);
}

#[test]
fn test_output_directory() {
    let env = TestEnv::prepare();
    env.write_dedent("Base.qml", "import qmluic.QtWidgets; QWidget {}");
    env.write_dedent("dir/Nested.qml", "import qmluic.QtWidgets; QWidget {}");

    env.generate_ui_cmd(["-O", "out/put", "Base.qml", "dir/Nested.qml"])
        .assert()
        .success();
    assert!(env.join("out/put/base.ui").exists());
    assert!(env.join("out/put/dir/nested.ui").exists());
}

#[test]
fn test_output_directory_absolute() {
    let env = TestEnv::prepare();
    env.write_dedent("Foo.qml", "import qmluic.QtWidgets; QWidget {}");
    let a = env
        .generate_ui_cmd(["-O", "out", env.join("Foo.qml").to_str().unwrap()])
        .assert()
        .failure();
    insta::assert_snapshot!(env.replace_base_path(str::from_utf8(&a.get_output().stderr).unwrap()), @r###"
    error: source file paths must be relative if --output-directory is specified
    "###);
}

#[test]
fn test_output_directory_escaped() {
    let env = TestEnv::prepare();
    env.write_dedent("Foo.qml", "import qmluic.QtWidgets; QWidget {}");
    env.create_dir_all("dir");
    let a = env
        .generate_ui_cmd(["-O", "out", "./../Foo.qml"])
        .current_dir(env.join("dir"))
        .assert()
        .failure();
    insta::assert_snapshot!(env.replace_base_path(str::from_utf8(&a.get_output().stderr).unwrap()), @r###"
    error: source file paths must be relative if --output-directory is specified
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
