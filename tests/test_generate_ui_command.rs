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

    let a = env
        .generate_ui_cmd(["--no-dynamic-binding", "Simple.qml"])
        .assert()
        .success();
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
    assert!(!env.join("uisupport_simple.h").exists());
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
    assert!(env.join("out/put/uisupport_base.h").exists());
    assert!(env.join("out/put/dir/nested.ui").exists());
    assert!(env.join("out/put/dir/uisupport_nested.h").exists());
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
      ┌─ PropError.qml:3:5
      │
    3 │     unknown: ""
      │     ^^^^^^^^^^^

    "###);

    assert!(!env.join("properror.ui").exists());
}

#[test]
fn test_syntax_error_general() {
    let env = TestEnv::prepare();
    env.write_dedent(
        "SyntaxError.qml",
        r###"
        import qmluic.QtWidgets
        QDialog { windowTitle: "what" "ever" }
        "###,
    );

    let a = env.generate_ui_cmd(["SyntaxError.qml"]).assert().failure();
    insta::assert_snapshot!(env.replace_base_path(str::from_utf8(&a.get_output().stderr).unwrap()), @r###"
    processing SyntaxError.qml
    error: syntax error
      ┌─ SyntaxError.qml:2:24
      │
    2 │ QDialog { windowTitle: "what" "ever" }
      │                        ^^^^^^

    "###);

    assert!(!env.join("syntaxerror.ui").exists());
}

#[test]
fn test_syntax_error_missing() {
    let env = TestEnv::prepare();
    env.write_dedent(
        "SyntaxError.qml",
        r###"
        import qmluic.QtWidgets
        QDialog { windowTitle: "what"
        "###,
    );

    let a = env.generate_ui_cmd(["SyntaxError.qml"]).assert().failure();
    insta::assert_snapshot!(env.replace_base_path(str::from_utf8(&a.get_output().stderr).unwrap()), @r###"
    processing SyntaxError.qml
    error: syntax error: missing }
      ┌─ SyntaxError.qml:2:30
      │
    2 │ QDialog { windowTitle: "what"
      │                              ^ missing }

    "###);

    assert!(!env.join("syntaxerror.ui").exists());
}

#[test]
fn test_warning() {
    let env = TestEnv::prepare();
    env.write_dedent(
        "Warning.qml",
        r###"
        import qmluic.QtWidgets 6.2
        QDialog {}
        "###,
    );

    let a = env.generate_ui_cmd(["Warning.qml"]).assert().success();
    insta::assert_snapshot!(env.replace_base_path(str::from_utf8(&a.get_output().stderr).unwrap()), @r###"
    processing Warning.qml
    warning: import version is ignored
      ┌─ Warning.qml:1:1
      │
    1 │ import qmluic.QtWidgets 6.2
      │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^

    "###);

    assert!(env.join("warning.ui").exists());
}
