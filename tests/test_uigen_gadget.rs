pub mod common;

#[test]
fn test_unpaired_size_policy() {
    insta::assert_debug_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QWidget {
        sizePolicy.horizontalPolicy: QSizePolicy.Expanding
    }
    "###).unwrap_err(), @r###"
    Diagnostics {
        diagnostics: [
            Diagnostic {
                kind: Error,
                byte_range: 51..101,
                message: "both horizontal and vertical policies must be specified",
            },
        ],
    }
    "###);
}
