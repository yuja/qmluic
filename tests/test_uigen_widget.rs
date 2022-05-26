pub mod common;

#[test]
fn test_reserved_word_workaround() {
    insta::assert_snapshot!(common::translate_str(r###"
    import qmluic.QtWidgets
    QPushButton { default_: true }
    "###).unwrap(), @r###"
    <ui version="4.0">
     <widget class="QPushButton">
      <property name="default">
       <bool>true</bool>
      </property>
     </widget>
    </ui>
    "###);
}
