//! Utility for Qt naming convention.

/// Checks if the property name follows the standard setter function naming convention.
///
/// See `PropertyDef::stdCppSet()` in `qtbase/src/tools/moc/moc.h` for details.
pub fn is_std_set_property(name: &str, write_func_name: Option<&str>) -> bool {
    if let (Some(f), Some(h)) = (write_func_name, name.chars().next()) {
        // f == set<Name>
        f.starts_with("set")
            && f[3..].starts_with(h.to_ascii_uppercase())
            && f[(3 + h.len_utf8())..] == name[h.len_utf8()..]
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn std_set_property_name() {
        assert!(is_std_set_property("std", Some("setStd")));
        assert!(!is_std_set_property("std", Some("updateStd")));
        assert!(!is_std_set_property("std", None));
        assert!(!is_std_set_property("", Some("set")));
    }
}
