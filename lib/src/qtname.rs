//! Utility for Qt naming convention.

/// File naming rules.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FileNameRules {
    pub cxx_header_suffix: String,
    pub lowercase: bool,
}

impl FileNameRules {
    pub fn type_name_to_cxx_header_name<S>(&self, type_name: S) -> String
    where
        S: AsRef<str>,
    {
        self.apply_case_change(format!(
            "{}.{}",
            type_name.as_ref(),
            &self.cxx_header_suffix
        ))
    }

    pub fn type_name_to_ui_name<S>(&self, type_name: S) -> String
    where
        S: AsRef<str>,
    {
        self.apply_case_change(format!("{}.ui", type_name.as_ref()))
    }

    fn apply_case_change(&self, mut file_name: String) -> String {
        if self.lowercase {
            file_name.make_ascii_lowercase();
        }
        file_name
    }
}

impl Default for FileNameRules {
    fn default() -> Self {
        Self {
            cxx_header_suffix: "h".to_owned(),
            lowercase: true,
        }
    }
}

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
