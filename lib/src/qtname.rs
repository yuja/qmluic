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

    pub fn type_name_to_ui_cxx_header_name<S>(&self, type_name: S) -> String
    where
        S: AsRef<str>,
    {
        self.apply_case_change(format!(
            "ui_{}.{}",
            type_name.as_ref(),
            &self.cxx_header_suffix
        ))
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

/// Generates variable name candidate for the given type.
///
/// The `type_name` should be a valid identifier. `Scoped::Name` is invalid for example.
///
/// This follows the convention of `Driver::qtify()` defined in
/// `qtbase/src/tools/uic/driver.cpp`, but does not return an empty string unless the given
/// `type_name` is empty.
pub fn variable_name_for_type<S>(type_name: S) -> String
where
    S: AsRef<str>,
{
    let type_name = type_name.as_ref();
    let mut var_name = String::with_capacity(type_name.len());
    let mut chars = type_name.chars().fuse();
    if type_name.starts_with(|c| c == 'Q' || c == 'K')
        && type_name[1..].starts_with(|c: char| c.is_ascii_alphabetic())
    {
        chars.next();
    }
    for c in chars.by_ref() {
        var_name.push(c.to_ascii_lowercase());
        if !c.is_ascii_uppercase() {
            break;
        }
    }
    var_name.extend(chars);
    var_name
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

    #[test]
    fn qtified_variable_name() {
        assert_eq!(variable_name_for_type("QMainWindow"), "mainWindow");
        assert_eq!(variable_name_for_type("QVBoxLayout"), "vboxLayout");
        assert_eq!(variable_name_for_type("Q"), "q");
        assert_eq!(variable_name_for_type("Q3D"), "q3D");
    }
}
