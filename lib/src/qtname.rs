//! Utility for Qt naming convention.

use std::collections::HashMap;

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

    pub fn type_name_to_ui_support_cxx_header_name<S>(&self, type_name: S) -> String
    where
        S: AsRef<str>,
    {
        self.apply_case_change(format!(
            "uisupport_{}.{}",
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

/// Generates unique name with string prefixes.
///
/// The naming rule follows `Driver::unique()` defined in `qtbase/src/tools/uic/driver.cpp`.
#[derive(Clone, Debug, Default)]
pub struct UniqueNameGenerator {
    used_prefixes: HashMap<String, usize>, // prefix: next count
}

impl UniqueNameGenerator {
    pub fn new() -> Self {
        UniqueNameGenerator::default()
    }

    /// Generates unique name starting with the given `prefix`.
    pub fn generate<S>(&mut self, prefix: S) -> String
    where
        S: AsRef<str>,
    {
        let prefix = prefix.as_ref();
        let count = self.used_prefixes.entry(prefix.to_owned()).or_insert(0);
        let id = concat_number_suffix(prefix, *count);
        *count += 1;
        id
    }

    /// Generates unique name starting with the given `prefix`, and not listed in
    /// the reserved map.
    pub fn generate_with_reserved_map<S, V>(
        &mut self,
        prefix: S,
        reserved_map: &HashMap<String, V>,
    ) -> String
    where
        S: AsRef<str>,
    {
        let prefix = prefix.as_ref();
        let count = self.used_prefixes.entry(prefix.to_owned()).or_insert(0);
        let (n, id) = (*count..=*count + reserved_map.len())
            .find_map(|n| {
                let id = concat_number_suffix(prefix, n);
                if reserved_map.contains_key(&id) {
                    None
                } else {
                    Some((n, id))
                }
            })
            .expect("unused id must be found within N+1 tries");
        *count = n + 1;
        id
    }
}

fn concat_number_suffix(prefix: &str, n: usize) -> String {
    if n == 0 {
        prefix.to_owned()
    } else {
        format!("{prefix}{n}")
    }
}

/// Creates a copy of the given string which the first character is turned into ASCII upper case.
pub fn to_ascii_capitalized(name: &str) -> String {
    if name.starts_with(|c: char| c.is_ascii_lowercase()) {
        let src_bytes = name.as_bytes();
        let mut capitalized = Vec::with_capacity(src_bytes.len());
        capitalized.push(src_bytes[0].to_ascii_uppercase());
        capitalized.extend_from_slice(&src_bytes[1..]);
        String::from_utf8(capitalized).expect("changing ASCII letter should not invalidate UTF-8")
    } else {
        name.to_owned()
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
