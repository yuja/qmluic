use camino::Utf8PathBuf;
use std::ffi::OsStr;
use std::fmt;
use std::io;
use std::process::Command;
use std::str;
use thiserror::Error;

/// Information about Qt installation.
#[derive(Clone, Debug, Default)]
pub struct QtPaths {
    pub sysroot: Option<Utf8PathBuf>,
    pub install_prefix: Option<Utf8PathBuf>,
    pub install_archdata: Option<Utf8PathBuf>,
    pub install_data: Option<Utf8PathBuf>,
    pub install_docs: Option<Utf8PathBuf>,
    pub install_headers: Option<Utf8PathBuf>,
    pub install_libs: Option<Utf8PathBuf>,
    pub install_libexecs: Option<Utf8PathBuf>,
    pub install_bins: Option<Utf8PathBuf>,
    pub install_tests: Option<Utf8PathBuf>,
    pub install_plugins: Option<Utf8PathBuf>,
    pub install_imports: Option<Utf8PathBuf>,
    pub install_qml: Option<Utf8PathBuf>,
    pub install_translations: Option<Utf8PathBuf>,
    pub install_configuration: Option<Utf8PathBuf>,
    pub install_examples: Option<Utf8PathBuf>,
    pub install_demos: Option<Utf8PathBuf>,
    pub host_prefix: Option<Utf8PathBuf>,
    pub host_data: Option<Utf8PathBuf>,
    pub host_bins: Option<Utf8PathBuf>,
    pub host_libs: Option<Utf8PathBuf>,
    // QMAKE_SPEC:linux-g++
    // QMAKE_XSPEC:linux-g++
    // QMAKE_VERSION:3.1
    pub version: Option<QtVersion>,
}

impl QtPaths {
    /// Queries paths by executing `qmake -query`.
    pub fn query() -> Result<Self, QueryError> {
        Self::query_with("qmake")
    }

    pub fn query_with<S>(program: S) -> Result<Self, QueryError>
    where
        S: AsRef<OsStr>,
    {
        let mut cmd = Command::new(program);
        cmd.arg("-query");
        log::debug!("executing {cmd:?}");
        let output = cmd.output()?;
        if !output.status.success() {
            return Err(QueryError::CommandFailed(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }
        Self::parse(str::from_utf8(&output.stdout)?)
    }

    fn parse(data: &str) -> Result<Self, QueryError> {
        let mut paths = QtPaths::default();
        for line in data.lines() {
            if let Some((k, v)) = line.split_once(':') {
                match k {
                    "QT_SYSROOT" => paths.sysroot = to_path_buf(v),
                    "QT_INSTALL_PREFIX" => paths.install_prefix = to_path_buf(v),
                    "QT_INSTALL_ARCHDATA" => paths.install_archdata = to_path_buf(v),
                    "QT_INSTALL_DATA" => paths.install_data = to_path_buf(v),
                    "QT_INSTALL_DOCS" => paths.install_docs = to_path_buf(v),
                    "QT_INSTALL_HEADERS" => paths.install_headers = to_path_buf(v),
                    "QT_INSTALL_LIBS" => paths.install_libs = to_path_buf(v),
                    "QT_INSTALL_LIBEXECS" => paths.install_libexecs = to_path_buf(v),
                    "QT_INSTALL_BINS" => paths.install_bins = to_path_buf(v),
                    "QT_INSTALL_TESTS" => paths.install_tests = to_path_buf(v),
                    "QT_INSTALL_PLUGINS" => paths.install_plugins = to_path_buf(v),
                    "QT_INSTALL_IMPORTS" => paths.install_imports = to_path_buf(v),
                    "QT_INSTALL_QML" => paths.install_qml = to_path_buf(v),
                    "QT_INSTALL_TRANSLATIONS" => paths.install_translations = to_path_buf(v),
                    "QT_INSTALL_CONFIGURATION" => paths.install_configuration = to_path_buf(v),
                    "QT_INSTALL_EXAMPLES" => paths.install_examples = to_path_buf(v),
                    "QT_INSTALL_DEMOS" => paths.install_demos = to_path_buf(v),
                    "QT_HOST_PREFIX" => paths.host_prefix = to_path_buf(v),
                    "QT_HOST_DATA" => paths.host_data = to_path_buf(v),
                    "QT_HOST_BINS" => paths.host_bins = to_path_buf(v),
                    "QT_HOST_LIBS" => paths.host_libs = to_path_buf(v),
                    "QT_VERSION" => paths.version = Some(v.try_into()?),
                    _ => {}
                }
            }
        }
        Ok(paths)
    }
}

#[derive(Debug, Error)]
pub enum QueryError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("qmake -query failed: {0}")]
    CommandFailed(String),
    #[error("invalid content: {0}")]
    InvalidData(#[from] str::Utf8Error),
    #[error("invalid version digits: {0}")]
    ParseVersion(String),
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct QtVersion {
    pub major: u8,
    pub minor: u8,
    pub patch: u8,
}

impl TryFrom<&str> for QtVersion {
    type Error = QueryError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let parse_one = |x: Option<&str>| {
            x.and_then(|s| s.parse().ok())
                .ok_or_else(|| QueryError::ParseVersion(value.to_owned()))
        };
        let mut digits = value.splitn(3, '.');
        let major = parse_one(digits.next())?;
        let minor = parse_one(digits.next())?;
        let patch = parse_one(digits.next())?;
        if digits.next().is_none() {
            Ok(QtVersion {
                major,
                minor,
                patch,
            })
        } else {
            Err(QueryError::ParseVersion(value.to_owned()))
        }
    }
}

impl fmt::Display for QtVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

fn to_path_buf(s: &str) -> Option<Utf8PathBuf> {
    if s.is_empty() {
        None
    } else {
        Some(Utf8PathBuf::from(s))
    }
}
