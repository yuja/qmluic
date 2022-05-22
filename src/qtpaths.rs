use std::io;
use std::path::PathBuf;
use std::process::Command;
use std::str;

/// Information about Qt installation.
#[derive(Clone, Debug, Default)]
pub struct QtPaths {
    pub sysroot: Option<PathBuf>,
    pub install_prefix: Option<PathBuf>,
    pub install_archdata: Option<PathBuf>,
    pub install_data: Option<PathBuf>,
    pub install_docs: Option<PathBuf>,
    pub install_headers: Option<PathBuf>,
    pub install_libs: Option<PathBuf>,
    pub install_libexecs: Option<PathBuf>,
    pub install_bins: Option<PathBuf>,
    pub install_tests: Option<PathBuf>,
    pub install_plugins: Option<PathBuf>,
    pub install_imports: Option<PathBuf>,
    pub install_qml: Option<PathBuf>,
    pub install_translations: Option<PathBuf>,
    pub install_configuration: Option<PathBuf>,
    pub install_examples: Option<PathBuf>,
    pub install_demos: Option<PathBuf>,
    pub host_prefix: Option<PathBuf>,
    pub host_data: Option<PathBuf>,
    pub host_bins: Option<PathBuf>,
    pub host_libs: Option<PathBuf>,
    // QMAKE_SPEC:linux-g++
    // QMAKE_XSPEC:linux-g++
    // QMAKE_VERSION:3.1
    // QT_VERSION:5.15.2
}

impl QtPaths {
    /// Queries paths by executing `qmake -query`.
    pub fn query() -> io::Result<Self> {
        let output = Command::new("qmake").arg("-query").output()?;
        if !output.status.success() {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "qmake -query failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                ),
            ));
        }
        Ok(Self::parse(str::from_utf8(&output.stdout).map_err(
            |e| io::Error::new(io::ErrorKind::InvalidData, e),
        )?))
    }

    fn parse(data: &str) -> Self {
        let mut paths = QtPaths::default();
        for line in data.lines() {
            match line.split_once(':') {
                Some(("QT_SYSROOT", v)) => paths.sysroot = to_path_buf(v),
                Some(("QT_INSTALL_PREFIX", v)) => paths.install_prefix = to_path_buf(v),
                Some(("QT_INSTALL_ARCHDATA", v)) => paths.install_archdata = to_path_buf(v),
                Some(("QT_INSTALL_DATA", v)) => paths.install_data = to_path_buf(v),
                Some(("QT_INSTALL_DOCS", v)) => paths.install_docs = to_path_buf(v),
                Some(("QT_INSTALL_HEADERS", v)) => paths.install_headers = to_path_buf(v),
                Some(("QT_INSTALL_LIBS", v)) => paths.install_libs = to_path_buf(v),
                Some(("QT_INSTALL_LIBEXECS", v)) => paths.install_libexecs = to_path_buf(v),
                Some(("QT_INSTALL_BINS", v)) => paths.install_bins = to_path_buf(v),
                Some(("QT_INSTALL_TESTS", v)) => paths.install_tests = to_path_buf(v),
                Some(("QT_INSTALL_PLUGINS", v)) => paths.install_plugins = to_path_buf(v),
                Some(("QT_INSTALL_IMPORTS", v)) => paths.install_imports = to_path_buf(v),
                Some(("QT_INSTALL_QML", v)) => paths.install_qml = to_path_buf(v),
                Some(("QT_INSTALL_TRANSLATIONS", v)) => paths.install_translations = to_path_buf(v),
                Some(("QT_INSTALL_CONFIGURATION", v)) => {
                    paths.install_configuration = to_path_buf(v)
                }
                Some(("QT_INSTALL_EXAMPLES", v)) => paths.install_examples = to_path_buf(v),
                Some(("QT_INSTALL_DEMOS", v)) => paths.install_demos = to_path_buf(v),
                Some(("QT_HOST_PREFIX", v)) => paths.host_prefix = to_path_buf(v),
                Some(("QT_HOST_DATA", v)) => paths.host_data = to_path_buf(v),
                Some(("QT_HOST_BINS", v)) => paths.host_bins = to_path_buf(v),
                Some(("QT_HOST_LIBS", v)) => paths.host_libs = to_path_buf(v),
                _ => {}
            }
        }
        paths
    }
}

fn to_path_buf(s: &str) -> Option<PathBuf> {
    if s.is_empty() {
        None
    } else {
        Some(PathBuf::from(s.to_owned()))
    }
}
