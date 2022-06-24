#![forbid(unsafe_code)]

mod qtpaths;
pub mod reporting;
mod uiviewer;

pub use qtpaths::{QtPaths, QtVersion};
pub use uiviewer::UiViewer;
