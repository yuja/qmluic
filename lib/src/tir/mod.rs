//! Type-checked intermediate representation of expressions.

mod builder;
mod ceval;
mod core;
mod dump;
pub mod interpret;
mod propdep;
mod testenv;

pub use self::builder::{build, build_callback}; // re-export
pub use self::core::*; // re-export
pub use self::dump::dump_code_body; // re-export
pub use self::interpret::evaluate_code; // re-export
pub use self::propdep::analyze_code_property_dependency; // re-export
