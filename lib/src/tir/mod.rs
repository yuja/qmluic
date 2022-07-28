//! Type-checked intermediate representation of expressions.

mod builder;
mod ceval;
mod core;
mod dump;
pub mod interpret;
mod testenv;

pub use self::builder::build; // re-export
pub use self::core::*; // re-export
pub use self::dump::dump_code_body; // re-export
pub use self::interpret::evaluate_code; // re-export
