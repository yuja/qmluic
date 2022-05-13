//! Qt user interface XML (.ui) generator.

mod expr;
mod gadget;
mod object;
mod xmlutil;

pub use self::expr::*; // re-export
pub use self::gadget::*; // re-export
pub use self::object::*; // re-export

pub type XmlError = quick_xml::Error;
pub type XmlResult<T> = quick_xml::Result<T>;
pub type XmlWriter<W> = quick_xml::Writer<W>;
