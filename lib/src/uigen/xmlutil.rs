//! Utility for UI XML generation.

use super::{XmlResult, XmlWriter};
use quick_xml::events::{BytesStart, BytesText, Event};
use std::io;

pub(super) fn write_tagged_str<W, S, T>(
    writer: &mut XmlWriter<W>,
    tag: T,
    content: S,
) -> XmlResult<()>
where
    W: io::Write,
    S: AsRef<str>,
    T: AsRef<[u8]>,
{
    let tag = BytesStart::borrowed_name(tag.as_ref());
    writer.write_event(Event::Start(tag.to_borrowed()))?;
    writer.write_event(Event::Text(BytesText::from_plain_str(content.as_ref())))?;
    writer.write_event(Event::End(tag.to_end()))?;
    Ok(())
}
