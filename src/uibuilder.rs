use qmluic::qml;
use quick_xml::events::{BytesStart, BytesText, Event};
use std::io;

pub struct UiBuilder<'doc, W>
where
    W: io::Write,
{
    writer: quick_xml::Writer<W>,
    doc: &'doc qml::UiDocument,
    class_name: String, // TODO: maybe UiDocument should have the name?
    errors: Vec<qml::ParseError<'doc>>,
}

impl<'doc, W> UiBuilder<'doc, W>
where
    W: io::Write,
{
    pub fn new(dest: W, doc: &'doc qml::UiDocument, class_name: impl AsRef<str>) -> Self {
        let writer = quick_xml::Writer::new_with_indent(dest, b' ', 1);
        UiBuilder {
            writer,
            doc,
            class_name: class_name.as_ref().to_owned(),
            errors: Vec::new(),
        }
    }

    pub fn build(&mut self) -> quick_xml::Result<()> {
        let ui_tag = BytesStart::borrowed_name(b"ui")
            .with_attributes([(b"version".as_ref(), b"4.0".as_ref())]);
        self.writer
            .write_event(Event::Start(ui_tag.to_borrowed()))?;
        write_tagged_str(&mut self.writer, b"class", &self.class_name)?;
        // TODO
        self.writer.write_event(Event::End(ui_tag.to_end()))?;
        self.writer.write(b"\n")?;
        Ok(())
    }

    pub fn errors(&self) -> &[qml::ParseError<'doc>] {
        &self.errors
    }
}

fn write_tagged_str<W>(
    writer: &mut quick_xml::Writer<W>,
    tag: &[u8],
    content: &str,
) -> quick_xml::Result<()>
where
    W: io::Write,
{
    let tag = BytesStart::borrowed_name(tag);
    writer.write_event(Event::Start(tag.to_borrowed()))?;
    writer.write_event(Event::Text(BytesText::from_plain_str(content)))?;
    writer.write_event(Event::End(tag.to_end()))?;
    Ok(())
}
