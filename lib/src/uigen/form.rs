use super::object::UiObject;
use super::xmlutil;
use super::{FileNameRules, XmlResult, XmlWriter};
use crate::typemap::{Class, TypeSpace};
use quick_xml::events::{BytesStart, Event};
use std::io;

/// Top-level object wrapper to be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct UiForm {
    pub class: Option<String>,
    pub root_object: UiObject,
    pub custom_widgets: Vec<CustomWidget>,
}

impl UiForm {
    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let tag = BytesStart::borrowed_name(b"ui").with_attributes([("version", "4.0")]);
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        if let Some(name) = &self.class {
            xmlutil::write_tagged_str(writer, "class", name)?;
        }
        self.root_object.serialize_to_xml(writer)?;
        if !self.custom_widgets.is_empty() {
            let tag = BytesStart::borrowed_name(b"customwidgets");
            writer.write_event(Event::Start(tag.to_borrowed()))?;
            for w in &self.custom_widgets {
                w.serialize_to_xml(writer)?;
            }
            writer.write_event(Event::End(tag.to_end()))?;
        }
        writer.write_event(Event::End(tag.to_end()))?;
        writer.write(b"\n")
    }
}

/// User type referenced from the [`UiForm`].
#[derive(Clone, Debug)]
pub struct CustomWidget {
    pub class: String,
    pub extends: String,
    pub header: String,
}

impl CustomWidget {
    pub(super) fn from_class(cls: &Class, file_name_rules: &FileNameRules) -> Option<Self> {
        // If super class doesn't exist, diagnostic message would have been emitted while
        // building the object representation. So returns silently.
        let super_cls = cls.public_super_classes().next()?;
        Some(CustomWidget {
            class: cls.qualified_cxx_name().into(),
            extends: super_cls.qualified_cxx_name().into(),
            header: file_name_rules.type_name_to_cxx_header_name(cls.name()),
        })
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let tag = BytesStart::borrowed_name(b"customwidget");
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        xmlutil::write_tagged_str(writer, "class", &self.class)?;
        xmlutil::write_tagged_str(writer, "extends", &self.extends)?;
        xmlutil::write_tagged_str(writer, "header", &self.header)?;
        writer.write_event(Event::End(tag.to_end()))
    }
}
