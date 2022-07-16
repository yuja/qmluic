use super::context::BuildDocContext;
use super::object::Widget;
use super::property;
use super::xmlutil;
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectNode;
use crate::qtname::FileNameRules;
use crate::typemap::{Class, TypeSpace};
use itertools::Itertools as _;
use quick_xml::events::{BytesStart, Event};
use std::io;

/// Top-level object wrapper to be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct UiForm {
    pub class: String,
    pub root_widget: Widget,
    pub custom_widgets: Vec<CustomWidget>,
}

impl UiForm {
    pub(super) fn build(
        ctx: &BuildDocContext,
        obj_node: ObjectNode,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let cls = obj_node.class();
        if !cls.is_derived_from(&ctx.classes.widget) {
            diagnostics.push(Diagnostic::error(
                obj_node.obj().node().byte_range(),
                format!("class '{}' is not a QWidget", cls.qualified_cxx_name()),
            ));
            // but continue anyway to report as many errors as possible
        }

        let properties_map = property::make_properties_from_code_map(
            &ctx.make_object_context(),
            ctx.code_map_for_object(obj_node).properties(),
            diagnostics,
        );
        let root_widget = Widget::build(ctx, obj_node, properties_map, diagnostics);
        let custom_widgets = ctx
            .object_tree
            .flat_iter()
            .filter_map(|n| n.is_custom_type().then(|| n.class().clone()))
            .unique()
            .filter_map(|cls| CustomWidget::from_class(&cls, ctx.file_name_rules))
            .collect();
        UiForm {
            class: ctx.type_name.to_owned(),
            root_widget,
            custom_widgets,
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let tag = BytesStart::borrowed_name(b"ui").with_attributes([("version", "4.0")]);
        writer.write_event(Event::Start(tag.to_borrowed()))?;
        xmlutil::write_tagged_str(writer, "class", &self.class)?;
        self.root_widget.serialize_to_xml(writer)?;
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
        let super_cls = cls.public_super_classes().next().and_then(|r| r.ok())?;
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
