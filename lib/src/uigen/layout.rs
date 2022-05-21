use super::expr::{self, ConstantExpression};
use super::object::{self, Widget};
use super::BuildContext;
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::qmlast::{Node, UiObjectDefinition};
use crate::typemap::{Class, TypeSpace};
use itertools::Itertools as _;
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::io;

/// Layout definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Layout {
    pub class: String,
    pub name: Option<String>,
    attributes: LayoutAttributes,
    pub properties: HashMap<String, ConstantExpression>,
    pub children: Vec<LayoutItem>,
}

#[derive(Clone, Debug, Default)]
pub struct LayoutAttributes {
    column_stretch: Vec<Option<i32>>,
    row_stretch: Vec<Option<i32>>,
    stretch: Vec<Option<i32>>, // for vbox/hbox
}

impl Layout {
    /// Generates layout of `cls` type and its children recursively from the given `obj`
    /// definition.
    pub(super) fn from_object_definition(
        ctx: &BuildContext,
        cls: &Class,
        obj: &UiObjectDefinition,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let binding_map = diagnostics.consume_err(obj.build_binding_map(ctx.source))?;
        let (attributes, children) = if cls.is_derived_from(&ctx.vbox_layout_class) {
            process_vbox_layout_children(ctx, obj, diagnostics)
        } else if cls.is_derived_from(&ctx.hbox_layout_class) {
            process_hbox_layout_children(ctx, obj, diagnostics)
        } else if cls.is_derived_from(&ctx.form_layout_class) {
            process_form_layout_children(ctx, obj, diagnostics)
        } else if cls.is_derived_from(&ctx.grid_layout_class) {
            process_grid_layout_children(ctx, obj, diagnostics)
        } else {
            diagnostics.push(Diagnostic::error(
                obj.node().byte_range(),
                format!("unknown layout class: {}", cls.qualified_name()),
            ));
            // use the most restricted one to report as many errors as possible
            process_vbox_layout_children(ctx, obj, diagnostics)
        };
        Some(Layout {
            class: cls.name().to_owned(),
            name: obj.object_id().map(|n| n.to_str(ctx.source).to_owned()),
            attributes,
            properties: object::collect_properties(cls, &binding_map, ctx.source, diagnostics),
            children,
        })
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"layout");
        tag.push_attribute(("class", self.class.as_ref()));
        if let Some(n) = &self.name {
            tag.push_attribute(("name", n.as_ref()));
        }
        if !self.attributes.column_stretch.is_empty() {
            tag.push_attribute((
                "columnstretch",
                format_opt_i32_array(&self.attributes.column_stretch, 1).as_ref(),
            ));
        }
        if !self.attributes.row_stretch.is_empty() {
            tag.push_attribute((
                "rowstretch",
                format_opt_i32_array(&self.attributes.row_stretch, 1).as_ref(),
            ));
        }
        if !self.attributes.stretch.is_empty() {
            tag.push_attribute((
                "stretch",
                format_opt_i32_array(&self.attributes.stretch, 1).as_ref(),
            ));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        object::serialize_properties_to_xml(writer, &self.properties)?;

        for c in &self.children {
            c.serialize_to_xml(writer)?;
        }

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Layout item definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct LayoutItem {
    pub alignment: Option<String>,
    pub column: Option<i32>,
    pub column_span: Option<i32>,
    pub row: Option<i32>,
    pub row_span: Option<i32>,
    pub content: LayoutItemContent,
}

impl LayoutItem {
    fn new(mut attached: LayoutItemAttached, content: LayoutItemContent) -> Self {
        LayoutItem {
            alignment: attached.alignment.take().map(|(_, v)| v),
            column: attached.column.take().map(|(_, v)| v),
            column_span: attached.column_span.take().map(|(_, v)| v),
            row: attached.row.take().map(|(_, v)| v),
            row_span: attached.row_span.take().map(|(_, v)| v),
            content,
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"item");
        if let Some(v) = &self.alignment {
            tag.push_attribute(("alignment", v.as_ref()));
        }
        if let Some(v) = self.column {
            tag.push_attribute(("column", v.to_string().as_ref()));
        }
        if let Some(v) = self.column_span {
            tag.push_attribute(("columnspan", v.to_string().as_ref()));
        }
        if let Some(v) = self.row {
            tag.push_attribute(("row", v.to_string().as_ref()));
        }
        if let Some(v) = self.row_span {
            tag.push_attribute(("rowspan", v.to_string().as_ref()));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        self.content.serialize_to_xml(writer)?;

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

/// Layout properties to be resolved into [`Layout`] and [`LayoutItem`].
#[derive(Clone, Debug, Default)]
struct LayoutItemAttached<'t> {
    // binding node is stored for error reporting
    pub alignment: Option<(Node<'t>, String)>,
    pub column: Option<(Node<'t>, i32)>,
    pub column_span: Option<(Node<'t>, i32)>,
    pub column_stretch: Option<(Node<'t>, i32)>,
    pub row: Option<(Node<'t>, i32)>,
    pub row_span: Option<(Node<'t>, i32)>,
    pub row_stretch: Option<(Node<'t>, i32)>,
}

impl<'t> LayoutItemAttached<'t> {
    fn from_object_definition<'a, P>(
        ctx: &BuildContext,
        parent_space: &P, // TODO: should be QML space, not C++ metatype space
        obj: &UiObjectDefinition<'t>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self>
    where
        P: TypeSpace<'a>,
    {
        let attached_type_map = diagnostics.consume_err(obj.build_attached_type_map(ctx.source))?;
        let binding_map = attached_type_map.get(["QLayoutItem"].as_ref())?; // TODO: resolve against imported types
        let mut properties = LayoutItemAttached::default();
        for (&name, value) in binding_map {
            match name {
                "alignment" => {
                    // type name is resolved within C++ metatype space, which is correct
                    properties.alignment = expr::resolve_type_scoped_for_node(
                        parent_space,
                        "Qt::Alignment",
                        value.node(),
                        diagnostics,
                    )
                    .and_then(|ty| {
                        expr::format_enum_expression(
                            parent_space,
                            &ty,
                            value,
                            ctx.source,
                            diagnostics,
                        )
                    })
                    .map(|v| (value.binding_node(), v));
                }
                "column" => {
                    properties.column =
                        expr::evaluate_i32(parent_space, value, ctx.source, diagnostics)
                            .map(|v| (value.binding_node(), v));
                }
                "columnSpan" => {
                    properties.column_span =
                        expr::evaluate_i32(parent_space, value, ctx.source, diagnostics)
                            .map(|v| (value.binding_node(), v));
                }
                "columnStretch" => {
                    properties.column_stretch =
                        expr::evaluate_i32(parent_space, value, ctx.source, diagnostics)
                            .map(|v| (value.binding_node(), v));
                }
                "row" => {
                    properties.row =
                        expr::evaluate_i32(parent_space, value, ctx.source, diagnostics)
                            .map(|v| (value.binding_node(), v));
                }
                "rowSpan" => {
                    properties.row_span =
                        expr::evaluate_i32(parent_space, value, ctx.source, diagnostics)
                            .map(|v| (value.binding_node(), v));
                }
                "rowStretch" => {
                    properties.row_stretch =
                        expr::evaluate_i32(parent_space, value, ctx.source, diagnostics)
                            .map(|v| (value.binding_node(), v));
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        value.binding_node().byte_range(),
                        "unknown layout property",
                    ));
                }
            }
        }
        Some(properties)
    }
}

/// Variant for the object managed by the layout item.
#[derive(Clone, Debug)]
pub enum LayoutItemContent {
    Layout(Layout),
    SpacerItem(SpacerItem),
    Widget(Widget),
}

impl LayoutItemContent {
    /// Generates layout content and its children recursively from the given `node`.
    fn from_object_definition(
        ctx: &BuildContext,
        cls: &Class,
        obj: &UiObjectDefinition,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        if cls.is_derived_from(&ctx.layout_class) {
            Layout::from_object_definition(ctx, cls, obj, diagnostics)
                .map(LayoutItemContent::Layout)
        } else if cls.is_derived_from(&ctx.spacer_item_class) {
            SpacerItem::from_object_definition(ctx, cls, obj, diagnostics)
                .map(LayoutItemContent::SpacerItem)
        } else if cls.is_derived_from(&ctx.widget_class) {
            Widget::from_object_definition(ctx, cls, obj, diagnostics)
                .map(LayoutItemContent::Widget)
        } else {
            diagnostics.push(Diagnostic::error(
                obj.node().byte_range(),
                format!(
                    "class '{}' is not a QLayout, QSpacerItem, nor QWidget",
                    cls.qualified_name()
                ),
            ));
            None
        }
    }

    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        use LayoutItemContent::*;
        match self {
            Layout(x) => x.serialize_to_xml(writer),
            SpacerItem(x) => x.serialize_to_xml(writer),
            Widget(x) => x.serialize_to_xml(writer),
        }
    }
}

/// Spacer item definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct SpacerItem {
    pub name: Option<String>,
    pub properties: HashMap<String, ConstantExpression>,
}

impl SpacerItem {
    /// Generates spacer item of `cls` type from the given `obj` definition.
    ///
    /// The given `cls` is supposed to be of `QSpacerItem` type.
    fn from_object_definition(
        ctx: &BuildContext,
        cls: &Class,
        obj: &UiObjectDefinition,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let binding_map = diagnostics.consume_err(obj.build_binding_map(ctx.source))?;
        object::confine_children(cls, obj, diagnostics);
        Some(SpacerItem {
            name: obj.object_id().map(|n| n.to_str(ctx.source).to_owned()),
            properties: object::collect_properties(cls, &binding_map, ctx.source, diagnostics),
        })
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"spacer");
        if let Some(n) = &self.name {
            tag.push_attribute(("name", n.as_ref()));
        }
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        object::serialize_properties_to_xml(writer, &self.properties)?;

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

fn process_vbox_layout_children(
    ctx: &BuildContext,
    layout_obj: &UiObjectDefinition,
    diagnostics: &mut Diagnostics,
) -> (LayoutAttributes, Vec<LayoutItem>) {
    let mut attributes = LayoutAttributes::default();
    let children = layout_obj
        .child_object_nodes()
        .iter()
        .enumerate()
        .filter_map(|(row, &n)| {
            const UNSUPPORTED_MSG: &str = "unsupported vbox layout property";
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics)?;
            if let Some((n, _)) = attached.column {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.column_span {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.column_stretch {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row_span {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            maybe_insert_into_opt_i32_array(
                &mut attributes./*row_*/stretch,
                row,
                attached.row_stretch,
                diagnostics,
            );
            Some(LayoutItem::new(attached, content))
        })
        .collect();
    (attributes, children)
}

fn process_hbox_layout_children(
    ctx: &BuildContext,
    layout_obj: &UiObjectDefinition,
    diagnostics: &mut Diagnostics,
) -> (LayoutAttributes, Vec<LayoutItem>) {
    let mut attributes = LayoutAttributes::default();
    let children = layout_obj
        .child_object_nodes()
        .iter()
        .enumerate()
        .filter_map(|(column, &n)| {
            const UNSUPPORTED_MSG: &str = "unsupported hbox layout property";
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics)?;
            if let Some((n, _)) = attached.column {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.column_span {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            maybe_insert_into_opt_i32_array(
                &mut attributes./*column_*/stretch,
                column,
                attached.column_stretch,
                diagnostics,
            );
            if let Some((n, _)) = attached.row {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row_span {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row_stretch {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            Some(LayoutItem::new(attached, content))
        })
        .collect();
    (attributes, children)
}

fn process_form_layout_children(
    ctx: &BuildContext,
    layout_obj: &UiObjectDefinition,
    diagnostics: &mut Diagnostics,
) -> (LayoutAttributes, Vec<LayoutItem>) {
    let attributes = LayoutAttributes::default();
    let children = layout_obj
        .child_object_nodes()
        .iter()
        .filter_map(|&n| {
            const UNSUPPORTED_MSG: &str = "unsupported form layout property";
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics)?;
            expect_layout_index("column", attached.column, 1, n, diagnostics)?;
            expect_layout_index("row", attached.row, i32::MAX, n, diagnostics)?;
            if let Some((n, _)) = attached.column_span {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.column_stretch {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row_span {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row_stretch {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            Some(LayoutItem::new(attached, content))
        })
        .collect();
    (attributes, children)
}

fn process_grid_layout_children(
    ctx: &BuildContext,
    layout_obj: &UiObjectDefinition,
    diagnostics: &mut Diagnostics,
) -> (LayoutAttributes, Vec<LayoutItem>) {
    const MAX_INDEX: i32 = 65535; // arbitrary value to avoid excessive allocation
    let mut attributes = LayoutAttributes::default();
    let children = layout_obj
        .child_object_nodes()
        .iter()
        .filter_map(|&n| {
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics)?;
            let column = expect_layout_index("column", attached.column, MAX_INDEX, n, diagnostics)?;
            let row = expect_layout_index("row", attached.row, MAX_INDEX, n, diagnostics)?;
            maybe_insert_into_opt_i32_array(
                &mut attributes.column_stretch,
                column,
                attached.column_stretch,
                diagnostics,
            );
            maybe_insert_into_opt_i32_array(
                &mut attributes.row_stretch,
                row,
                attached.row_stretch,
                diagnostics,
            );
            Some(LayoutItem::new(attached, content))
        })
        .collect();
    (attributes, children)
}

fn make_layout_item_pair<'t>(
    ctx: &BuildContext,
    node: Node<'t>,
    diagnostics: &mut Diagnostics,
) -> Option<(LayoutItemAttached<'t>, LayoutItemContent)> {
    let (obj, cls) = object::resolve_object_definition(ctx, node, diagnostics)?;
    let attached = LayoutItemAttached::from_object_definition(ctx, &cls, &obj, diagnostics)
        .unwrap_or_default();
    let content = LayoutItemContent::from_object_definition(ctx, &cls, &obj, diagnostics)?;
    Some((attached, content))
}

fn expect_layout_index(
    field_name: &str,
    index: Option<(Node, i32)>,
    max_index: i32,
    content_node: Node,
    diagnostics: &mut Diagnostics,
) -> Option<usize> {
    match index {
        Some((n, i)) if i < 0 => {
            diagnostics.push(Diagnostic::error(
                n.byte_range(),
                format!("negative {field_name} is not allowed"),
            ));
            None
        }
        Some((n, i)) if i > max_index => {
            diagnostics.push(Diagnostic::error(
                n.byte_range(),
                format!("{field_name} is too large"),
            ));
            None
        }
        Some((_, i)) => Some(i as usize),
        None => {
            diagnostics.push(Diagnostic::error(
                content_node.byte_range(),
                format!("{field_name} is not set"),
            ));
            None
        }
    }
}

fn maybe_insert_into_opt_i32_array(
    array: &mut Vec<Option<i32>>,
    index: usize,
    value: Option<(Node, i32)>,
    diagnostics: &mut Diagnostics,
) {
    if let Some((n, v1)) = value {
        if index >= array.len() {
            array.resize_with(index + 1, Default::default);
        }
        match array[index] {
            Some(v0) if v0 != v1 => {
                diagnostics.push(Diagnostic::error(
                    n.byte_range(),
                    format!("mismatched with the value previously set: {v0}"),
                ));
            }
            _ => {
                array[index] = Some(v1);
            }
        }
    }
}

fn format_opt_i32_array(array: &[Option<i32>], default: i32) -> String {
    array.iter().map(|x| x.unwrap_or(default)).join(",")
}
