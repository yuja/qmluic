use super::expr::ConstantExpression;
use super::object::{self, Widget};
use super::property::{self, WithNode};
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
    column_minimum_width: Vec<Option<i32>>,
    column_stretch: Vec<Option<i32>>,
    row_minimum_height: Vec<Option<i32>>,
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
        let mut properties_map =
            property::collect_properties_with_node(cls, &binding_map, &[], ctx.source, diagnostics);

        let (attributes, children) = if cls.is_derived_from(&ctx.vbox_layout_class) {
            process_vbox_layout_children(ctx, obj, diagnostics)
        } else if cls.is_derived_from(&ctx.hbox_layout_class) {
            process_hbox_layout_children(ctx, obj, diagnostics)
        } else if cls.is_derived_from(&ctx.form_layout_class) {
            process_form_layout_children(ctx, obj, diagnostics)
        } else if cls.is_derived_from(&ctx.grid_layout_class) {
            let flow = LayoutFlow::parse(&mut properties_map, diagnostics);
            process_grid_layout_children(ctx, obj, flow, diagnostics)
        } else {
            diagnostics.push(Diagnostic::error(
                obj.node().byte_range(),
                format!("unknown layout class: {}", cls.qualified_name()),
            ));
            // use the most restricted one to report as many errors as possible
            process_vbox_layout_children(ctx, obj, diagnostics)
        };

        let mut properties: HashMap<_, _> = properties_map
            .into_iter()
            .map(|(k, v)| (k, v.into_value()))
            .collect();
        // TODO: if metatypes were broken, contentsMargins could be of different type
        if let Some(ConstantExpression::Gadget(m)) = properties.remove("contentsMargins") {
            properties.extend(
                m.properties
                    .into_iter()
                    .map(|(k, v)| (k + "Margin", ConstantExpression::Value(v))),
            );
        }

        Some(Layout {
            class: cls.name().to_owned(),
            name: obj.object_id().map(|n| n.to_str(ctx.source).to_owned()),
            attributes,
            properties,
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
        if !self.attributes.column_minimum_width.is_empty() {
            tag.push_attribute((
                "columnminimumwidth",
                format_opt_i32_array(&self.attributes.column_minimum_width, 0).as_ref(),
            ));
        }
        if !self.attributes.column_stretch.is_empty() {
            tag.push_attribute((
                "columnstretch",
                format_opt_i32_array(&self.attributes.column_stretch, 1).as_ref(),
            ));
        }
        if !self.attributes.row_minimum_height.is_empty() {
            tag.push_attribute((
                "rowminimumheight",
                format_opt_i32_array(&self.attributes.row_minimum_height, 0).as_ref(),
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

        property::serialize_properties_to_xml(writer, &self.properties)?;

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
    fn new(
        row: Option<i32>,
        column: Option<i32>,
        mut attached: LayoutItemAttached,
        content: LayoutItemContent,
    ) -> Self {
        LayoutItem {
            alignment: attached.alignment.take().map(|v| v.into_value()),
            column,
            column_span: attached.column_span.take().map(|v| v.into_value()),
            row,
            row_span: attached.row_span.take().map(|v| v.into_value()),
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
            tag.push_attribute(("colspan", v.to_string().as_ref())); // not "columnspan"
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
    alignment: Option<WithNode<'t, String>>,
    column: Option<WithNode<'t, i32>>,
    column_minimum_width: Option<WithNode<'t, i32>>,
    column_span: Option<WithNode<'t, i32>>,
    column_stretch: Option<WithNode<'t, i32>>,
    row: Option<WithNode<'t, i32>>,
    row_minimum_height: Option<WithNode<'t, i32>>,
    row_span: Option<WithNode<'t, i32>>,
    row_stretch: Option<WithNode<'t, i32>>,
}

impl<'t> LayoutItemAttached<'t> {
    fn from_object_definition(
        ctx: &BuildContext,
        obj: &UiObjectDefinition<'t>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let attached_type_map = diagnostics.consume_err(obj.build_attached_type_map(ctx.source))?;
        let binding_map = attached_type_map.get(["QLayout"].as_ref())?; // TODO: resolve against imported types
        let properties_map = property::collect_properties_with_node(
            &ctx.layout_attached_class,
            binding_map,
            &[],
            ctx.source,
            diagnostics,
        );
        let get_enum_property = |name, diagnostics: &mut Diagnostics| {
            properties_map
                .get(name)
                .and_then(|v| diagnostics.consume_err(v.to_enum_with_node()))
                .map(|v| v.map_value(|s| s.to_owned()))
        };
        let get_i32_property = |name, diagnostics: &mut Diagnostics| {
            properties_map
                .get(name)
                .and_then(|v| diagnostics.consume_err(v.to_i32_with_node()))
        };

        Some(LayoutItemAttached {
            // should be kept sync with QLayoutAttached definition in metatype_tweak.rs
            alignment: get_enum_property("alignment", diagnostics),
            column: get_i32_property("column", diagnostics),
            column_minimum_width: get_i32_property("columnMinimumWidth", diagnostics),
            column_span: get_i32_property("columnSpan", diagnostics),
            column_stretch: get_i32_property("columnStretch", diagnostics),
            row: get_i32_property("row", diagnostics),
            row_minimum_height: get_i32_property("rowMinimumHeight", diagnostics),
            row_span: get_i32_property("rowSpan", diagnostics),
            row_stretch: get_i32_property("rowStretch", diagnostics),
        })
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
            properties: property::collect_properties(
                cls,
                &binding_map,
                &[],
                ctx.source,
                diagnostics,
            ),
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

        property::serialize_properties_to_xml(writer, &self.properties)?;

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
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics)?;
            check_unsupported_property(&attached.column, diagnostics);
            check_unsupported_property(&attached.column_minimum_width, diagnostics);
            check_unsupported_property(&attached.column_span, diagnostics);
            check_unsupported_property(&attached.column_stretch, diagnostics);
            check_unsupported_property(&attached.row, diagnostics);
            check_unsupported_property(&attached.row_minimum_height, diagnostics);
            check_unsupported_property(&attached.row_span, diagnostics);
            maybe_insert_into_opt_i32_array(
                &mut attributes./*row_*/stretch,
                row,
                attached.row_stretch,
                diagnostics,
            );
            Some(LayoutItem::new(None, None, attached, content))
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
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics)?;
            check_unsupported_property(&attached.column, diagnostics);
            check_unsupported_property(&attached.column_minimum_width, diagnostics);
            check_unsupported_property(&attached.column_span, diagnostics);
            maybe_insert_into_opt_i32_array(
                &mut attributes./*column_*/stretch,
                column,
                attached.column_stretch,
                diagnostics,
            );
            check_unsupported_property(&attached.row, diagnostics);
            check_unsupported_property(&attached.row_minimum_height, diagnostics);
            check_unsupported_property(&attached.row_span, diagnostics);
            check_unsupported_property(&attached.row_stretch, diagnostics);
            Some(LayoutItem::new(None, None, attached, content))
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
    let mut index_counter = LayoutIndexCounter::new(LayoutFlow::LeftToRight { columns: 2 });
    let children = layout_obj
        .child_object_nodes()
        .iter()
        .filter_map(|&n| {
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics)?;
            let (row, column) = index_counter.parse_next(&attached, diagnostics)?;
            check_unsupported_property(&attached.column_minimum_width, diagnostics);
            check_unsupported_property(&attached.column_span, diagnostics);
            check_unsupported_property(&attached.column_stretch, diagnostics);
            check_unsupported_property(&attached.row_minimum_height, diagnostics);
            check_unsupported_property(&attached.row_span, diagnostics);
            check_unsupported_property(&attached.row_stretch, diagnostics);
            Some(LayoutItem::new(Some(row), Some(column), attached, content))
        })
        .collect();
    (attributes, children)
}

fn process_grid_layout_children(
    ctx: &BuildContext,
    layout_obj: &UiObjectDefinition,
    flow: LayoutFlow,
    diagnostics: &mut Diagnostics,
) -> (LayoutAttributes, Vec<LayoutItem>) {
    let mut attributes = LayoutAttributes::default();
    let mut index_counter = LayoutIndexCounter::new(flow);
    let children = layout_obj
        .child_object_nodes()
        .iter()
        .filter_map(|&n| {
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics)?;
            let (row, column) = index_counter.parse_next(&attached, diagnostics)?;
            maybe_insert_into_opt_i32_array(
                &mut attributes.column_minimum_width,
                column as usize,
                attached.column_minimum_width,
                diagnostics,
            );
            maybe_insert_into_opt_i32_array(
                &mut attributes.column_stretch,
                column as usize,
                attached.column_stretch,
                diagnostics,
            );
            maybe_insert_into_opt_i32_array(
                &mut attributes.row_minimum_height,
                column as usize,
                attached.row_minimum_height,
                diagnostics,
            );
            maybe_insert_into_opt_i32_array(
                &mut attributes.row_stretch,
                row as usize,
                attached.row_stretch,
                diagnostics,
            );
            Some(LayoutItem::new(Some(row), Some(column), attached, content))
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
    let attached =
        LayoutItemAttached::from_object_definition(ctx, &obj, diagnostics).unwrap_or_default();
    let content = LayoutItemContent::from_object_definition(ctx, &cls, &obj, diagnostics)?;
    Some((attached, content))
}

fn check_unsupported_property<V>(value: &Option<WithNode<V>>, diagnostics: &mut Diagnostics) {
    if let Some(v) = value {
        diagnostics.push(Diagnostic::error(
            v.binding_node().byte_range(),
            "unsupported layout property",
        ));
    }
}

/// Generates contiguous `(row, column)` of layout items.
#[derive(Clone, Debug)]
struct LayoutIndexCounter {
    flow: LayoutFlow,
    next_row: i32,
    next_column: i32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum LayoutFlow {
    LeftToRight { columns: i32 },
    TopToBottom { rows: i32 },
}

impl LayoutIndexCounter {
    fn new(flow: LayoutFlow) -> Self {
        LayoutIndexCounter {
            flow,
            next_row: 0,
            next_column: 0,
        }
    }

    fn next(&mut self, row: Option<i32>, column: Option<i32>) -> (i32, i32) {
        if let (Some(r), Some(c)) = (row, column) {
            self.next_row = r;
            self.next_column = c;
        } else if let Some(r) = row {
            self.next_row = r;
            self.next_column = match self.flow {
                LayoutFlow::LeftToRight { .. } => 0,
                LayoutFlow::TopToBottom { .. } => self.next_column,
            };
        } else if let Some(c) = column {
            self.next_column = c;
            self.next_row = match self.flow {
                LayoutFlow::LeftToRight { .. } => self.next_row,
                LayoutFlow::TopToBottom { .. } => 0,
            };
        }

        let cur = (self.next_row, self.next_column);
        match self.flow {
            LayoutFlow::LeftToRight { columns } => {
                self.next_column = (self.next_column + 1) % columns;
                self.next_row += (self.next_column == 0) as i32;
            }
            LayoutFlow::TopToBottom { rows } => {
                self.next_row = (self.next_row + 1) % rows;
                self.next_column += (self.next_row == 0) as i32;
            }
        }
        cur
    }

    fn parse_next(
        &mut self,
        attached: &LayoutItemAttached,
        diagnostics: &mut Diagnostics,
    ) -> Option<(i32, i32)> {
        const MAX_INDEX: i32 = 65535; // arbitrary value to avoid excessive allocation
        let (max_row, max_column) = match self.flow {
            LayoutFlow::LeftToRight { columns } => (MAX_INDEX, columns - 1),
            LayoutFlow::TopToBottom { rows } => (rows - 1, MAX_INDEX),
        };
        Some(self.next(
            maybe_parse_layout_index("row", attached.row, max_row, diagnostics)?,
            maybe_parse_layout_index("column", attached.column, max_column, diagnostics)?,
        ))
    }
}

impl LayoutFlow {
    fn parse(
        properties_map: &mut HashMap<String, WithNode<ConstantExpression>>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        // should be kept sync with QGridLayout definition in metatype_tweak.rs
        let left_to_right = if let Some(v) = properties_map.remove("flow") {
            match diagnostics.consume_err(v.to_enum()) {
                Some("QGridLayout::LeftToRight") => true,
                Some("QGridLayout::TopToBottom") => false,
                Some(s) => {
                    diagnostics.push(Diagnostic::error(
                        v.node().byte_range(),
                        format!("unsupported layout flow expression: {s}"),
                    ));
                    true // don't care
                }
                None => true, // don't care
            }
        } else {
            true // LeftToRight by default
        };

        const MAX_COUNT: i32 = 65536; // arbitrary value to avoid excessive allocation if any
        let mut pop_count_property = |name| {
            properties_map
                .remove(name)
                .and_then(|v| {
                    let c = diagnostics.consume_err(v.to_i32())?;
                    if c <= 0 {
                        diagnostics.push(Diagnostic::error(
                            v.node().byte_range(),
                            format!("negative or zero {name} is not allowed"),
                        ));
                        None
                    } else if c > MAX_COUNT {
                        diagnostics.push(Diagnostic::error(
                            v.node().byte_range(),
                            format!("{name} is too large"),
                        ));
                        None
                    } else {
                        Some(c)
                    }
                })
                .unwrap_or(MAX_COUNT)
        };

        // TODO: maybe warn unused columns/rows?
        let columns = pop_count_property("columns");
        let rows = pop_count_property("rows");
        if left_to_right {
            LayoutFlow::LeftToRight { columns }
        } else {
            LayoutFlow::TopToBottom { rows }
        }
    }
}

fn maybe_parse_layout_index(
    field_name: &str,
    index: Option<WithNode<i32>>,
    max_index: i32,
    diagnostics: &mut Diagnostics,
) -> Option<Option<i32>> {
    match index {
        Some(i) if *i.value() < 0 => {
            diagnostics.push(Diagnostic::error(
                i.node().byte_range(),
                format!("negative {field_name} is not allowed"),
            ));
            None
        }
        Some(i) if *i.value() > max_index => {
            diagnostics.push(Diagnostic::error(
                i.node().byte_range(),
                format!("{field_name} is too large"),
            ));
            None
        }
        Some(i) => Some(Some(*i.value())),
        None => Some(None),
    }
}

fn maybe_insert_into_opt_i32_array(
    array: &mut Vec<Option<i32>>,
    index: usize,
    value: Option<WithNode<i32>>,
    diagnostics: &mut Diagnostics,
) {
    if let Some(v1) = value {
        if index >= array.len() {
            array.resize_with(index + 1, Default::default);
        }
        match array[index] {
            Some(v0) if v0 != *v1.value() => {
                diagnostics.push(Diagnostic::error(
                    v1.node().byte_range(),
                    format!("mismatched with the value previously set: {v0}"),
                ));
            }
            _ => {
                array[index] = Some(*v1.value());
            }
        }
    }
}

fn format_opt_i32_array(array: &[Option<i32>], default: i32) -> String {
    array.iter().map(|x| x.unwrap_or(default)).join(",")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn index_counter_left_to_right() {
        let mut counter = LayoutIndexCounter::new(LayoutFlow::LeftToRight { columns: 2 });
        assert_eq!(counter.next(None, None), (0, 0));
        assert_eq!(counter.next(None, None), (0, 1));
        assert_eq!(counter.next(None, None), (1, 0));
        assert_eq!(counter.next(Some(2), None), (2, 0));
        assert_eq!(counter.next(None, None), (2, 1));
        assert_eq!(counter.next(None, Some(1)), (3, 1));
        assert_eq!(counter.next(Some(4), Some(1)), (4, 1));
    }

    #[test]
    fn index_counter_top_to_bottom() {
        let mut counter = LayoutIndexCounter::new(LayoutFlow::TopToBottom { rows: 3 });
        assert_eq!(counter.next(None, None), (0, 0));
        assert_eq!(counter.next(None, None), (1, 0));
        assert_eq!(counter.next(None, None), (2, 0));
        assert_eq!(counter.next(None, None), (0, 1));
        assert_eq!(counter.next(None, Some(3)), (0, 3));
        assert_eq!(counter.next(Some(2), None), (2, 3));
        assert_eq!(counter.next(Some(3), Some(4)), (3, 4));
    }
}
