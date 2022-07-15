use super::context::BuildDocContext;
use super::expr::Value;
use super::object::{self, Widget};
use super::property::{self, PropertiesMap, PropertySetter, WithNode};
use super::{XmlResult, XmlWriter};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::objtree::ObjectNode;
use crate::typemap::{Class, TypeSpace};
use itertools::Itertools as _;
use quick_xml::events::{BytesStart, Event};
use std::collections::HashMap;
use std::io;

/// Layout definition which can be serialized to UI XML.
#[derive(Clone, Debug)]
pub struct Layout {
    pub class: String,
    pub name: String,
    attributes: LayoutAttributes,
    pub properties: HashMap<String, (Value, PropertySetter)>,
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
    /// Creates a serializable tree by visiting the children recursively.
    pub(super) fn build(
        ctx: &BuildDocContext,
        obj_node: ObjectNode,
        mut properties_map: PropertiesMap,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let cls = obj_node.class();
        let (attributes, children) = if cls.is_derived_from(&ctx.classes.vbox_layout) {
            process_vbox_layout_children(ctx, obj_node, diagnostics)
        } else if cls.is_derived_from(&ctx.classes.hbox_layout) {
            process_hbox_layout_children(ctx, obj_node, diagnostics)
        } else if cls.is_derived_from(&ctx.classes.form_layout) {
            process_form_layout_children(ctx, obj_node, diagnostics)
        } else if cls.is_derived_from(&ctx.classes.grid_layout) {
            let flow = LayoutFlow::parse(&mut properties_map, diagnostics);
            process_grid_layout_children(ctx, obj_node, flow, diagnostics)
        } else {
            diagnostics.push(Diagnostic::error(
                obj_node.obj().node().byte_range(),
                format!("unknown layout class: {}", cls.qualified_cxx_name()),
            ));
            // use the most restricted one to report as many errors as possible
            process_vbox_layout_children(ctx, obj_node, diagnostics)
        };

        Self::new(
            obj_node.class(),
            obj_node.name(),
            attributes,
            properties_map,
            children,
            diagnostics,
        )
    }

    pub(super) fn new(
        class: &Class,
        name: impl Into<String>,
        attributes: LayoutAttributes,
        mut properties_map: PropertiesMap,
        children: Vec<LayoutItem>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        property::reject_unwritable_properties(&mut properties_map, diagnostics);
        let mut properties = property::make_serializable_properties(properties_map, diagnostics);
        // TODO: if metatypes were broken, contentsMargins could be of different type
        if let Some((Value::Gadget(m), s)) = properties.remove("contentsMargins") {
            // don't care the property setter since uic will anyway retranslate them
            properties.extend(
                m.properties
                    .into_iter()
                    .map(|(k, v)| (k + "Margin", (v, s))),
            );
        }

        Layout {
            class: class.qualified_cxx_name().into_owned(),
            name: name.into(),
            attributes,
            properties,
            children,
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let mut tag = BytesStart::borrowed_name(b"layout");
        tag.push_attribute(("class", self.class.as_ref()));
        tag.push_attribute(("name", self.name.as_ref()));
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

        property::serialize_properties_to_xml(writer, "property", &self.properties)?;

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
            alignment: attached.alignment.take().map(|v| v.into_data()),
            column,
            column_span: attached.column_span.take().map(|v| v.into_data()),
            row,
            row_span: attached.row_span.take().map(|v| v.into_data()),
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
    fn from_object_node(
        ctx: &BuildDocContext<'_, 't, '_>,
        obj_node: ObjectNode<'_, 't, '_>,
        diagnostics: &mut Diagnostics,
    ) -> Option<Self> {
        let attached_type_map =
            diagnostics.consume_err(obj_node.obj().build_attached_type_map(ctx.source))?;
        let binding_map = attached_type_map.get(["QLayout"].as_ref())?; // TODO: resolve against imported types
        let properties_map = property::collect_properties_with_node(
            &ctx.make_object_context(),
            &ctx.classes.layout_attached,
            binding_map,
            diagnostics,
        );
        let get_enum_property = |name, diagnostics: &mut Diagnostics| {
            properties_map
                .get(name)
                .and_then(|v| diagnostics.consume_err(v.to_enum_with_node()))
                .map(|v| v.map_data(|s| s.to_owned()))
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
    /// Generates layout content and its children recursively from the given `obj_node`.
    fn build(ctx: &BuildDocContext, obj_node: ObjectNode, diagnostics: &mut Diagnostics) -> Self {
        let cls = obj_node.class();
        let properties_map = ctx.properties_for_object(obj_node).clone();
        if cls.is_derived_from(&ctx.classes.layout) {
            LayoutItemContent::Layout(Layout::build(ctx, obj_node, properties_map, diagnostics))
        } else if cls.is_derived_from(&ctx.classes.spacer_item) {
            object::confine_children(obj_node, diagnostics);
            LayoutItemContent::SpacerItem(SpacerItem::new(
                obj_node.name(),
                properties_map,
                diagnostics,
            ))
        } else if cls.is_derived_from(&ctx.classes.widget) {
            LayoutItemContent::Widget(Widget::build(ctx, obj_node, properties_map, diagnostics))
        } else {
            diagnostics.push(Diagnostic::error(
                obj_node.obj().node().byte_range(),
                format!(
                    "class '{}' is not a QLayout, QSpacerItem, nor QWidget",
                    cls.qualified_cxx_name()
                ),
            ));
            // but process as widget to report as many errors as possible
            LayoutItemContent::Widget(Widget::build(ctx, obj_node, properties_map, diagnostics))
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
    pub name: String,
    pub properties: HashMap<String, (Value, PropertySetter)>,
}

impl SpacerItem {
    pub(super) fn new(
        name: impl Into<String>,
        properties_map: PropertiesMap,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        // no check for writable as all spacer properties are translated by uic
        let properties = property::make_serializable_properties(properties_map, diagnostics);
        SpacerItem {
            name: name.into(),
            properties,
        }
    }

    /// Serializes this to UI XML.
    pub fn serialize_to_xml<W>(&self, writer: &mut XmlWriter<W>) -> XmlResult<()>
    where
        W: io::Write,
    {
        let tag =
            BytesStart::borrowed_name(b"spacer").with_attributes([("name", self.name.as_ref())]);
        writer.write_event(Event::Start(tag.to_borrowed()))?;

        property::serialize_properties_to_xml(writer, "property", &self.properties)?;

        writer.write_event(Event::End(tag.to_end()))?;
        Ok(())
    }
}

fn process_vbox_layout_children(
    ctx: &BuildDocContext,
    layout_obj_node: ObjectNode,
    diagnostics: &mut Diagnostics,
) -> (LayoutAttributes, Vec<LayoutItem>) {
    let mut attributes = LayoutAttributes::default();
    let children = layout_obj_node
        .children()
        .enumerate()
        .map(|(row, n)| {
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics);
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
            LayoutItem::new(None, None, attached, content)
        })
        .collect();
    (attributes, children)
}

fn process_hbox_layout_children(
    ctx: &BuildDocContext,
    layout_obj_node: ObjectNode,
    diagnostics: &mut Diagnostics,
) -> (LayoutAttributes, Vec<LayoutItem>) {
    let mut attributes = LayoutAttributes::default();
    let children = layout_obj_node
        .children()
        .enumerate()
        .map(|(column, n)| {
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics);
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
            LayoutItem::new(None, None, attached, content)
        })
        .collect();
    (attributes, children)
}

fn process_form_layout_children(
    ctx: &BuildDocContext,
    layout_obj_node: ObjectNode,
    diagnostics: &mut Diagnostics,
) -> (LayoutAttributes, Vec<LayoutItem>) {
    let attributes = LayoutAttributes::default();
    let mut index_counter = LayoutIndexCounter::new(LayoutFlow::LeftToRight { columns: 2 });
    let children = layout_obj_node
        .children()
        .map(|n| {
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics);
            let (row, column) = index_counter.parse_next(&attached, diagnostics);
            check_unsupported_property(&attached.column_minimum_width, diagnostics);
            check_unsupported_property(&attached.column_span, diagnostics);
            check_unsupported_property(&attached.column_stretch, diagnostics);
            check_unsupported_property(&attached.row_minimum_height, diagnostics);
            check_unsupported_property(&attached.row_span, diagnostics);
            check_unsupported_property(&attached.row_stretch, diagnostics);
            LayoutItem::new(Some(row), Some(column), attached, content)
        })
        .collect();
    (attributes, children)
}

fn process_grid_layout_children(
    ctx: &BuildDocContext,
    layout_obj_node: ObjectNode,
    flow: LayoutFlow,
    diagnostics: &mut Diagnostics,
) -> (LayoutAttributes, Vec<LayoutItem>) {
    let mut attributes = LayoutAttributes::default();
    let mut index_counter = LayoutIndexCounter::new(flow);
    let children = layout_obj_node
        .children()
        .map(|n| {
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics);
            let (row, column) = index_counter.parse_next(&attached, diagnostics);
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
            LayoutItem::new(Some(row), Some(column), attached, content)
        })
        .collect();
    (attributes, children)
}

fn make_layout_item_pair<'t>(
    ctx: &BuildDocContext<'_, 't, '_>,
    obj_node: ObjectNode<'_, 't, '_>,
    diagnostics: &mut Diagnostics,
) -> (LayoutItemAttached<'t>, LayoutItemContent) {
    let attached =
        LayoutItemAttached::from_object_node(ctx, obj_node, diagnostics).unwrap_or_default();
    let content = LayoutItemContent::build(ctx, obj_node, diagnostics);
    (attached, content)
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
    ) -> (i32, i32) {
        const MAX_INDEX: i32 = 65535; // arbitrary value to avoid excessive allocation
        let (max_row, max_column) = match self.flow {
            LayoutFlow::LeftToRight { columns } => (MAX_INDEX, columns - 1),
            LayoutFlow::TopToBottom { rows } => (rows - 1, MAX_INDEX),
        };
        self.next(
            maybe_parse_layout_index("row", attached.row, max_row, diagnostics),
            maybe_parse_layout_index("column", attached.column, max_column, diagnostics),
        )
    }
}

impl LayoutFlow {
    fn parse(properties_map: &mut PropertiesMap, diagnostics: &mut Diagnostics) -> Self {
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
) -> Option<i32> {
    index.and_then(|i| {
        let v = *i.data();
        if v < 0 {
            diagnostics.push(Diagnostic::error(
                i.node().byte_range(),
                format!("negative {field_name} is not allowed"),
            ));
            None
        } else if v > max_index {
            diagnostics.push(Diagnostic::error(
                i.node().byte_range(),
                format!("{field_name} is too large"),
            ));
            None
        } else {
            Some(v)
        }
    })
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
            Some(v0) if v0 != *v1.data() => {
                diagnostics.push(Diagnostic::error(
                    v1.node().byte_range(),
                    format!("mismatched with the value previously set: {v0}"),
                ));
            }
            _ => {
                array[index] = Some(*v1.data());
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
