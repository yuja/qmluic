use super::context::{BuildDocContext, ObjectContext};
use super::expr::Value;
use super::objcode::PropertyCode;
use super::object::{self, Widget};
use super::property::{self, PropertySetter};
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
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let cls = obj_node.class();
        let properties_code_map = ctx.code_map_for_object(obj_node).properties();
        let (attributes, children) = if cls.is_derived_from(&ctx.classes.vbox_layout) {
            process_vbox_layout_children(ctx, obj_node, diagnostics)
        } else if cls.is_derived_from(&ctx.classes.hbox_layout) {
            process_hbox_layout_children(ctx, obj_node, diagnostics)
        } else if cls.is_derived_from(&ctx.classes.form_layout) {
            process_form_layout_children(ctx, obj_node, diagnostics)
        } else if cls.is_derived_from(&ctx.classes.grid_layout) {
            let flow =
                LayoutFlow::parse(&ctx.make_object_context(), properties_code_map, diagnostics);
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
            &ctx.make_object_context(),
            obj_node.class(),
            obj_node.name(),
            attributes,
            properties_code_map,
            children,
            diagnostics,
        )
    }

    pub(super) fn new(
        ctx: &ObjectContext,
        class: &Class,
        name: impl Into<String>,
        attributes: LayoutAttributes,
        properties_code_map: &HashMap<&str, PropertyCode>,
        children: Vec<LayoutItem>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        let pseudo_property_names = if class.is_derived_from(&ctx.classes.grid_layout) {
            ["flow", "columns", "rows"].as_ref() // see LayoutFlow
        } else {
            [].as_ref()
        };
        let mut properties = property::make_serializable_map(
            ctx,
            properties_code_map,
            pseudo_property_names,
            diagnostics,
        );
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
        attached: &LayoutItemAttached,
        content: LayoutItemContent,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        LayoutItem {
            alignment: attached.alignment(diagnostics).map(|(_, v)| v),
            column,
            column_span: attached.column_span(diagnostics).map(|(_, v)| v),
            row,
            row_span: attached.row_span(diagnostics).map(|(_, v)| v),
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
#[derive(Debug)]
struct LayoutItemAttached<'a, 't, 's, 'm> {
    ctx: ObjectContext<'a, 't, 's>,
    map: Option<&'m HashMap<&'s str, PropertyCode<'a, 't, 's>>>,
}

macro_rules! impl_attached_enum_property {
    ($name:ident, $key:expr) => {
        fn $name(
            &self,
            diagnostics: &mut Diagnostics,
        ) -> Option<(&'m PropertyCode<'a, 't, 's>, String)> {
            self.map
                .and_then(|m| property::get_enum(&self.ctx, m, $key, diagnostics))
        }
    };
}

macro_rules! impl_attached_i32_property {
    ($name:ident, $key:expr) => {
        fn $name(
            &self,
            diagnostics: &mut Diagnostics,
        ) -> Option<(&'m PropertyCode<'a, 't, 's>, i32)> {
            self.map
                .and_then(|m| property::get_i32(&self.ctx, m, $key, diagnostics))
        }
    };
}

impl<'a, 't, 's, 'm> LayoutItemAttached<'a, 't, 's, 'm> {
    fn prepare(ctx: &'m BuildDocContext<'a, 't, 's>, obj_node: ObjectNode) -> Self {
        let code_map = ctx.code_map_for_object(obj_node);
        let map = code_map
            .attached_properties(&ctx.classes.layout)
            .map(|(_, m)| m);
        LayoutItemAttached {
            ctx: ctx.make_object_context(),
            map,
        }
    }

    impl_attached_enum_property!(alignment, "alignment");
    impl_attached_i32_property!(column, "column");
    impl_attached_i32_property!(column_minimum_width, "columnMinimumWidth");
    impl_attached_i32_property!(column_span, "columnSpan");
    impl_attached_i32_property!(column_stretch, "columnStretch");
    impl_attached_i32_property!(row, "row");
    impl_attached_i32_property!(row_minimum_height, "rowMinimumHeight");
    impl_attached_i32_property!(row_span, "rowSpan");
    impl_attached_i32_property!(row_stretch, "rowStretch");
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
        if cls.is_derived_from(&ctx.classes.layout) {
            LayoutItemContent::Layout(Layout::build(ctx, obj_node, diagnostics))
        } else if cls.is_derived_from(&ctx.classes.spacer_item) {
            object::confine_children(obj_node, diagnostics);
            LayoutItemContent::SpacerItem(SpacerItem::new(
                &ctx.make_object_context(),
                obj_node.name(),
                ctx.code_map_for_object(obj_node).properties(),
                diagnostics,
            ))
        } else if cls.is_derived_from(&ctx.classes.widget) {
            LayoutItemContent::Widget(Widget::build(ctx, obj_node, diagnostics))
        } else {
            diagnostics.push(Diagnostic::error(
                obj_node.obj().node().byte_range(),
                format!(
                    "class '{}' is not a QLayout, QSpacerItem, nor QWidget",
                    cls.qualified_cxx_name()
                ),
            ));
            // but process as widget to report as many errors as possible
            LayoutItemContent::Widget(Widget::build(ctx, obj_node, diagnostics))
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
    pub properties: HashMap<String, Value>,
}

impl SpacerItem {
    pub(super) fn new(
        ctx: &ObjectContext,
        name: impl Into<String>,
        properties_code_map: &HashMap<&str, PropertyCode>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        // no check for writable as all spacer properties are translated by uic
        let properties = property::make_value_map(ctx, properties_code_map, &[], diagnostics);

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

        for (k, v) in self.properties.iter().sorted_by_key(|&(k, _)| k) {
            let t = BytesStart::borrowed_name(b"property").with_attributes([("name", k.as_ref())]);
            writer.write_event(Event::Start(t.to_borrowed()))?;
            v.serialize_to_xml(writer)?;
            writer.write_event(Event::End(t.to_end()))?;
        }

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
            let attached = LayoutItemAttached::prepare(ctx, n);
            let content = LayoutItemContent::build(ctx, n, diagnostics);
            maybe_insert_into_opt_i32_array(
                &mut attributes./*row_*/stretch,
                row,
                attached.row_stretch(diagnostics),
                diagnostics,
            );
            LayoutItem::new(None, None, &attached, content, diagnostics)
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
            let attached = LayoutItemAttached::prepare(ctx, n);
            let content = LayoutItemContent::build(ctx, n, diagnostics);
            maybe_insert_into_opt_i32_array(
                &mut attributes./*column_*/stretch,
                column,
                attached.column_stretch(diagnostics),
                diagnostics,
            );
            LayoutItem::new(None, None, &attached, content, diagnostics)
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
            let attached = LayoutItemAttached::prepare(ctx, n);
            let content = LayoutItemContent::build(ctx, n, diagnostics);
            let (row, column) = index_counter.parse_next(&attached, diagnostics);
            LayoutItem::new(Some(row), Some(column), &attached, content, diagnostics)
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
            let attached = LayoutItemAttached::prepare(ctx, n);
            let content = LayoutItemContent::build(ctx, n, diagnostics);
            let (row, column) = index_counter.parse_next(&attached, diagnostics);
            maybe_insert_into_opt_i32_array(
                &mut attributes.column_minimum_width,
                column as usize,
                attached.column_minimum_width(diagnostics),
                diagnostics,
            );
            maybe_insert_into_opt_i32_array(
                &mut attributes.column_stretch,
                column as usize,
                attached.column_stretch(diagnostics),
                diagnostics,
            );
            maybe_insert_into_opt_i32_array(
                &mut attributes.row_minimum_height,
                column as usize,
                attached.row_minimum_height(diagnostics),
                diagnostics,
            );
            maybe_insert_into_opt_i32_array(
                &mut attributes.row_stretch,
                row as usize,
                attached.row_stretch(diagnostics),
                diagnostics,
            );
            LayoutItem::new(Some(row), Some(column), &attached, content, diagnostics)
        })
        .collect();
    (attributes, children)
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
            maybe_parse_layout_index("row", attached.row(diagnostics), max_row, diagnostics),
            maybe_parse_layout_index(
                "column",
                attached.column(diagnostics),
                max_column,
                diagnostics,
            ),
        )
    }
}

impl LayoutFlow {
    fn parse(
        ctx: &ObjectContext,
        properties_code_map: &HashMap<&str, PropertyCode>,
        diagnostics: &mut Diagnostics,
    ) -> Self {
        // should be kept sync with QGridLayout definition in metatype_tweak.rs
        let left_to_right = if let Some((p, s)) =
            property::get_enum(ctx, properties_code_map, "flow", diagnostics)
        {
            match s.as_ref() {
                "QGridLayout::LeftToRight" => true,
                "QGridLayout::TopToBottom" => false,
                s => {
                    diagnostics.push(Diagnostic::error(
                        p.node().byte_range(),
                        format!("unsupported layout flow expression: {s}"),
                    ));
                    true // don't care
                }
            }
        } else {
            true // LeftToRight by default
        };

        const MAX_COUNT: i32 = 65536; // arbitrary value to avoid excessive allocation if any
        let mut pop_count_property = |name| {
            property::get_i32(ctx, properties_code_map, name, diagnostics)
                .and_then(|(p, c)| {
                    if c <= 0 {
                        diagnostics.push(Diagnostic::error(
                            p.node().byte_range(),
                            format!("negative or zero {name} is not allowed"),
                        ));
                        None
                    } else if c > MAX_COUNT {
                        diagnostics.push(Diagnostic::error(
                            p.node().byte_range(),
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
    index: Option<(&PropertyCode, i32)>,
    max_index: i32,
    diagnostics: &mut Diagnostics,
) -> Option<i32> {
    index.and_then(|(p, v)| {
        if v < 0 {
            diagnostics.push(Diagnostic::error(
                p.node().byte_range(),
                format!("negative {field_name} is not allowed"),
            ));
            None
        } else if v > max_index {
            diagnostics.push(Diagnostic::error(
                p.node().byte_range(),
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
    value: Option<(&PropertyCode, i32)>,
    diagnostics: &mut Diagnostics,
) {
    if let Some((p1, v1)) = value {
        if index >= array.len() {
            array.resize_with(index + 1, Default::default);
        }
        match array[index] {
            Some(v0) if v0 != v1 => {
                diagnostics.push(Diagnostic::error(
                    p1.node().byte_range(),
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
