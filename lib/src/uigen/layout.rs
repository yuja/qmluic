use super::expr::{self, ConstantExpression, ConstantValue};
use super::gadget::Margins;
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

        let mut properties = object::collect_properties(
            cls,
            &binding_map,
            &["contentsMargins"],
            ctx.source,
            diagnostics,
        );
        if let Some(m) = binding_map
            .get("contentsMargins")
            .and_then(|v| Margins::from_binding_value(ctx, cls, v, diagnostics))
        {
            let to_v = |d| ConstantExpression::Value(ConstantValue::Number(d as f64));
            properties.insert("leftMargin".to_owned(), to_v(m.left));
            properties.insert("topMargin".to_owned(), to_v(m.top));
            properties.insert("rightMargin".to_owned(), to_v(m.right));
            properties.insert("bottomMargin".to_owned(), to_v(m.bottom));
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
    fn new(
        row: Option<i32>,
        column: Option<i32>,
        mut attached: LayoutItemAttached,
        content: LayoutItemContent,
    ) -> Self {
        LayoutItem {
            alignment: attached.alignment.take().map(|(_, v)| v),
            column,
            column_span: attached.column_span.take().map(|(_, v)| v),
            row,
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
    alignment: Option<(Node<'t>, String)>,
    column: Option<(Node<'t>, i32)>,
    column_minimum_width: Option<(Node<'t>, i32)>,
    column_span: Option<(Node<'t>, i32)>,
    column_stretch: Option<(Node<'t>, i32)>,
    row: Option<(Node<'t>, i32)>,
    row_minimum_height: Option<(Node<'t>, i32)>,
    row_span: Option<(Node<'t>, i32)>,
    row_stretch: Option<(Node<'t>, i32)>,
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
        let binding_map = attached_type_map.get(["QLayout"].as_ref())?; // TODO: resolve against imported types
        let mut properties = LayoutItemAttached::default();
        for (&name, value) in binding_map {
            // should be kept sync with QLayoutAttached definition in metatype_tweak.rs
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
                "columnMinimumWidth" => {
                    properties.column_minimum_width =
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
                "rowMinimumHeight" => {
                    properties.row_minimum_height =
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
            properties: object::collect_properties(cls, &binding_map, &[], ctx.source, diagnostics),
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
            if let Some((n, _)) = attached.column_minimum_width {
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
            if let Some((n, _)) = attached.row_minimum_height {
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
            const UNSUPPORTED_MSG: &str = "unsupported hbox layout property";
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics)?;
            if let Some((n, _)) = attached.column {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.column_minimum_width {
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
            if let Some((n, _)) = attached.row_minimum_height {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row_span {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row_stretch {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
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
            const UNSUPPORTED_MSG: &str = "unsupported form layout property";
            let (attached, content) = make_layout_item_pair(ctx, n, diagnostics)?;
            let (row, column) = index_counter.parse_next(&attached, diagnostics)?;
            if let Some((n, _)) = attached.column_minimum_width {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.column_span {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.column_stretch {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row_minimum_height {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row_span {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            if let Some((n, _)) = attached.row_stretch {
                diagnostics.push(Diagnostic::error(n.byte_range(), UNSUPPORTED_MSG));
            }
            Some(LayoutItem::new(Some(row), Some(column), attached, content))
        })
        .collect();
    (attributes, children)
}

fn process_grid_layout_children(
    ctx: &BuildContext,
    layout_obj: &UiObjectDefinition,
    diagnostics: &mut Diagnostics,
) -> (LayoutAttributes, Vec<LayoutItem>) {
    let mut attributes = LayoutAttributes::default();
    let mut index_counter = LayoutIndexCounter::new(LayoutFlow::LeftToRight { columns: 65536 }); // TODO
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
    let attached = LayoutItemAttached::from_object_definition(ctx, &cls, &obj, diagnostics)
        .unwrap_or_default();
    let content = LayoutItemContent::from_object_definition(ctx, &cls, &obj, diagnostics)?;
    Some((attached, content))
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

fn maybe_parse_layout_index(
    field_name: &str,
    index: Option<(Node, i32)>,
    max_index: i32,
    diagnostics: &mut Diagnostics,
) -> Option<Option<i32>> {
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
        Some((_, i)) => Some(Some(i)),
        None => Some(None),
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
