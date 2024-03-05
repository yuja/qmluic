//! Qt color type and parser.

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::str::FromStr;
use thiserror::Error;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Color {
    Rgb8(ColorRgb8),
    Rgba8(ColorRgba8),
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ColorRgb8 {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ColorRgba8 {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
    pub alpha: u8,
}

impl Color {
    pub fn rgb8(red: u8, green: u8, blue: u8) -> Self {
        Color::Rgb8(ColorRgb8::new(red, green, blue))
    }

    pub fn rgba8(red: u8, green: u8, blue: u8, alpha: u8) -> Self {
        Color::Rgba8(ColorRgba8::new(red, green, blue, alpha))
    }
}

impl FromStr for Color {
    type Err = ParseColorError;

    fn from_str(src: &str) -> Result<Self, Self::Err> {
        if let Some(hex) = src.strip_prefix('#') {
            parse_hex_color(hex).ok_or(ParseColorError::InvalidHex)
        } else if src.eq_ignore_ascii_case("transparent") {
            Ok(Color::rgba8(0, 0, 0, 0))
        } else if let Some(&c) = SVG_NAMED_COLORS.get(src) {
            Ok(Color::Rgb8(c))
        } else if let Some(&c) = SVG_NAMED_COLORS.get(src.to_ascii_lowercase().as_str()) {
            Ok(Color::Rgb8(c))
        } else {
            Err(ParseColorError::UnknownName)
        }
    }
}

impl ColorRgb8 {
    pub fn new(red: u8, green: u8, blue: u8) -> Self {
        ColorRgb8 { red, green, blue }
    }
}

impl ColorRgba8 {
    pub fn new(red: u8, green: u8, blue: u8, alpha: u8) -> Self {
        ColorRgba8 {
            red,
            green,
            blue,
            alpha,
        }
    }
}

#[derive(Debug, Error)]
pub enum ParseColorError {
    #[error("invalid hex color")]
    InvalidHex,
    #[error("unknown named color")]
    UnknownName,
}

fn parse_hex_color(hex: &str) -> Option<Color> {
    if hex.contains(|c: char| !c.is_ascii_hexdigit()) {
        return None;
    }
    // unlike CSS, QML color is #argb, not #rgba
    let argb = u32::from_str_radix(hex, 16).ok()?;
    match hex.len() {
        3 => Some(Color::rgb8(
            ((argb >> 8) & 0xf) as u8 * 0x11,
            ((argb >> 4) & 0xf) as u8 * 0x11,
            (argb & 0xf) as u8 * 0x11,
        )),
        4 => Some(Color::rgba8(
            ((argb >> 8) & 0xf) as u8 * 0x11,
            ((argb >> 4) & 0xf) as u8 * 0x11,
            (argb & 0xf) as u8 * 0x11,
            ((argb >> 12) & 0xf) as u8 * 0x11,
        )),
        6 => Some(Color::rgb8(
            ((argb >> 16) & 0xff) as u8,
            ((argb >> 8) & 0xff) as u8,
            (argb & 0xff) as u8,
        )),
        8 => Some(Color::rgba8(
            ((argb >> 16) & 0xff) as u8,
            ((argb >> 8) & 0xff) as u8,
            (argb & 0xff) as u8,
            ((argb >> 24) & 0xff) as u8,
        )),
        _ => None,
    }
}

// https://www.w3.org/Graphics/SVG/1.1/types.html#ColorKeywords
static SVG_NAMED_COLORS: Lazy<HashMap<&'static str, ColorRgb8>> = Lazy::new(|| {
    HashMap::from([
        ("aliceblue", ColorRgb8::new(240, 248, 255)),
        ("antiquewhite", ColorRgb8::new(250, 235, 215)),
        ("aqua", ColorRgb8::new(0, 255, 255)),
        ("aquamarine", ColorRgb8::new(127, 255, 212)),
        ("azure", ColorRgb8::new(240, 255, 255)),
        ("beige", ColorRgb8::new(245, 245, 220)),
        ("bisque", ColorRgb8::new(255, 228, 196)),
        ("black", ColorRgb8::new(0, 0, 0)),
        ("blanchedalmond", ColorRgb8::new(255, 235, 205)),
        ("blue", ColorRgb8::new(0, 0, 255)),
        ("blueviolet", ColorRgb8::new(138, 43, 226)),
        ("brown", ColorRgb8::new(165, 42, 42)),
        ("burlywood", ColorRgb8::new(222, 184, 135)),
        ("cadetblue", ColorRgb8::new(95, 158, 160)),
        ("chartreuse", ColorRgb8::new(127, 255, 0)),
        ("chocolate", ColorRgb8::new(210, 105, 30)),
        ("coral", ColorRgb8::new(255, 127, 80)),
        ("cornflowerblue", ColorRgb8::new(100, 149, 237)),
        ("cornsilk", ColorRgb8::new(255, 248, 220)),
        ("crimson", ColorRgb8::new(220, 20, 60)),
        ("cyan", ColorRgb8::new(0, 255, 255)),
        ("darkblue", ColorRgb8::new(0, 0, 139)),
        ("darkcyan", ColorRgb8::new(0, 139, 139)),
        ("darkgoldenrod", ColorRgb8::new(184, 134, 11)),
        ("darkgray", ColorRgb8::new(169, 169, 169)),
        ("darkgreen", ColorRgb8::new(0, 100, 0)),
        ("darkgrey", ColorRgb8::new(169, 169, 169)),
        ("darkkhaki", ColorRgb8::new(189, 183, 107)),
        ("darkmagenta", ColorRgb8::new(139, 0, 139)),
        ("darkolivegreen", ColorRgb8::new(85, 107, 47)),
        ("darkorange", ColorRgb8::new(255, 140, 0)),
        ("darkorchid", ColorRgb8::new(153, 50, 204)),
        ("darkred", ColorRgb8::new(139, 0, 0)),
        ("darksalmon", ColorRgb8::new(233, 150, 122)),
        ("darkseagreen", ColorRgb8::new(143, 188, 143)),
        ("darkslateblue", ColorRgb8::new(72, 61, 139)),
        ("darkslategray", ColorRgb8::new(47, 79, 79)),
        ("darkslategrey", ColorRgb8::new(47, 79, 79)),
        ("darkturquoise", ColorRgb8::new(0, 206, 209)),
        ("darkviolet", ColorRgb8::new(148, 0, 211)),
        ("deeppink", ColorRgb8::new(255, 20, 147)),
        ("deepskyblue", ColorRgb8::new(0, 191, 255)),
        ("dimgray", ColorRgb8::new(105, 105, 105)),
        ("dimgrey", ColorRgb8::new(105, 105, 105)),
        ("dodgerblue", ColorRgb8::new(30, 144, 255)),
        ("firebrick", ColorRgb8::new(178, 34, 34)),
        ("floralwhite", ColorRgb8::new(255, 250, 240)),
        ("forestgreen", ColorRgb8::new(34, 139, 34)),
        ("fuchsia", ColorRgb8::new(255, 0, 255)),
        ("gainsboro", ColorRgb8::new(220, 220, 220)),
        ("ghostwhite", ColorRgb8::new(248, 248, 255)),
        ("gold", ColorRgb8::new(255, 215, 0)),
        ("goldenrod", ColorRgb8::new(218, 165, 32)),
        ("gray", ColorRgb8::new(128, 128, 128)),
        ("green", ColorRgb8::new(0, 128, 0)),
        ("greenyellow", ColorRgb8::new(173, 255, 47)),
        ("grey", ColorRgb8::new(128, 128, 128)),
        ("honeydew", ColorRgb8::new(240, 255, 240)),
        ("hotpink", ColorRgb8::new(255, 105, 180)),
        ("indianred", ColorRgb8::new(205, 92, 92)),
        ("indigo", ColorRgb8::new(75, 0, 130)),
        ("ivory", ColorRgb8::new(255, 255, 240)),
        ("khaki", ColorRgb8::new(240, 230, 140)),
        ("lavender", ColorRgb8::new(230, 230, 250)),
        ("lavenderblush", ColorRgb8::new(255, 240, 245)),
        ("lawngreen", ColorRgb8::new(124, 252, 0)),
        ("lemonchiffon", ColorRgb8::new(255, 250, 205)),
        ("lightblue", ColorRgb8::new(173, 216, 230)),
        ("lightcoral", ColorRgb8::new(240, 128, 128)),
        ("lightcyan", ColorRgb8::new(224, 255, 255)),
        ("lightgoldenrodyellow", ColorRgb8::new(250, 250, 210)),
        ("lightgray", ColorRgb8::new(211, 211, 211)),
        ("lightgreen", ColorRgb8::new(144, 238, 144)),
        ("lightgrey", ColorRgb8::new(211, 211, 211)),
        ("lightpink", ColorRgb8::new(255, 182, 193)),
        ("lightsalmon", ColorRgb8::new(255, 160, 122)),
        ("lightseagreen", ColorRgb8::new(32, 178, 170)),
        ("lightskyblue", ColorRgb8::new(135, 206, 250)),
        ("lightslategray", ColorRgb8::new(119, 136, 153)),
        ("lightslategrey", ColorRgb8::new(119, 136, 153)),
        ("lightsteelblue", ColorRgb8::new(176, 196, 222)),
        ("lightyellow", ColorRgb8::new(255, 255, 224)),
        ("lime", ColorRgb8::new(0, 255, 0)),
        ("limegreen", ColorRgb8::new(50, 205, 50)),
        ("linen", ColorRgb8::new(250, 240, 230)),
        ("magenta", ColorRgb8::new(255, 0, 255)),
        ("maroon", ColorRgb8::new(128, 0, 0)),
        ("mediumaquamarine", ColorRgb8::new(102, 205, 170)),
        ("mediumblue", ColorRgb8::new(0, 0, 205)),
        ("mediumorchid", ColorRgb8::new(186, 85, 211)),
        ("mediumpurple", ColorRgb8::new(147, 112, 219)),
        ("mediumseagreen", ColorRgb8::new(60, 179, 113)),
        ("mediumslateblue", ColorRgb8::new(123, 104, 238)),
        ("mediumspringgreen", ColorRgb8::new(0, 250, 154)),
        ("mediumturquoise", ColorRgb8::new(72, 209, 204)),
        ("mediumvioletred", ColorRgb8::new(199, 21, 133)),
        ("midnightblue", ColorRgb8::new(25, 25, 112)),
        ("mintcream", ColorRgb8::new(245, 255, 250)),
        ("mistyrose", ColorRgb8::new(255, 228, 225)),
        ("moccasin", ColorRgb8::new(255, 228, 181)),
        ("navajowhite", ColorRgb8::new(255, 222, 173)),
        ("navy", ColorRgb8::new(0, 0, 128)),
        ("oldlace", ColorRgb8::new(253, 245, 230)),
        ("olive", ColorRgb8::new(128, 128, 0)),
        ("olivedrab", ColorRgb8::new(107, 142, 35)),
        ("orange", ColorRgb8::new(255, 165, 0)),
        ("orangered", ColorRgb8::new(255, 69, 0)),
        ("orchid", ColorRgb8::new(218, 112, 214)),
        ("palegoldenrod", ColorRgb8::new(238, 232, 170)),
        ("palegreen", ColorRgb8::new(152, 251, 152)),
        ("paleturquoise", ColorRgb8::new(175, 238, 238)),
        ("palevioletred", ColorRgb8::new(219, 112, 147)),
        ("papayawhip", ColorRgb8::new(255, 239, 213)),
        ("peachpuff", ColorRgb8::new(255, 218, 185)),
        ("peru", ColorRgb8::new(205, 133, 63)),
        ("pink", ColorRgb8::new(255, 192, 203)),
        ("plum", ColorRgb8::new(221, 160, 221)),
        ("powderblue", ColorRgb8::new(176, 224, 230)),
        ("purple", ColorRgb8::new(128, 0, 128)),
        ("red", ColorRgb8::new(255, 0, 0)),
        ("rosybrown", ColorRgb8::new(188, 143, 143)),
        ("royalblue", ColorRgb8::new(65, 105, 225)),
        ("saddlebrown", ColorRgb8::new(139, 69, 19)),
        ("salmon", ColorRgb8::new(250, 128, 114)),
        ("sandybrown", ColorRgb8::new(244, 164, 96)),
        ("seagreen", ColorRgb8::new(46, 139, 87)),
        ("seashell", ColorRgb8::new(255, 245, 238)),
        ("sienna", ColorRgb8::new(160, 82, 45)),
        ("silver", ColorRgb8::new(192, 192, 192)),
        ("skyblue", ColorRgb8::new(135, 206, 235)),
        ("slateblue", ColorRgb8::new(106, 90, 205)),
        ("slategray", ColorRgb8::new(112, 128, 144)),
        ("slategrey", ColorRgb8::new(112, 128, 144)),
        ("snow", ColorRgb8::new(255, 250, 250)),
        ("springgreen", ColorRgb8::new(0, 255, 127)),
        ("steelblue", ColorRgb8::new(70, 130, 180)),
        ("tan", ColorRgb8::new(210, 180, 140)),
        ("teal", ColorRgb8::new(0, 128, 128)),
        ("thistle", ColorRgb8::new(216, 191, 216)),
        ("tomato", ColorRgb8::new(255, 99, 71)),
        ("turquoise", ColorRgb8::new(64, 224, 208)),
        ("violet", ColorRgb8::new(238, 130, 238)),
        ("wheat", ColorRgb8::new(245, 222, 179)),
        ("white", ColorRgb8::new(255, 255, 255)),
        ("whitesmoke", ColorRgb8::new(245, 245, 245)),
        ("yellow", ColorRgb8::new(255, 255, 0)),
        ("yellowgreen", ColorRgb8::new(154, 205, 50)),
    ])
});

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_hex() {
        assert_eq!(
            "#48c".parse::<Color>().unwrap(),
            Color::rgb8(0x44, 0x88, 0xcc)
        );
        assert_eq!(
            "#F48c".parse::<Color>().unwrap(),
            Color::rgba8(0x44, 0x88, 0xcc, 0xff)
        );
        assert_eq!(
            "#012abc".parse::<Color>().unwrap(),
            Color::rgb8(0x01, 0x2a, 0xbc)
        );
        assert_eq!(
            "#DeadBeef".parse::<Color>().unwrap(),
            Color::rgba8(0xad, 0xbe, 0xef, 0xde)
        );
        assert_eq!(
            "#00000000".parse::<Color>().unwrap(),
            Color::rgba8(0x00, 0x00, 0x00, 0x00)
        );
        assert_eq!(
            "#ffffffff".parse::<Color>().unwrap(),
            Color::rgba8(0xff, 0xff, 0xff, 0xff)
        );
        assert!("#+48c".parse::<Color>().is_err());
        assert!("#4_8_c".parse::<Color>().is_err());
        assert!("#".parse::<Color>().is_err());
        assert!("#000000001".parse::<Color>().is_err());
    }

    #[test]
    fn parse_named() {
        assert_eq!(
            "red".parse::<Color>().unwrap(),
            Color::rgb8(0xff, 0x00, 0x00)
        );
        assert_eq!(
            "Blue".parse::<Color>().unwrap(),
            Color::rgb8(0x00, 0x00, 0xff)
        );
        assert_eq!(
            "transparent".parse::<Color>().unwrap(),
            Color::rgba8(0, 0, 0, 0)
        );
        assert_eq!(
            "Transparent".parse::<Color>().unwrap(),
            Color::rgba8(0, 0, 0, 0)
        );
    }
}
