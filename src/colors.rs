use core::marker::PhantomData;
use embedded_graphics_core::pixelcolor::{BinaryColor, Gray2, Gray4, Gray8, Rgb888};
use embedded_graphics_core::prelude::PixelColor;
use crate::ParsedPng;
use crate::types::PixelType;

// TODO options for alpha channel
// Mix with (known background) // replace transparent with known background (and mix properly with alpha)
// IgnoreAlpha                 // return the color without transparency or alpha
// DontDraw                    // do not draw transparent pixels (sample alpha channel at 127)
// Enable                      // return the color containing alpha channel
/*
enum AlphaHandling {
    UseBackground(C),
    Ignore,
    Enable,
}*/

pub trait AlphaHandler<C> : Clone{ }
pub trait ReturnC {}

#[derive(Clone)]
pub struct IgnoreAlpha;
impl<C> AlphaHandler<C> for IgnoreAlpha {}
impl ReturnC for IgnoreAlpha {}

/// Not available yet in embedded-graphics
#[derive(Clone)]
pub struct AlphaColor;
impl<C> AlphaHandler<C> for AlphaColor {}
impl ReturnC for AlphaColor {}

#[derive(Clone)]
pub struct WithBackground<C>(C);
impl<C: PixelColor> AlphaHandler<C> for WithBackground<C> {}
impl<C> ReturnC for WithBackground<C> {}

#[derive(Clone)]
pub struct DontDraw;
impl<C> AlphaHandler<C> for DontDraw {}

pub struct PixelsIterator<'a, Color, Handler> {
    pixel_type: PixelType<'a>,
    palette: Option<&'a [u8]>,
    scanline: &'a [u8],
    pos: usize,
    max_pos: usize, // for bit packed pixels
    handler: Handler,
    _phantom: PhantomData<Color>,
}

#[inline]
// TranspareNT: get alpha value for transparent colors
fn tnt(reference: u8, value: u8) -> u8 {
    if reference == value {
        0
    } else {
        255
    }
}

#[inline]
// TransparentPalette: get alpha value for transparent palette
fn tpal(palette: &[u8], value: u8) -> u8 {
    match palette.get(value as usize) {
        None => 255,
        Some(v) => *v,
    }
}

#[inline]
// TranspareNTRgb: get alpha value for transparent colors
fn tntr(reference: &[u8], value: &[u8]) -> u8 {
    if reference == value {
        0
    } else {
        255
    }
}

impl <'a, Color, Handler: AlphaHandler<Color>> PixelsIterator<'a, Color, Handler> {
    pub fn new(png: &ParsedPng<'a, Handler>, scanline: &'a [u8]) -> Self {
        PixelsIterator {
            pixel_type: png.pixel_type,
            palette: png.palette,
            scanline,
            pos: 0,
            max_pos: scanline.len(),
            handler: png.alpha_handler.clone(),
            _phantom: PhantomData,
        }
    }

    #[inline]
    fn bits<const N: usize>(&mut self) -> u8 {
        let s = 8 / N;
        let byte = self.scanline[self.pos / s];
        let rshift = self.pos % s;
        let bits = (byte >> ((s-1-rshift)*N)) & (0xFF >> (s-1)*N);
        self.pos += 1;
        bits
    }

    #[inline]
    fn bytes<const N: usize>(&mut self) -> [u8; N] {
        let mut res = [0_u8; N];
        for i in 0..N {
            res[i] = self.scanline[self.pos+i];
        }
        self.pos += N;
        res
    }

    #[inline]
    fn words<const N: usize>(&mut self) -> [u8; N] {
        // not supported by embedded-graphics, juste take most significant byte
        // rough approximation of a color rounding
        let mut res = [0_u8; N];
        for i in 0..N {
            res[i] = self.scanline[self.pos+2*i]; // big endian
        }
        self.pos += 2*N;
        res
    }

    fn next_opaque(&mut self) -> Color
    where BinaryColor: Into<Color>,
          Gray2: Into<Color>,
          Gray4: Into<Color>,
          Gray8: Into<Color>,
          Rgb888: Into<Color>,
    {
        match self.pixel_type {
            PixelType::Grayscale1 => {
                let bit = self.bits::<1>();
                if bit != 0 {
                    BinaryColor::On
                } else {
                    BinaryColor::Off
                }.into()
            },
            PixelType::Grayscale2 => {
                let bits = self.bits::<2>();
                Gray2::new(bits >> 6).into()
            },
            PixelType::Grayscale4 => {
                let bits = self.bits::<2>();
                Gray4::new(bits >> 4).into()
            },
            PixelType::Grayscale8 => {
                let byte = self.bytes::<1>();
                Gray8::new(byte[0]).into()
            },
            PixelType::Grayscale16 => {
                let byte = self.words::<1>();
                Gray8::new(byte[0]).into()
            },
            PixelType::Palette1(palette) => {
                let bit = self.bits::<1>();
                get_rgb_from_palette(palette, bit >> 7).into()
            },
            PixelType::Palette2(palette) => {
                let bits = self.bits::<2>();
                get_rgb_from_palette(palette, bits >> 6).into()
            },
            PixelType::Palette4(palette) => {
                let bits = self.bits::<4>();
                get_rgb_from_palette(palette, bits >> 4).into()
            },
            PixelType::Palette8(palette) => {
                let byte = self.bytes::<1>();
                get_rgb_from_palette(palette, byte[0]).into()
            },
            PixelType::Rgb8 => {
                let bytes = self.bytes::<3>();
                Rgb888::new(bytes[0], bytes[1], bytes[2]).into()
            },
            PixelType::Rgb16 => {
                let bytes = self.words::<3>();
                Rgb888::new(bytes[0], bytes[1], bytes[2]).into()
            },
            _ => unreachable!()
        }
    }

    // TODO, is this better than next_keep_alpha.1
    fn next_skip_alpha(&mut self) -> Color
    where BinaryColor: Into<Color>,
          Gray2: Into<Color>,
          Gray4: Into<Color>,
          Gray8: Into<Color>,
          Rgb888: Into<Color>,
    {
        match self.pixel_type {
            PixelType::Grayscale1Transparent(_) => {
                let bit = self.bits::<1>();
                if bit != 0 {
                    BinaryColor::On
                } else {
                    BinaryColor::Off
                }.into()
            },
            PixelType::Grayscale2Transparent(_) => {
                let bits = self.bits::<2>();
                Gray2::new(bits >> 6).into()
            },
            PixelType::Grayscale4Transparent(_) => {
                let bits = self.bits::<2>();
                Gray4::new(bits >> 4).into()
            },
            PixelType::Grayscale8Transparent(_) => {
                let byte = self.bytes::<1>();
                Gray8::new(byte[0]).into()
            },
            PixelType::Grayscale16Transparent(_) => {
                let byte = self.words::<1>();
                Gray8::new(byte[0]).into()
            },
            PixelType::Palette1Transparent(palette, _) => {
                let bit = self.bits::<1>();
                get_rgb_from_palette(palette, bit >> 7).into()
            },
            PixelType::Palette2Transparent(palette, _) => {
                let bits = self.bits::<2>();
                get_rgb_from_palette(palette, bits >> 6).into()
            },
            PixelType::Palette4Transparent(palette, _) => {
                let bits = self.bits::<4>();
                get_rgb_from_palette(palette, bits >> 4).into()
            },
            PixelType::Palette8Transparent(palette, _) => {
                let byte = self.bytes::<1>();
                get_rgb_from_palette(palette, byte[0]).into()
            },
            PixelType::Rgb8Transparent(_) => {
                let bytes = self.bytes::<3>();
                Rgb888::new(bytes[0], bytes[1], bytes[2]).into()
            },
            PixelType::Rgb16Transparent(_) => {
                let bytes = self.words::<3>();
                Rgb888::new(bytes[0], bytes[1], bytes[2]).into()
            },
            PixelType::GrayscaleAlpha8 => {
                let bytes = self.bytes::<2>();
                Gray8::new(bytes[0]).into()
            }
            PixelType::GrayscaleAlpha16 => {
                let byte = self.words::<2>();
                Gray8::new(byte[0]).into()
            }
            PixelType::RgbAlpha8 => {
                let bytes = self.bytes::<4>();
                Rgb888::new(bytes[0], bytes[1], bytes[2]).into()
            },
            PixelType::RgbAlpha16 => {
                let bytes = self.words::<4>();
                Rgb888::new(bytes[0], bytes[1], bytes[2]).into()
            },
            _ => self.next_opaque()
        }
    }

    fn next_keep_alpha(&mut self) -> (u8, Color)
    where BinaryColor: Into<Color>,
          Gray2: Into<Color>,
          Gray4: Into<Color>,
          Gray8: Into<Color>,
          Rgb888: Into<Color>,
    {
        match self.pixel_type {
            PixelType::Grayscale1Transparent(t) => {
                let bit = self.bits::<1>();
                (tnt(t,bit),
                     if bit != 0 {
                        BinaryColor::On
                    } else {
                        BinaryColor::Off
                    }.into()
                )
            },
            PixelType::Grayscale2Transparent(t) => {
                let bits = self.bits::<2>();
                (tnt(t,bits),Gray2::new(bits >> 6).into())
            },
            PixelType::Grayscale4Transparent(t) => {
                let bits = self.bits::<2>();
                (tnt(t,bits),Gray4::new(bits >> 4).into())
            },
            PixelType::Grayscale8Transparent(t) => {
                let byte = self.bytes::<1>();
                (tnt(t,byte[0]),Gray8::new(byte[0]).into())
            },
            PixelType::Grayscale16Transparent(t) => {
                let byte = self.words::<1>();
                (tnt((t>>8) as u8,byte[0]),Gray8::new(byte[0]).into())
            },
            PixelType::Palette1Transparent(palette, p) => {
                let bit = self.bits::<1>();
                (tpal(p,bit),get_rgb_from_palette(palette, bit >> 7).into())
            },
            PixelType::Palette2Transparent(palette, p) => {
                let bits = self.bits::<2>();
                (tpal(p,bits),get_rgb_from_palette(palette, bits >> 6).into())
            },
            PixelType::Palette4Transparent(palette, p) => {
                let bits = self.bits::<4>();
                (tpal(p,bits),get_rgb_from_palette(palette, bits >> 4).into())
            },
            PixelType::Palette8Transparent(palette, p) => {
                let byte = self.bytes::<1>();
                (tpal(p,byte[0]),get_rgb_from_palette(palette, byte[0]).into())
            },
            PixelType::Rgb8Transparent(t) => {
                let bytes = self.bytes::<3>();
                (tntr(&t, &bytes),Rgb888::new(bytes[0], bytes[1], bytes[2]).into())
            },
            PixelType::Rgb16Transparent(t) => {
                let bytes = self.words::<3>();
                (tntr(&t, &bytes),Rgb888::new(bytes[0], bytes[1], bytes[2]).into())
            },
            PixelType::GrayscaleAlpha8 => {
                let bytes = self.bytes::<2>();
                (bytes[1], Gray8::new(bytes[0]).into())
            }
            PixelType::GrayscaleAlpha16 => {
                let bytes = self.words::<2>();
                (bytes[1], Gray8::new(bytes[0]).into())
            }
            PixelType::RgbAlpha8 => {
                let bytes = self.bytes::<4>();
                (bytes[3], Rgb888::new(bytes[0], bytes[1], bytes[2]).into())
            },
            PixelType::RgbAlpha16 => {
                let bytes = self.words::<4>();
                (bytes[3], Rgb888::new(bytes[0], bytes[1], bytes[2]).into())
            },
            _ => (0xFF, self.next_opaque())
        }
    }

    fn next_alpha_color(&mut self) -> Color
    where BinaryColor: Into<Color>,
          Gray2: Into<Color>,
          Gray4: Into<Color>,
          Gray8: Into<Color>,
          Rgb888: Into<Color>,
          //Argb8888: Into<Color>
    {
        match self.pixel_type {
            PixelType::Grayscale1Transparent(t) => {
                let bit = self.bits::<1>();
                todo!()
            },
            PixelType::Grayscale2Transparent(t) => {
                let bits = self.bits::<2>();
                todo!()
            },
            PixelType::Grayscale4Transparent(t) => {
                let bits = self.bits::<2>();
                todo!()
            },
            PixelType::Grayscale8Transparent(t) => {
                let byte = self.bytes::<1>();
                todo!()
            },
            PixelType::Grayscale16Transparent(t) => {
                let byte = self.words::<1>();
                todo!()
            },
            PixelType::Palette1Transparent(palette, p) => {
                let bit = self.bits::<1>();
                todo!()
            },
            PixelType::Palette2Transparent(palette, p) => {
                let bits = self.bits::<2>();
                todo!()
            },
            PixelType::Palette4Transparent(palette, p) => {
                let bits = self.bits::<4>();
                todo!()
            },
            PixelType::Palette8Transparent(palette, p) => {
                let byte = self.bytes::<1>();
                todo!()
            },
            PixelType::Rgb8Transparent(t) => {
                let bytes = self.bytes::<3>();
                //Argb8888::new(bytes[0], bytes[1], bytes[2], tntr(&t, &bytes)).into()
                todo!()
            },
            PixelType::Rgb16Transparent(t) => {
                let bytes = self.words::<3>();
                //Argb8888::new(bytes[0], bytes[1], bytes[2], tntr(&t, &bytes)).into()
                todo!()
            },
            PixelType::GrayscaleAlpha8 => {
                let bytes = self.bytes::<2>();
                todo!()
            }
            PixelType::GrayscaleAlpha16 => {
                let bytes = self.words::<2>();
                todo!()
            }
            PixelType::RgbAlpha8 => {
                let bytes = self.bytes::<4>();
                //Argb8888::new(bytes[0], bytes[1], bytes[2], bytes[3]).into()
                todo!()
            },
            PixelType::RgbAlpha16 => {
                let bytes = self.words::<4>();
                //Argb8888::new(bytes[0], bytes[1], bytes[2], bytes[3]).into()
                todo!()
            },
            _ => self.next_opaque()
        }
    }
}

fn get_rgb_from_palette(palette: &[u8], idx: u8) -> Rgb888 {
    let idx = (idx as usize) * 3;
    if idx + 2 >= palette.len() {
        // this is an error according to PNG reference, but we prefer displaying something
        Rgb888::new(0, 0, 0)
    } else {
        let r = palette[idx];
        let g = palette[idx + 1];
        let b = palette[idx + 2];
        Rgb888::new(r,g,b)
    }
}

impl <'a, C> Iterator for PixelsIterator<'a, C, IgnoreAlpha>
where BinaryColor: Into<C>,
      Gray2: Into<C>,
      Gray4: Into<C>,
      Gray8: Into<C>,
      Rgb888: Into<C>,
{
    type Item = C;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.max_pos {
            return None;
        }
        Some(self.next_skip_alpha())
    }
}

impl <'a, C> Iterator for PixelsIterator<'a, C, AlphaColor>
where BinaryColor: Into<C>,
      Gray2: Into<C>,
      Gray4: Into<C>,
      Gray8: Into<C>,
      Rgb888: Into<C>,
//      Argb8888: Into<C>,
{
    type Item = C;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.max_pos {
            return None;
        }
        Some(self.next_alpha_color())
    }
}

impl <'a, C> Iterator for PixelsIterator<'a, C, WithBackground<C>>
where BinaryColor: Into<C>,
      Gray2: Into<C>,
      Gray4: Into<C>,
      Gray8: Into<C>,
      Rgb888: Into<C>,
      C: PixelColor,
{
    type Item = C;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.max_pos {
            return None;
        }
        let (alpha, color) = self.next_keep_alpha();
        todo!()
    }
}

impl <'a, C> Iterator for PixelsIterator<'a, C, DontDraw>
where BinaryColor: Into<C>,
      Gray2: Into<C>,
      Gray4: Into<C>,
      Gray8: Into<C>,
      Rgb888: Into<C>,
{
    type Item = (u8, C);

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.max_pos {
            return None;
        }
        Some(self.next_keep_alpha())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bits1() {
        let scanline = [0b1010_1100,0b1010_1100];
        let mut it = PixelsIterator {
            pixel_type: PixelType::Grayscale1,
            palette: None,
            scanline: &scanline,
            pos: 0,
            max_pos: 0,
            handler: IgnoreAlpha,
            _phantom: PhantomData::<Rgb888>,
        };
        assert_eq!(it.bits::<1>(), 1);
        assert_eq!(it.bits::<1>(), 0);
        assert_eq!(it.bits::<1>(), 1);
        assert_eq!(it.bits::<1>(), 0);
        assert_eq!(it.bits::<1>(), 1);
        assert_eq!(it.bits::<1>(), 1);
        assert_eq!(it.bits::<1>(), 0);
        assert_eq!(it.bits::<1>(), 0);
        assert_eq!(it.bits::<1>(), 1);
        assert_eq!(it.bits::<1>(), 0);
    }
    #[test]
    fn bits2() {
        let scanline = [0b1010_1100,0b1010_1100];
        let mut it = PixelsIterator {
            pixel_type: PixelType::Grayscale1,
            palette: None,
            scanline: &scanline,
            pos: 0,
            max_pos: 0,
            handler: IgnoreAlpha,
            _phantom: PhantomData::<Rgb888>,
        };
        assert_eq!(it.bits::<2>(), 0b10);
        assert_eq!(it.bits::<2>(), 0b10);
        assert_eq!(it.bits::<2>(), 0b11);
        assert_eq!(it.bits::<2>(), 0b00);
        assert_eq!(it.bits::<2>(), 0b10);
        assert_eq!(it.bits::<2>(), 0b10);
        assert_eq!(it.bits::<2>(), 0b11);
        assert_eq!(it.bits::<2>(), 0b00);
    }
    #[test]
    fn bits4() {
        let scanline = [0b1010_1100,0b1010_1100];
        let mut it = PixelsIterator {
            pixel_type: PixelType::Grayscale1,
            palette: None,
            scanline: &scanline,
            pos: 0,
            max_pos: 0,
            handler: IgnoreAlpha,
            _phantom: PhantomData::<Rgb888>,
        };
        assert_eq!(it.bits::<4>(), 0b1010);
        assert_eq!(it.bits::<4>(), 0b1100);
        assert_eq!(it.bits::<4>(), 0b1010);
        assert_eq!(it.bits::<4>(), 0b1100);
    }
    #[test]
    fn bytes1() {
        let scanline = [0xAA,0xBB,0xCC,0xDD];
        let mut it = PixelsIterator {
            pixel_type: PixelType::Grayscale1,
            palette: None,
            scanline: &scanline,
            pos: 0,
            max_pos: 0,
            handler: IgnoreAlpha,
            _phantom: PhantomData::<Rgb888>,
        };
        assert_eq!(it.bytes::<1>(), [0xAA]);
        assert_eq!(it.bytes::<1>(), [0xBB]);
        assert_eq!(it.bytes::<1>(), [0xCC]);
        assert_eq!(it.bytes::<1>(), [0xDD]);
    }
    #[test]
    fn bytes2() {
        let scanline = [0xAA,0xBB,0xCC,0xDD];
        let mut it = PixelsIterator {
            pixel_type: PixelType::Grayscale1,
            palette: None,
            scanline: &scanline,
            pos: 0,
            max_pos: 0,
            handler: IgnoreAlpha,
            _phantom: PhantomData::<Rgb888>,
        };
        assert_eq!(it.bytes::<2>(), [0xAA, 0xBB]);
        assert_eq!(it.bytes::<2>(), [0xCC, 0xDD]);
    }

}