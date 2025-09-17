use core::marker::PhantomData;
use embedded_graphics_core::pixelcolor::{Argb8888, BinaryColor, Gray2, Gray4, Gray8, Rgb888};
use crate::ParsedPng;
use crate::types::PixelType;

pub struct PixelsIterator<'a, C> {
    pixel_type: PixelType<'a>,
    palette: Option<&'a [u8]>,
    scanline: &'a [u8],
    pos: usize,
    max_pos: usize, // for bit packed pixels
    _phantom: PhantomData<C>,
}

impl <'a, C> PixelsIterator<'a, C> {
    pub fn new(png: &ParsedPng<'a>, scanline: &'a [u8]) -> Self {
        let max_pos = png.header.color_type.sample_multiplier() * png.header.width;
        PixelsIterator {
            pixel_type: png.pixel_type,
            palette: png.palette,
            scanline,
            pos: 0,
            max_pos,
            _phantom: PhantomData,
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

impl <'a, C> Iterator for PixelsIterator<'a, C>
where BinaryColor: Into<C>,
      Gray2: Into<C>,
      Gray4: Into<C>,
      Gray8: Into<C>,
      Rgb888: Into<C>,
      Argb8888: Into<C>,
{
    type Item = C;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.max_pos {
            return None;
        }
        Some(match self.pixel_type {
            PixelType::Grayscale1 => {
                let byte = self.scanline[self.pos>>3];
                let bit = (byte << (self.pos & 7)) & 0b1000_0000;
                self.pos += 1;
                if bit != 0 {
                    BinaryColor::On
                } else {
                    BinaryColor::Off
                }.into()
            },
            PixelType::Grayscale2 => {
                let byte = self.scanline[self.pos>>2];
                let bits = (byte << ((self.pos & 3) << 1)) & 0b1100_0000;
                self.pos += 1;
                Gray2::new(bits >> 6).into()
            },
            PixelType::Grayscale4 => {
                let byte = self.scanline[self.pos>>1];
                let bits = (byte << ((self.pos & 1) << 2)) & 0b1111_0000;
                self.pos += 1;
                Gray4::new(bits >> 4).into()
            },
            PixelType::Grayscale8 => {
                let byte = self.scanline[self.pos];
                self.pos += 1;
                Gray8::new(byte).into()
            },
            PixelType::Grayscale16 => {
                // not supported by embedded-graphics, convert to Gray8
                let byte = self.scanline[self.pos]; // big endian
                self.pos += 2;
                Gray8::new(byte).into()
            },
            PixelType::Rgb8 => {
                let r = self.scanline[self.pos];
                let g = self.scanline[self.pos + 1];
                let b = self.scanline[self.pos + 2];
                self.pos += 3;
                Rgb888::new(r,g,b).into()
            },
            PixelType::Rgb16 => {
                // not supported by embedded-graphics, convert to Rgb888
                let r = self.scanline[self.pos];      // big endian
                let g = self.scanline[self.pos + 2];
                let b = self.scanline[self.pos + 4];
                Rgb888::new(r,g,b).into()
            },
            PixelType::Palette1(palette) => {
                let byte = self.scanline[self.pos>>3];
                let bit = (byte << (self.pos & 7)) & 0b1000_0000;
                self.pos += 1;
                get_rgb_from_palette(palette, bit >> 7).into()
            },
            PixelType::Palette2(palette) => {
                let byte = self.scanline[self.pos>>2];
                let bits = (byte << ((self.pos & 3) << 1)) & 0b1100_0000;
                self.pos += 1;
                get_rgb_from_palette(palette, bits >> 6).into()
            },
            PixelType::Palette4(palette) => {
                let byte = self.scanline[self.pos>>1];
                let bits = (byte << ((self.pos & 1) << 2)) & 0b1111_0000;
                self.pos += 1;
                get_rgb_from_palette(palette, bits >> 4).into()
            },
            PixelType::Palette8(palette) => {
                let byte = self.scanline[self.pos];
                self.pos += 1;
                get_rgb_from_palette(palette, byte).into()
            },
            PixelType::RgbAlpha8 => {
                let r = self.scanline[self.pos];
                let g = self.scanline[self.pos + 1];
                let b = self.scanline[self.pos + 2];
                let a = self.scanline[self.pos + 3];
                self.pos += 4;
                Argb8888::new(r, g, b, a).into()
            },
            _ => todo!("alpha colors not ready"),
        })
    }
}
