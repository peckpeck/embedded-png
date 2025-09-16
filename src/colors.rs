use std::marker::PhantomData;
use embedded_graphics_core::Pixel;
use embedded_graphics_core::pixelcolor::Argb8888;
use embedded_graphics_core::primitives::Rectangle;
use crate::ParsedPng;
use crate::types::{PixelType, TransparencyChunk};

/*
pub fn get_iterator<C>(pixel_type: PixelType, scanline: &[u8]) -> dyn Iterator<Item = C>
where Argb8888: Into<C> {
    match pixel_type {
        PixelType::RgbAlpha8 => RgbAlpha8 { scanline, pos: 0, _phantom: PhantomData },
        PixelType::Rgb8 => Rgb8 { scanline, pos: 0, _phantom: PhantomData },
        _ => todo!()
    }

}


pub fn get2<C, I: IntoIterator<Item = C>>(pixel_type: PixelType, scanline: &[u8]) -> I {
    match pixel_type {
        PixelType::RgbAlpha8 => RgbAlpha8 { scanline, pos: 0, _phantom: PhantomData }.into(),
        PixelType::Rgb8 => Rgb8 { scanline, pos: 0, _phantom: PhantomData }.into(),
        _ => todo!()
    }
    todo!()
}
*/

pub struct PixelsIterator<'a, C> {
    pixel_type: PixelType<'a>,
    palette: Option<&'a [u8]>,
    scanline: &'a [u8],
    pos: usize,
    bit_pos: usize, // for bit packed pixels
    _phantom: PhantomData<C>,
}

impl <'a, C> PixelsIterator<'a, C> {
    pub fn new(png: &ParsedPng<'a>, scanline: &'a [u8]) -> Self {
        PixelsIterator {
            pixel_type: png.pixel_type,
            palette: png.palette,
            scanline,
            pos: 0,
            bit_pos: 0,
            _phantom: PhantomData,
        }
    }
}


impl <'a, C> Iterator for PixelsIterator<'a, C>
where C: Into<C> {
    type Item = C;

    fn next(&mut self) -> Option<Self::Item> {
        match self.pixel_type {
            PixelType::Grayscale1 => {
                let value = (self.scanline[self.pos] << self.bit_pos) & 0b10000000;
                let byte = self.scanline[self.pos / 8];
                let bit_offset = 7 - self.pos % 8;
                let grayscale_val = (byte >> bit_offset) & 1;

                let pixel_val = grayscale_val * 255;

                Some((pixel_val, pixel_val, pixel_val, alpha))
            },
            PixelType::Grayscale2 => {
                let byte = self.scanline[self.pos / 4];
                let bit_offset = 6 - ((self.pos % 4) * 2);
                let grayscale_val = (byte >> bit_offset) & 0b11;

                let alpha = match self.transparency {
                    Some(TransparencyChunk::Grayscale(transparent_val))
                    if grayscale_val == transparent_val =>
                        {
                            0
                        },
                    _ => 255,
                };

                // TODO - use a lookup table
                let pixel_val = ((grayscale_val as f32 / 3.0) * 255.0) as u8;

                Some((pixel_val, pixel_val, pixel_val, alpha))
            },
            PixelType::Grayscale4 => {
                let byte = self.scanline[self.pos / 2];
                let bit_offset = 4 - ((self.pos % 2) * 4);
                let grayscale_val = (byte >> bit_offset) & 0b1111;

                let alpha = match self.transparency {
                    Some(TransparencyChunk::Grayscale(transparent_val))
                    if grayscale_val == transparent_val =>
                        {
                            0
                        },
                    _ => 255,
                };

                // TODO - use a lookup table
                let pixel_val = ((grayscale_val as f32 / 15.0) * 255.0) as u8;
                Some((pixel_val, pixel_val, pixel_val, alpha))
            },
            PixelType::Grayscale8 => {
                let byte = self.scanline[self.pos];

                let alpha = match self.transparency {
                    Some(TransparencyChunk::Grayscale(transparent_val))
                    if byte == transparent_val =>
                        {
                            0
                        },
                    _ => 255,
                };
                Some((byte, byte, byte, alpha))
            },
            PixelType::Grayscale16 => {
                let offset = self.pos * 2;
                let grayscale_val =
                    u16::from_be_bytes([self.scanline[offset], self.scanline[offset + 1]]);

                let pixel_val = u16_to_u8(grayscale_val);

                // TODO(bschwind) - This may need to be compared to the original
                //                  16-bit transparency value, instead of the transformed
                //                  8-bit value.
                let alpha = match self.extra_chunks.transparency {
                    Some(TransparencyChunk::Grayscale(transparent_val))
                    if pixel_val == transparent_val =>
                        {
                            0
                        },
                    _ => 255,
                };

                Some((pixel_val, pixel_val, pixel_val, alpha))
            },
            PixelType::Rgb8 => {
                let offset = self.pos * 3;
                let r = self.scanline[offset];
                let g = self.scanline[offset + 1];
                let b = self.scanline[offset + 2];

                let alpha = match self.transparency {
                    Some(TransparencyChunk::Rgb(t_r, t_g, t_b))
                    if r == t_r && g == t_g && b == t_b =>
                        {
                            0
                        },
                    _ => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::Rgb16 => {
                let offset = self.pos * 6;
                let r = u16::from_be_bytes([self.scanline[offset], self.scanline[offset + 1]]);
                let g = u16::from_be_bytes([self.scanline[offset + 2], self.scanline[offset + 3]]);
                let b = u16::from_be_bytes([self.scanline[offset + 4], self.scanline[offset + 5]]);

                let r = u16_to_u8(r);
                let g = u16_to_u8(g);
                let b = u16_to_u8(b);

                let alpha = match self.transparency {
                    Some(TransparencyChunk::Rgb(t_r, t_g, t_b))
                    if r == t_r && g == t_g && b == t_b =>
                        {
                            0
                        },
                    _ => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::Palette1 => {
                let byte = self.scanline[self.pos / 8];
                let bit_offset = 7 - self.pos % 8;
                let palette_idx = ((byte >> bit_offset) & 1) as usize;

                let offset = palette_idx * 3;

                let palette = self.palette.unwrap();
                let r = palette[offset];
                let g = palette[offset + 1];
                let b = palette[offset + 2];

                let alpha: u8 = match self.transparency {
                    Some(TransparencyChunk::Palette(data)) => {
                        *data.get(palette_idx).unwrap_or(&255)
                    },
                    Some(_) | None => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::Palette2 => {
                let byte = self.scanline[self.pos / 4];
                let bit_offset = 6 - ((self.pos % 4) * 2);
                let palette_idx = ((byte >> bit_offset) & 0b11) as usize;

                let offset = palette_idx * 3;

                let palette = self.palette.unwrap();
                let r = palette[offset];
                let g = palette[offset + 1];
                let b = palette[offset + 2];

                let alpha: u8 = match self.transparency {
                    Some(TransparencyChunk::Palette(data)) => {
                        *data.get(palette_idx).unwrap_or(&255)
                    },
                    Some(_) | None => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::Palette4 => {
                let byte = self.scanline[self.pos / 2];
                let bit_offset = 4 - ((self.pos % 2) * 4);
                let palette_idx = ((byte >> bit_offset) & 0b1111) as usize;

                let offset = palette_idx * 3;

                let palette = self.palette.unwrap();
                let r = palette[offset];
                let g = palette[offset + 1];
                let b = palette[offset + 2];

                let alpha: u8 = match self.transparency {
                    Some(TransparencyChunk::Palette(data)) => {
                        *data.get(palette_idx).unwrap_or(&255)
                    },
                    Some(_) | None => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::Palette8 => {
                let offset = self.scanline[self.pos] as usize * 3;

                let palette = self.palette.unwrap();
                let r = palette[offset];
                let g = palette[offset + 1];
                let b = palette[offset + 2];

                let alpha: u8 = match self.transparency {
                    Some(TransparencyChunk::Palette(data)) => *data.get(offset).unwrap_or(&255),
                    Some(_) | None => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::GrayscaleAlpha8 => {
                let offset = self.pos * 2;
                let grayscale_val = self.scanline[offset];
                let alpha = self.scanline[offset + 1];

                Some((grayscale_val, grayscale_val, grayscale_val, alpha))
            },
            PixelType::GrayscaleAlpha16 => {
                let offset = self.pos * 4;
                let grayscale_val =
                    u16::from_be_bytes([self.scanline[offset], self.scanline[offset + 1]]);
                let alpha =
                    u16::from_be_bytes([self.scanline[offset + 2], self.scanline[offset + 3]]);

                let grayscale_val = u16_to_u8(grayscale_val);
                let alpha = u16_to_u8(alpha);

                Some((grayscale_val, grayscale_val, grayscale_val, alpha))
            },
            PixelType::RgbAlpha8 => {
                let offset = self.pos * 4;
                let r = self.scanline[offset];
                let g = self.scanline[offset + 1];
                let b = self.scanline[offset + 2];
                let a = self.scanline[offset + 3];

                Some((r, g, b, a))
            },
            PixelType::RgbAlpha16 => {
                let offset = self.pos * 8;
                let r = u16::from_be_bytes([self.scanline[offset], self.scanline[offset + 1]]);
                let g = u16::from_be_bytes([self.scanline[offset + 2], self.scanline[offset + 3]]);
                let b = u16::from_be_bytes([self.scanline[offset + 4], self.scanline[offset + 5]]);
                let a = u16::from_be_bytes([self.scanline[offset + 6], self.scanline[offset + 7]]);

                let r = u16_to_u8(r);
                let g = u16_to_u8(g);
                let b = u16_to_u8(b);
                let a = u16_to_u8(a);

                Some((r, g, b, a))
            },
        }
    }
}
