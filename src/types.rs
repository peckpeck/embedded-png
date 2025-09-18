// Partly taken as is from png-decoder crate by Brian Schwind, licence MIT

use num_enum::TryFromPrimitive;
use crate::error::DecodeError;

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, TryFromPrimitive)]
pub enum ColorType {
    Grayscale = 0,
    Rgb = 2,
    Palette = 3,
    GrayscaleAlpha = 4,
    RgbAlpha = 6,
}

impl ColorType {
    pub fn sample_multiplier(&self) -> usize {
        match self {
            ColorType::Grayscale => 1,
            ColorType::Rgb => 3,
            ColorType::Palette => 1,
            ColorType::GrayscaleAlpha => 2,
            ColorType::RgbAlpha => 4,
        }
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, TryFromPrimitive)]
pub enum CompressionMethod {
    Deflate = 0,
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, TryFromPrimitive)]
pub enum FilterMethod {
    Adaptive = 0,
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, TryFromPrimitive)]
pub enum FilterType {
    None = 0,
    Sub = 1,
    Up = 2,
    Average = 3,
    Paeth = 4,
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, TryFromPrimitive)]
pub enum InterlaceMethod {
    None = 0,
    Adam7 = 1,
}

#[derive(Debug, Clone, Copy)]
pub enum PixelType<'a> {
    Grayscale1,
    Grayscale1Transparent(u8),
    Grayscale2,
    Grayscale2Transparent(u8),
    Grayscale4,
    Grayscale4Transparent(u8),
    Grayscale8,
    Grayscale8Transparent(u8),
    Grayscale16,
    Grayscale16Transparent(u16),

    Palette1(&'a [u8]),
    Palette1Transparent(&'a [u8],&'a [u8]),
    Palette2(&'a [u8]),
    Palette2Transparent(&'a [u8],&'a [u8]),
    Palette4(&'a [u8]),
    Palette4Transparent(&'a [u8],&'a [u8]),
    Palette8(&'a [u8]),
    Palette8Transparent(&'a [u8],&'a [u8]),

    GrayscaleAlpha8,
    GrayscaleAlpha16,

    Rgb8,
    Rgb8Transparent([u8; 3]),
    Rgb16,
    // keep u8 as embedded graphics doesn't support u16
    Rgb16Transparent([u8; 3]),

    RgbAlpha8,
    RgbAlpha16,
}

impl<'a> PixelType<'a> {
    pub fn new(color_type: ColorType, bit_depth: u8, palette_chunk: Option<&'a [u8]>, transparency_chunk: Option<&'a [u8]>) -> Result<Self, DecodeError> {
        let result = match (color_type, bit_depth, palette_chunk, transparency_chunk) {
            (ColorType::Grayscale,1, None, None) => PixelType::Grayscale1,
            (ColorType::Grayscale,1, None, Some(data)) => PixelType::Grayscale1Transparent(data[1] & 0b1),
            (ColorType::Grayscale,2, None, None) => PixelType::Grayscale2,
            (ColorType::Grayscale,2, None, Some(data)) => PixelType::Grayscale2Transparent(data[1] & 0b11),
            (ColorType::Grayscale,4, None, None) => PixelType::Grayscale4,
            (ColorType::Grayscale,4, None, Some(data)) => PixelType::Grayscale4Transparent(data[1] & 0b1111),
            (ColorType::Grayscale,8, None, None) => PixelType::Grayscale8,
            (ColorType::Grayscale,8, None, Some(data)) => PixelType::Grayscale8Transparent(data[1]),
            (ColorType::Grayscale,16, None, None) => PixelType::Grayscale16,
            (ColorType::Grayscale,16, None, Some(data)) => PixelType::Grayscale16Transparent((data[0] as u16) <<8 | data[1] as u16),
            (ColorType::Palette,1, Some(palette), None) => PixelType::Palette1(palette),
            (ColorType::Palette,1, Some(palette), Some(transparency)) => PixelType::Palette1Transparent(palette, transparency),
            (ColorType::Palette,2, Some(palette), None) => PixelType::Palette2(palette),
            (ColorType::Palette,2, Some(palette), Some(transparency)) => PixelType::Palette2Transparent(palette, transparency),
            (ColorType::Palette,4, Some(palette), None) => PixelType::Palette4(palette),
            (ColorType::Palette,4, Some(palette), Some(transparency)) => PixelType::Palette4Transparent(palette, transparency),
            (ColorType::Palette,8, Some(palette), None) => PixelType::Palette8(palette),
            (ColorType::Palette,8, Some(palette), Some(transparency)) => PixelType::Palette8Transparent(palette, transparency),
            (ColorType::GrayscaleAlpha,8, None, None) => PixelType::GrayscaleAlpha8,
            (ColorType::GrayscaleAlpha,16, None, None) => PixelType::GrayscaleAlpha16,
            (ColorType::Rgb,8, None, None) => PixelType::Rgb8,
            (ColorType::Rgb,8, None, Some(data)) => PixelType::Rgb8Transparent([data[1], data[3], data[5]]),
            (ColorType::Rgb,16, None, None) => PixelType::Rgb16,
            (ColorType::Rgb,16, None, Some(data)) => PixelType::Rgb16Transparent([data[0], data[2], data[4]]),
            (ColorType::RgbAlpha,8, None, None) => PixelType::RgbAlpha8,
            (ColorType::RgbAlpha,18, None, None) => PixelType::RgbAlpha16,
            _ => return Err(DecodeError::InvalidColorTypeBitDepthCombination),
        };
        Ok(result)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ChunkType {
    ImageHeader,
    Palette,
    Transparency,
    Background,
    Srgb,
    ImageData,
    ImageEnd,
    Gamma,
    Unknown([u8; 4]),
}

impl ChunkType {
    pub fn from_bytes(bytes: &[u8]) -> Self {
        match bytes {
            b"IHDR" => ChunkType::ImageHeader,
            b"PLTE" => ChunkType::Palette,
            b"tRNS" => ChunkType::Transparency,
            b"bKGD" => ChunkType::Background,
            b"sRGB" => ChunkType::Srgb,
            b"IDAT" => ChunkType::ImageData,
            b"IEND" => ChunkType::ImageEnd,
            b"gAMA" => ChunkType::Gamma,
            // unwrap allowed, we have a single private caller here
            unknown_chunk_type => ChunkType::Unknown(unknown_chunk_type.try_into().unwrap()),
        }
    }
}
