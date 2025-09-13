// Mostly taken as is from png-decoder crate by Brian Schwind, licence MIT

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
pub enum PixelType {
    Grayscale1,
    Grayscale2,
    Grayscale4,
    Grayscale8,
    Grayscale16,

    Rgb8,
    Rgb16,

    Palette1,
    Palette2,
    Palette4,
    Palette8,

    GrayscaleAlpha8,
    GrayscaleAlpha16,

    RgbAlpha8,
    RgbAlpha16,
}

impl PixelType {
    pub fn new(color_type: ColorType, bit_depth: u8) -> Result<Self, DecodeError> {
        let result = match (color_type, bit_depth) {
            (ColorType::Grayscale,1) => PixelType::Grayscale1,
            (ColorType::Grayscale,2) => PixelType::Grayscale2,
            (ColorType::Grayscale,4) => PixelType::Grayscale4,
            (ColorType::Grayscale,8) => PixelType::Grayscale8,
            (ColorType::Grayscale,16) => PixelType::Grayscale16,
            (ColorType::Rgb,8) => PixelType::Rgb8,
            (ColorType::Rgb,16) => PixelType::Rgb16,
            (ColorType::Palette,1) => PixelType::Palette1,
            (ColorType::Palette,2) => PixelType::Palette2,
            (ColorType::Palette,4) => PixelType::Palette4,
            (ColorType::Palette,8) => PixelType::Palette8,
            (ColorType::GrayscaleAlpha,8) => PixelType::GrayscaleAlpha8,
            (ColorType::GrayscaleAlpha,16) => PixelType::GrayscaleAlpha16,
            (ColorType::RgbAlpha,8) => PixelType::RgbAlpha8,
            (ColorType::RgbAlpha,18) => PixelType::RgbAlpha16,
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

#[derive(Debug, Clone)]
pub enum TransparencyChunk<'a> {
    Palette(&'a [u8]),
    Grayscale(u8),
    Rgb(u8, u8, u8),
}

impl<'a> TransparencyChunk<'a> {
    pub fn from_data(data: &'a [u8], pixel_type: PixelType) -> Option<Self> {
        log::info!("transparency {:?}", pixel_type);
        match pixel_type {
            PixelType::Grayscale1 => Some(TransparencyChunk::Grayscale(data[1] & 0b1)),
            PixelType::Grayscale2 => Some(TransparencyChunk::Grayscale(data[1] & 0b11)),
            PixelType::Grayscale4 => Some(TransparencyChunk::Grayscale(data[1] & 0b1111)),
            PixelType::Grayscale8 => Some(TransparencyChunk::Grayscale(data[1])),
            PixelType::Grayscale16 => Some(TransparencyChunk::Grayscale(data[0])),
            PixelType::Rgb8 => Some(TransparencyChunk::Rgb(data[1], data[3], data[5])),
            PixelType::Rgb16 => Some(TransparencyChunk::Rgb(data[0], data[2], data[4])),
            PixelType::Palette1 => Some(TransparencyChunk::Palette(data)),
            PixelType::Palette2 => Some(TransparencyChunk::Palette(data)),
            PixelType::Palette4 => Some(TransparencyChunk::Palette(data)),
            PixelType::Palette8 => Some(TransparencyChunk::Palette(data)),
            PixelType::GrayscaleAlpha8 => None,
            PixelType::GrayscaleAlpha16 => None,
            PixelType::RgbAlpha8 => None,
            PixelType::RgbAlpha16 => None,
        }
    }
}