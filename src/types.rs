// Mostly taken as is from png-decoder crate by Brian Schwind, licence MIT

use num_enum::TryFromPrimitive;
use crate::error::DecodeError;
use crate::png::Chunk;

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

    Rgb8,
    Rgb8Transparent(u8,u8,u8),
    Rgb16,
    Rgb16Transparent(u16,u16,u16),

    Palette1,
    Palette1Transparent(&'a [u8]),
    Palette2,
    Palette2Transparent(&'a [u8]),
    Palette4,
    Palette4Transparent(&'a [u8]),
    Palette8,
    Palette8Transparent(&'a [u8]),

    GrayscaleAlpha8,
    GrayscaleAlpha16,

    RgbAlpha8,
    RgbAlpha16,
}

impl<'a> PixelType<'a> {
    pub fn new(color_type: ColorType, bit_depth: u8, transparency_chunk: Option<&'a [u8]>) -> Result<Self, DecodeError> {
        let result = match (color_type, bit_depth, transparency_chunk) {
            (ColorType::Grayscale,1, None) => PixelType::Grayscale1,
            (ColorType::Grayscale,1, Some(data)) => PixelType::Grayscale1Transparent(data[1] & 0b1),
            (ColorType::Grayscale,2, None) => PixelType::Grayscale2,
            (ColorType::Grayscale,2, Some(data)) => PixelType::Grayscale2Transparent(data[1] & 0b11),
            (ColorType::Grayscale,4, None) => PixelType::Grayscale4,
            (ColorType::Grayscale,4, Some(data)) => PixelType::Grayscale4Transparent(data[1] & 0b1111),
            (ColorType::Grayscale,8, None) => PixelType::Grayscale8,
            (ColorType::Grayscale,8, Some(data)) => PixelType::Grayscale8Transparent(data[1]),
            (ColorType::Grayscale,16, None) => PixelType::Grayscale16,
            (ColorType::Grayscale,16, Some(data)) => PixelType::Grayscale16Transparent((data[0] as u16) <<8 | data[1] as u16),
            (ColorType::Rgb,8, None) => PixelType::Rgb8,
            (ColorType::Rgb,8, Some(data)) => PixelType::Rgb8Transparent(data[1], data[3], data[5]),
            (ColorType::Rgb,16, None) => PixelType::Rgb16,
            (ColorType::Rgb,16, Some(data)) => PixelType::Rgb16Transparent((data[0] as u16) <<8 | data[1] as u16,
                                                                           (data[2] as u16) <<8 | data[3] as u16,
                                                                           (data[4] as u16) <<8 | data[5] as u16),
            (ColorType::Palette,1, None) => PixelType::Palette1,
            (ColorType::Palette,1, Some(data)) => PixelType::Palette1Transparent(data),
            (ColorType::Palette,2, None) => PixelType::Palette2,
            (ColorType::Palette,2, Some(data)) => PixelType::Palette2Transparent(data),
            (ColorType::Palette,4, None) => PixelType::Palette4,
            (ColorType::Palette,4, Some(data)) => PixelType::Palette4Transparent(data),
            (ColorType::Palette,8, None) => PixelType::Palette8,
            (ColorType::Palette,8, Some(data)) => PixelType::Palette8Transparent(data),
            (ColorType::GrayscaleAlpha,8, None) => PixelType::GrayscaleAlpha8,
            (ColorType::GrayscaleAlpha,16, None) => PixelType::GrayscaleAlpha16,
            (ColorType::RgbAlpha,8, None) => PixelType::RgbAlpha8,
            (ColorType::RgbAlpha,18, None) => PixelType::RgbAlpha16,
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

/*
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
}*/