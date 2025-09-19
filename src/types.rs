// Partly taken as is from png-decoder crate by Brian Schwind, licence MIT

use num_enum::TryFromPrimitive;
use crate::error::DecodeError;

/// Image color type as defined in the PNG spec
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, TryFromPrimitive)]
pub enum ColorType {
    // Each pixel is a grayscale sample
    Grayscale = 0,
    // Each pixel is an R,G,B triple.
    Rgb = 2,
    // Each pixel is a palette index; a PLTE chunk must appear.
    Palette = 3,
    // Each pixel is a grayscale sample, followed by an alpha sample.
    GrayscaleAlpha = 4,
    // Each pixel is an R,G,B triple, followed by an alpha sample.
    RgbAlpha = 6,
}

impl ColorType {
    /// How many bytes should be processed at once when decoding a scanline for a 8bits depth color
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

/// The compression method as defined in the PNG spec
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, TryFromPrimitive)]
pub enum CompressionMethod {
    /// Only one compression method is supported by PNG
    Deflate = 0,
}

/// The filter method as defined in the PNG spec
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, TryFromPrimitive)]
pub enum FilterMethod {
    /// Only one filter method is supported by PNG
    Adaptive = 0,
}

/// The filter type corresponding to adaptive filtering as defined in the PNG spec
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, TryFromPrimitive)]
pub enum FilterType {
    /// No filter, just copy bytes
    None = 0,
    /// Subtract prior pixel
    Sub = 1,
    /// Subtract upper pixel
    Up = 2,
    /// Subtract mean of prior and upper pixel
    Average = 3,
    /// Subtract Paeth predictor
    Paeth = 4,
}

/// The interlace method as defined in the PNG spec
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, TryFromPrimitive)]
pub enum InterlaceMethod {
    /// No image interlacing
    None = 0,
    /// 7 pass interlacing
    Adam7 = 1,
}

/// A pixel type defines a valid way to represent a pixel in PNG
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
    /// Create a new pixel type from underlying data
    /// Return an Error if provided parameters are not compatible
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
            _ => return Err(DecodeError::InvalidPixelTypeCombination),
        };
        Ok(result)
    }
}

/// Chunk types as defined in the PNG spec
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ChunkType {
    /// IHDR Image header
    ImageHeader,
    /// PLTE Palette
    Palette,
    /// IDAT Image data
    ImageData,
    /// IEND Image trailer
    ImageEnd,
    /// tRNS Transparency
    Transparency,
    /// gAMA Image gamma
    Gamma,
    /// cHRM Primary chromaticities
    Chromaticity,
    /// sRGB Standard RGB color space
    Srgb,
    /// iCCP Embedded ICC profile
    IccProfile,
    /// tEXt Textual data
    Text,
    /// zTXt Compressed textual data
    CompressedText,
    /// iTXt International textual data
    InternationalText,
    /// bKGD Background color
    Background,
    /// pHYs Physical pixel dimensions
    PhysicalPixel,
    /// sBIT Significant bits
    SignificantBits,
    /// sPLT Suggested palette
    SuggestedPalette,
    /// hIST Palette histogram
    PaletteHistogram,
    /// tIME Image last-modification time
    Time,
    Unknown([u8; 4]),
}

impl ChunkType {
    /// Extract a chunk type from its header
    pub fn from_bytes(bytes: &[u8]) -> Self {
        match bytes {
            b"IHDR" => ChunkType::ImageHeader,
            b"PLTE" => ChunkType::Palette,
            b"IDAT" => ChunkType::ImageData,
            b"IEND" => ChunkType::ImageEnd,
            b"tRNS" => ChunkType::Transparency,
            b"gAMA" => ChunkType::Gamma,
            b"cHRM" => ChunkType::Chromaticity,
            b"sRGB" => ChunkType::Srgb,
            b"iCCP" => ChunkType::IccProfile,
            b"tEXt" => ChunkType::Text,
            b"zTXt" => ChunkType::CompressedText,
            b"iTXt" => ChunkType::InternationalText,
            b"bKGD" => ChunkType::Background,
            b"pHYs" => ChunkType::PhysicalPixel,
            b"sBIT" => ChunkType::SignificantBits,
            b"sPLT" => ChunkType::SuggestedPalette,
            b"hIST" => ChunkType::PaletteHistogram,
            b"tIME" => ChunkType::Time,
            // unwrap allowed, we have a single private caller here
            unknown_chunk_type => ChunkType::Unknown(unknown_chunk_type.try_into().unwrap()),
        }
    }
}
