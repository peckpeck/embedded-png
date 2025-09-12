use core::error::Error;
use core::fmt::{Display, Formatter};
use miniz_oxide::inflate::TINFLStatus;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecodeError {
    InvalidMagicBytes,
    MissingBytes,
    HeaderChunkNotFirst,
    EndChunkNotLast,
    InvalidChunkType,
    InvalidChunk,
    Decompress(TINFLStatus),

    IncorrectChunkCrc,
    InvalidBitDepth,
    InvalidColorType,
    InvalidColorTypeBitDepthCombination,
    InvalidCompressionMethod,
    InvalidFilterMethod,
    InvalidFilterType,
    InvalidInterlaceMethod,

    // The width/height specified in the image contains too many
    // bytes to address with a usize on this platform.
    IntegerOverflow,
}

impl Display for DecodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            DecodeError::InvalidMagicBytes => "invalid magic bytes",
            DecodeError::MissingBytes => "missing bytes",
            DecodeError::HeaderChunkNotFirst => "header chunk not first",
            DecodeError::EndChunkNotLast => "end chunk not last",
            DecodeError::InvalidChunkType => "invalid chunk type",
            DecodeError::InvalidChunk => "invalid chunk",
            DecodeError::Decompress(_) => "decompression",
            DecodeError::IncorrectChunkCrc => "incorrect chunk crc",
            DecodeError::InvalidBitDepth => "invalid bit depth",
            DecodeError::InvalidColorType => "invalid color type",
            DecodeError::InvalidColorTypeBitDepthCombination => "invalid color type bitdepth combination",
            DecodeError::InvalidCompressionMethod => "invalid compression method",
            DecodeError::InvalidFilterMethod => "invalid filter method",
            DecodeError::InvalidFilterType => "invalid filter type",
            DecodeError::InvalidInterlaceMethod => "invalid interlace method",
            DecodeError::IntegerOverflow => "integer overflow",
        };
        write!(f, "embedded-png Error: {}", s)
    }
}

impl Error for DecodeError {}