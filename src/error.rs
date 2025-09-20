use core::error::Error;
use core::fmt::{Display, Formatter};
use miniz_oxide::inflate::TINFLStatus;

#[derive(Debug, Clone, PartialEq, Eq)]
/// Errors that can be returned wby embedded-png operations
pub enum DecodeError {
    // TODO document
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
    InvalidPixelTypeCombination,
    InvalidCompressionMethod,
    InvalidFilterMethod,
    InvalidFilterType,
    InvalidInterlaceMethod,

    /// indexing bytes in the image with not work because usize is too small (unlikely)
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
            DecodeError::InvalidPixelTypeCombination => "invalid color type * bit depth * transparency combination",
            DecodeError::InvalidCompressionMethod => "invalid compression method",
            DecodeError::InvalidFilterMethod => "invalid filter method",
            DecodeError::InvalidFilterType => "invalid filter type",
            DecodeError::InvalidInterlaceMethod => "invalid interlace method",
            DecodeError::IntegerOverflow => "integer overflow (usize too small)",
        };
        write!(f, "embedded-png Error: {}", s)
    }
}

impl Error for DecodeError {}