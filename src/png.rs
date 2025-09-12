use core::convert::{TryFrom, TryInto};
use crc32fast::Hasher;
use log::info;
use miniz_oxide::inflate::core::DecompressorOxide;
use miniz_oxide::inflate::TINFLStatus;
use num_enum::TryFromPrimitive;
use crate::error::DecodeError;
use crate::read_u32;
use crate::types::*;

const PNG_MAGIC_BYTES: &[u8] = &[137, 80, 78, 71, 13, 10, 26, 10];

pub struct ParsedPng<'a> {
    pub header: PngHeader,
    // no need to store all data chunks separately, they HAVE TO be consecutive
    data_chunks: &'a[u8],
    pixel_type: PixelType,
    palette: Option<&'a [u8]>,
    transparency: Option<TransparencyChunk<'a>>,
    background: Option<&'a [u8]>,
}

struct ChunkDecompressor<'src, 'buf> {
    decompressor: DecompressorOxide,
    // slice containing ImageData chunks
    data_chunks: &'src [u8],
    // position of the next chunk in this slice
    next_chunk_start: Option<usize>,
    // slice of the current data chunk
    current_chunk: Option<&'src [u8]>,
    // circular buffer where decompressed data is written
    buffer: &'buf mut [u8],
    // first waiting decompressed byte
    buffer_pos: usize,
    // size of waiting decompressed data
    buffer_count: usize,
    // non-circular buffer data that must be kept out of circular buffer during decompress call
    buffer_extra: &'buf mut [u8],
    // size of waiting data in buffer_extra
    extra_count: usize,
}

impl<'src, 'buf> ChunkDecompressor<'src, 'buf> {
    // advance current chunk by one, result in self.current_chunk
    fn next_chunk(&mut self) {
        let next_start = match self.next_chunk_start {
            None => return,
            Some(x) => x,
        };
        // it was already unwrapped during first parse
        loop {
            if next_start >= self.data_chunks.len() {
                self.current_chunk = None;
                self.next_chunk_start = None;
                return;
            }
            let next_chunk = read_chunk(self.data_chunks, next_start, false).unwrap();
            self.next_chunk_start = Some(next_chunk.end);
            if next_chunk.chunk_type != ChunkType::ImageData {
                continue;
            } else {
                self.current_chunk = Some(next_chunk.data);
                return;
            }
        }
    }

    // get size bytes from circular buffer, or an error if it's not possible
    fn get_bytes(&mut self, size: usize) -> Result<&'buf [u8]> {
        if self.buffer_count + self.extra_count >= size {
            return Ok(Some(self.scanline2(size)?));
        }

        if self.next_chunk >= self.data_chunks.len() {
            info!("ended");
            return Ok(None);
        }

    }
}

pub struct ScanlineData<'a> {
    filter_type: FilterType,
    // a scanline spans at most 3 buffers
    buffer1: &'a mut [u8],
    buffer2: &'a mut [u8],
    buffer3: &'a mut [u8],
    // optimization to avoid computing position all the time
    buffer3_pos: usize,
    // used for bound checking only, TODO maybe remove
    total_size: usize,
}


#[derive(Debug, Clone)]
pub struct PngHeader {
    pub width: u32,
    pub height: u32,
    pub bit_depth: u8,
    pub color_type: ColorType,
    pub compression_method: CompressionMethod,
    pub filter_method: FilterMethod,
    pub interlace_method: InterlaceMethod,
}

impl PngHeader {
    fn from_chunk(chunk: &Chunk) -> Result<Self, DecodeError> {
        if chunk.chunk_type != ChunkType::ImageHeader {
            return Err(DecodeError::InvalidChunkType);
        }

        if chunk.data.len() < 13 {
            return Err(DecodeError::MissingBytes);
        }

        let width = read_u32(chunk.data, 0);
        let height = read_u32(chunk.data, 4);
        let bit_depth = chunk.data[8];
        if !bit_depth.is_power_of_two() || bit_depth > 16 {
            return Err(DecodeError::InvalidBitDepth);
        }
        let color_type = chunk.data[9];
        let compression_method = chunk.data[10];
        let filter_method = chunk.data[11];
        let interlace_method = chunk.data[12];

        Ok(PngHeader {
            width,
            height,
            bit_depth,
            color_type: TryFrom::try_from(color_type)
                .map_err(|_| DecodeError::InvalidColorType)?,
            compression_method: TryFrom::try_from(compression_method)
                .map_err(|_| DecodeError::InvalidCompressionMethod)?,
            filter_method: TryFrom::try_from(filter_method)
                .map_err(|_| DecodeError::InvalidFilterMethod)?,
            interlace_method: TryFrom::try_from(interlace_method)
                .map_err(|_| DecodeError::InvalidInterlaceMethod)?,
        })
    }

    fn pass_info(&self, pass: usize) -> (u32, u32) {
        match pass {
            1 => {
                let pass_width = (self.width + 7) / 8;
                let pass_height = (self.height + 7) / 8;
                (pass_width, pass_height)
            },
            2 => {
                let pass_width = (self.width / 8) + ((self.width % 8) / 5);
                let pass_height = (self.height + 7) / 8;
                (pass_width, pass_height)
            },
            3 => {
                let pass_width = ((self.width / 8) * 2) + (self.width % 8 + 3) / 4;
                let pass_height = (self.height / 8) + ((self.height % 8) / 5);
                (pass_width, pass_height)
            },
            4 => {
                let pass_width = ((self.width / 8) * 2) + (self.width % 8 + 1) / 4;
                let pass_height = (self.height + 3) / 4;
                (pass_width, pass_height)
            },
            5 => {
                let pass_width = (self.width / 2) + (self.width % 2);
                let pass_height = ((self.height / 8) * 2) + (self.height % 8 + 1) / 4;
                (pass_width, pass_height)
            },
            6 => {
                let pass_width = self.width / 2;
                let pass_height = (self.height / 2) + (self.height % 2);
                (pass_width, pass_height)
            },
            7 => {
                let pass_width = self.width;
                let pass_height = self.height / 2;
                (pass_width, pass_height)
            },
            _ => (0, 0),
        }
    }

    fn bytes_per_pixel(&self) -> usize {
        ((self.bit_depth as usize * self.color_type.sample_multiplier()) + 7) / 8
    }

    fn bytes_per_scanline_max(&self) -> Result<usize, png_decoder::DecodeError> {
        let bps = self.bytes_per_pixel() as u64 * self.width as u64;
        bps.try_into().map_err(|_| png_decoder::DecodeError::IntegerOverflow)
    }

    fn total_scanline_bytes(&self) -> Result<usize, png_decoder::DecodeError> {
        let pixel_count = self.bytes_per_pixel() as u64 * self.width as u64 * self.height as u64;
        let filter_count = match self.interlace_method {
            InterlaceMethod::None => self.height as u64,
            InterlaceMethod::Adam7 => (1..=7).fold(0_u64, |a,x| a + self.pass_info(x).1 as u64 ),
        };
        (pixel_count+filter_count).try_into().map_err(|_| png_decoder::DecodeError::IntegerOverflow)
    }

}

#[derive(Debug, Clone)]
enum TransparencyChunk<'a> {
    Palette(&'a [u8]),
    Grayscale(u8),
    Rgb(u8, u8, u8),
}

impl<'a> TransparencyChunk<'a> {
    fn from_chunk(chunk: &Chunk<'a>, pixel_type: PixelType) -> Option<Self> {
        log::info!("transparency {:?}", pixel_type);
        match pixel_type {
            PixelType::Grayscale1 => Some(TransparencyChunk::Grayscale(chunk.data[1] & 0b1)),
            PixelType::Grayscale2 => Some(TransparencyChunk::Grayscale(chunk.data[1] & 0b11)),
            PixelType::Grayscale4 => Some(TransparencyChunk::Grayscale(chunk.data[1] & 0b1111)),
            PixelType::Grayscale8 => Some(TransparencyChunk::Grayscale(chunk.data[1])),
            PixelType::Grayscale16 => Some(TransparencyChunk::Grayscale(chunk.data[0])),
            PixelType::Rgb8 => Some(TransparencyChunk::Rgb(chunk.data[1], chunk.data[3], chunk.data[5])),
            PixelType::Rgb16 => Some(TransparencyChunk::Rgb(chunk.data[0], chunk.data[2], chunk.data[4])),
            PixelType::Palette1 => Some(TransparencyChunk::Palette(chunk.data)),
            PixelType::Palette2 => Some(TransparencyChunk::Palette(chunk.data)),
            PixelType::Palette4 => Some(TransparencyChunk::Palette(chunk.data)),
            PixelType::Palette8 => Some(TransparencyChunk::Palette(chunk.data)),
            PixelType::GrayscaleAlpha8 => None,
            PixelType::GrayscaleAlpha16 => None,
            PixelType::RgbAlpha8 => None,
            PixelType::RgbAlpha16 => None,
        }
    }
}

#[derive(Debug)]
struct Chunk<'a> {
    chunk_type: ChunkType,
    data: &'a [u8],
    start: usize,
    end: usize,
    _crc: u32,
}

impl ChunkType {
    fn from_bytes(bytes: &[u8]) -> Self {
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

fn read_chunk(bytes: &[u8], start: usize, check_crc: bool) -> Result<Chunk, DecodeError> {
    if bytes.len() - start < 4 {
        return Err(DecodeError::MissingBytes);
    }

    let length = read_u32(bytes, start) as usize;
    let end = start + length + 12;

    if bytes.len() < end {
        return Err(DecodeError::MissingBytes);
    }
    let chunk_bytes = &bytes[start..end];

    let chunk_type = ChunkType::from_bytes(&chunk_bytes[4..8]);

    let crc_offset = length + 8;
    let crc = if check_crc {
        let crc = read_u32(chunk_bytes, crc_offset);
        let mut hasher = Hasher::new();
        hasher.reset();
        hasher.update(&chunk_bytes[4..crc_offset]);

        if crc != hasher.finalize() {
            return Err(DecodeError::IncorrectChunkCrc);
        }
        crc
    } else {
        0
    };

    Ok(Chunk { chunk_type, data: &chunk_bytes[8..crc_offset], start, end, _crc: crc })
}

pub fn parse_png(bytes: &[u8], check_crc: bool) -> Result<ParsedPng, DecodeError> {
    if bytes.len() < PNG_MAGIC_BYTES.len() {
        return Err(DecodeError::MissingBytes);
    }

    if &bytes[0..PNG_MAGIC_BYTES.len()] != PNG_MAGIC_BYTES {
        return Err(DecodeError::InvalidMagicBytes);
    }

    let header_chunk = read_chunk(bytes, PNG_MAGIC_BYTES.len(), check_crc)?;
    let header = PngHeader::from_chunk(&header_chunk)?;

    let pixel_type = PixelType::new(header.color_type, header.bit_depth)?;

    let mut palette = None;
    let mut transparency = None;
    let mut background = None;
    let mut data_start = None;
    let mut data_end = None;

    let mut start = header_chunk.end;

    // TODO we skip CRC for now
    while !bytes.is_empty() {
        let chunk = read_chunk(bytes, start, check_crc)?;

        match chunk.chunk_type {
            ChunkType::ImageData => {
                if data_start.is_none() { data_start = Some(chunk.start); }
                data_end = Some(chunk.end);
            },
            ChunkType::Palette => palette = Some(chunk.data),
            ChunkType::Transparency => transparency = TransparencyChunk::from_chunk(&chunk, pixel_type),
            ChunkType::Background => background = Some(chunk.data),
            ChunkType::ImageEnd => break,
            _ => {},
        }

        start = chunk.end;
    }
    if let (Some(data_start), Some(data_end)) = (data_start, data_end) {
        Ok(ParsedPng {
            header,
            data_chunks: &bytes[data_start..data_end],
            palette,
            pixel_type,
            background,
            transparency,
        })
    } else {
        Err(DecodeError::MissingBytes)
    }
}
