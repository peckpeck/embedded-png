use core::convert::{TryFrom, TryInto};
use std::cmp::min;
use crc32fast::Hasher;
use log::{error, info};
use miniz_oxide::inflate::core::{decompress, DecompressorOxide};
use miniz_oxide::inflate::core::inflate_flags::{TINFL_FLAG_COMPUTE_ADLER32, TINFL_FLAG_HAS_MORE_INPUT, TINFL_FLAG_PARSE_ZLIB_HEADER};
use miniz_oxide::inflate::TINFLStatus;
use png_decoder::Scanline2;
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

/// A single PNG scanline that points to decompressor buffer data
pub struct ScanlineData<'a> {
    // each scanline has its filter pe
    filter_type: FilterType,
    // a scanline spans at most 3 buffers because the decompressor uses a Q buffer
    buffer1: &'a mut [u8],
    buffer2: &'a mut [u8],
    buffer3: &'a mut [u8],
    // optimization to avoid computing position all the time
    buffer3_pos: usize,
    // used for bound checking only, TODO maybe remove
    total_size: usize,
}

impl<'a> ScanlineData<'a> {
    // create a scanline from the Q buffer
    // size is the size of the scanline,
    // with the filer type, size+1 bytes are taken from the buffer
    // returns the size of the consumed data from main buffer
    fn new(extra_buffer: &'a mut [u8], main_buffer: &'a mut [u8], main_pos: usize, size: usize) -> Result<(usize, Self), DecodeError> {
        // data is taken from extra_buffer them from main_buffer
        // - extra_buffer if fully consumed
        // - remaining bytes are taken from main buffer
        // - in potentially 2 steps if it spans past the end of the allocated memory
        if extra_buffer.len() >= size +1 { error!("Extra buffer too big {}", extra_buffer.len()); } // debug_assert
        // extract filter type
        let (type_byte, extra_start) = if extra_buffer.len() > 0 {
            (extra_buffer[0], 1)
        } else {
            (main_buffer[main_pos], 0)
        };
        let filter_type = FilterType::try_from(type_byte)
            .map_err(|_| DecodeError::InvalidFilterType)?;
        // consume extra_buffer into buffer1
        let size_rest = size - (extra_buffer.len() - extra_start);
        let buffer1 = &mut extra_buffer[extra_start..];
        // first part of main_buffer into buffer2
        let buffer2_start = main_pos + (1-extra_start);
        let buffer2_end = min(size_rest+buffer2_start, main_buffer.len());
        let size_rest = size_rest - (buffer2_end - buffer2_start);
        // since we take 2 mut pointers from main_buffer, we have to split_at_mut
        if buffer2_start < size_rest { error!("size rest too big"); return Err(DecodeError::InvalidChunk); } // debug_assert
        let (b1, b2) = main_buffer.split_at_mut(buffer2_start);
        let buffer2 = &mut b2[..buffer2_end-buffer2_start];
        // finally remaining data into buffer3
        let buffer3 = &mut b1[..size_rest];
        let buffer3_pos = buffer1.len() + buffer2.len();
        // also return consumed data for the caller to update its index
        let consumed = buffer2.len() + buffer3.len() + (1-extra_start);
        Ok((consumed, ScanlineData {
            filter_type,
            buffer1,
            buffer2,
            buffer3,
            buffer3_pos,
            total_size: size,
        }))
    }

    // we ignore scanline size for now, this can produce invalid data if it goes over scanline size
    // set a single byte of the uffer (this should be forbidden and will be removed)
    pub fn set(&mut self, index: usize, value: u8) {
        if index >= self.total_size { info!("set error {} >= {}", index, self.total_size); } // debug_assert
        if index < self.buffer1.len() {
            self.buffer1[index] = value;
        } else if index < self.buffer3_pos {
            self.buffer2[index-self.buffer1.len()] = value;
        } else {
            self.buffer3[index-self.buffer3_pos] = value;
        }
    }

    // get a single byte of the buffer
    // TODO get multi bytes
    pub fn get(&self, index: usize) -> u8 {
        if index >= self.total_size { info!("get error {} >= {}", index, self.total_size); }
        if index < self.buffer1.len() {
            self.buffer1[index]
        } else if index < self.buffer3_pos {
            self.buffer2[index-self.buffer1.len()]
        } else {
            self.buffer3[index-self.buffer3_pos]
        }
    }

    // copy the data part to a dedicated buffer (this should not be needed in the future)
    pub fn copy_to_slice(&self, slice: &mut [u8]) {
        if slice.len() > self.total_size { info!("copy_to_slice error {} >= {}", slice.len(), self.total_size); }
        slice[..self.buffer1.len()].copy_from_slice(self.buffer1);
        slice[self.buffer1.len()..self.buffer3_pos].copy_from_slice(self.buffer2);
        slice[self.buffer3_pos..].copy_from_slice(self.buffer3);
    }
}

/// The decompressor is implemented as a Q buffer
/// Q because it is a circular buffer + a linear buffer, which if you draw them, looks like a Q
/// Data is always decompressed in the circular buffer, but since the whole circular buffer
/// might be needed  when extracting new data, we temporarily store remaining data
/// in the linear buffer.
/// All of this would not be needed if we could control the maximum data that must be decompressed
struct ChunkDecompressor<'src, 'buf> {
    // internal miniz decompressor data
    decompressor: DecompressorOxide,
    // slice containing ImageData chunks
    data_chunks: &'src [u8],
    // position of the next chunk in this slice
    next_chunk_start: Option<usize>,
    // slice of the current data chunk
    current_chunk: Option<&'src [u8]>,
    // true when all data have been taken from chunk
    chunk_end: bool,
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
    // common flags for decompression
    flags: u32,
}

impl<'src, 'buf> ChunkDecompressor<'src, 'buf> {
    // buffer size must be >= min(32k, total_output size)
    // buffer extra size must be >= max scanline bytes
    fn new(data_chunks: &'src [u8], buffer: &'buf mut [u8], buffer_extra: &'buf mut [u8], check_crc: bool) -> Self {
        let decompressor = DecompressorOxide::new();
        // png has zlib header, always pass has more input, it doesn't matter it it's wrong
        let mut flags = TINFL_FLAG_PARSE_ZLIB_HEADER | TINFL_FLAG_HAS_MORE_INPUT;
        if check_crc {
            flags |= TINFL_FLAG_COMPUTE_ADLER32;
        }
        ChunkDecompressor {
            decompressor,
            data_chunks,
            next_chunk_start: Some(0),
            current_chunk: None,
            chunk_end: false,
            buffer,
            buffer_pos: 0,
            buffer_count: 0,
            buffer_extra,
            extra_count: 0,
            flags,
        }
    }

    // check if end of input has been reached and update flag accordingly
    fn check_for_end(&mut self) {
        // we have some data for sure
        if let Some(chunk) = self.current_chunk && !chunk.is_empty() {
            return;
        }
        if self.next_chunk_start.is_none() {
            // we truly reached the end
            self.chunk_end = true;
        }
    }

    // advance current chunk by one, result in self.current_chunk
    fn check_chunk_data(&mut self) {
        // we already have some data
        if let Some(chunk) = self.current_chunk && !chunk.is_empty() {
            return;
        }
        // loop just in case there are empty chunks
        loop {
            if let Some(next_start) = self.next_chunk_start && next_start < self.data_chunks.len() {
                // it was already checked during first parse, so we can unwrap, and avoid crc check
                let next_chunk = read_chunk(self.data_chunks, next_start, false).unwrap();
                self.next_chunk_start = Some(next_chunk.end);
                if next_chunk.chunk_type == ChunkType::ImageData {
                    if !next_chunk.data.is_empty() {
                        self.current_chunk = Some(next_chunk.data);
                        return;
                    }
                }
            } else {
                // this is the end my friend
                self.current_chunk = None;
                self.next_chunk_start = None;
                self.chunk_end = true;
                return;
            }
        }
    }

    // produce a single scanline of given size from the buffer, assuming it has enough data
    fn scanline(&mut self, size: usize) -> Result<ScanlineData, DecodeError> {
        let len =  self.buffer.len();
        let start_pos = if self.buffer_count > self.buffer_pos {
            self.data_chunks.len() - self.buffer_count + self.buffer_pos
        } else {
            self.buffer_pos - self.buffer_count
        };
        let (consumed, scanline) =
            ScanlineData::new(&mut self.buffer_extra[..self.extra_count],
                              &mut self.buffer,
                              start_pos, size)?;
        self.extra_count = 0;
        self.buffer_count -= consumed;
        Ok(scanline)
    }

    // get the next scanline, extracting data with the decompressor if needed
    pub fn get_scanline(&mut self, size: usize) -> Result<Option<ScanlineData>, DecodeError> {
        // we already have enough data
        if self.buffer_count + self.extra_count >= size + 1 {
            return Ok(Some(self.scanline(size)?));
        }

        // now we need to decompress some bytes

        // since decompress() does not cross the buffer wrap but can grow through the end
        // we must save any data that is after buffer_pos
        if self.buffer_count > self.buffer_pos {
            let data_count = self.buffer_count - self.buffer_pos;
            let data_start = self.buffer.len() - data_count;
            // there is no overflow since buffer_extra has enough data for a single scanline
            let new_extra_count = self.extra_count + data_count;
            self.buffer_extra[self.extra_count..new_extra_count].copy_from_slice(&self.buffer[data_start..self.buffer.len()]);
            self.extra_count = new_extra_count;
            // we have removed everything tha wrapped before 0
            self.buffer_count = self.buffer_pos;
        }

        // get some bytes to uncompress
        self.check_chunk_data();
        let next_data = match self.current_chunk {
            None => &[], // continue, we might have more output pending
            Some(x) => x,
        };

        // run decompress
        let (status, in_count, out_count) =
            decompress(&mut self.decompressor,
                       next_data,
                       &mut self.buffer,
                       self.buffer_pos,
                       self.flags
            );

        // account for byte read
        self.current_chunk.as_mut().map(|chunk| &chunk[in_count..]);
        self.check_for_end();

        // account for bytes written
        self.buffer_count += out_count;
        self.buffer_pos += out_count;
        if self.buffer_pos > self.buffer.len() { info!("decompress wrapped around"); } // debbug_assert
        if self.buffer_pos == self.buffer.len() {
            self.buffer_pos = 0;
        }

        // account for errors
        if (status as i32) < 0 {
            return Err(DecodeError::Decompress(status));
        }
        match status {
            TINFLStatus::Done => if !self.chunk_end {
                return Err(DecodeError::InvalidChunk);
            }
            TINFLStatus::NeedsMoreInput => if self.chunk_end {
                return Err(DecodeError::InvalidChunk);
            }
            // TINFLStatus::HasMoreOutput is handled gracefully by decompress on next run
            _ => {}
        }

        // rerun to not duplicate logic
        self.get_scanline(size)
    }
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
