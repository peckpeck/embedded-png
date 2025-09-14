use core::convert::{TryFrom, TryInto};
use crc32fast::Hasher;
use crate::error::DecodeError;
use crate::inflate::{ChunkDecompressor, ScanlineData};
use crate::read_u32;
use crate::types::*;

const PNG_MAGIC_BYTES: &[u8] = &[137, 80, 78, 71, 13, 10, 26, 10];

pub struct ParsedPng<'src> {
    pub header: PngHeader,
    pub pixel_type: PixelType,
    pub palette: Option<&'src [u8]>,
    pub transparency: Option<TransparencyChunk<'src>>,
    pub background: Option<&'src [u8]>,
    pub crc_checked: bool,
    // no need to store all data chunks separately, they HAVE TO be consecutive
    data_chunks: &'src [u8],
}

impl<'src> ParsedPng<'src> {
    pub fn from_bytes(bytes: &'src [u8], check_crc: bool) -> Result<ParsedPng, DecodeError> {
        if bytes.len() < PNG_MAGIC_BYTES.len() {
            return Err(DecodeError::MissingBytes);
        }

        if &bytes[0..PNG_MAGIC_BYTES.len()] != PNG_MAGIC_BYTES {
            return Err(DecodeError::InvalidMagicBytes);
        }

        let header_chunk = Chunk::from_bytes(bytes, PNG_MAGIC_BYTES.len(), check_crc)?;
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
            let chunk = Chunk::from_bytes(bytes, start, check_crc)?;

            match chunk.chunk_type {
                ChunkType::ImageData => {
                    if data_start.is_none() { data_start = Some(chunk.start); }
                    data_end = Some(chunk.end);
                },
                ChunkType::Palette => palette = Some(chunk.data),
                ChunkType::Transparency => transparency = TransparencyChunk::from_data(&chunk.data, pixel_type),
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
                crc_checked: check_crc,
            })
        } else {
            Err(DecodeError::MissingBytes)
        }
    }
}


#[derive(Debug, Clone)]
pub struct PngHeader {
    pub width: usize,
    pub height: usize,
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

        let width = read_u32(chunk.data, 0) as usize;
        let height = read_u32(chunk.data, 4) as usize;
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

    fn pass_info(&self, pass: usize) -> (usize, usize) {
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

    fn bytes_per_scanline_max(&self) -> Result<usize, DecodeError> {
        let bps = self.bytes_per_pixel() as u64 * self.width as u64;
        bps.try_into().map_err(|_| DecodeError::IntegerOverflow)
    }

    fn total_scanline_bytes(&self) -> Result<usize, DecodeError> {
        let pixel_count = self.bytes_per_pixel() as u64 * self.width as u64 * self.height as u64;
        let filter_count = match self.interlace_method {
            InterlaceMethod::None => self.height as u64,
            InterlaceMethod::Adam7 => (1..=7).fold(0_u64, |a,x| a + self.pass_info(x).1 as u64 ),
        };
        (pixel_count+filter_count).try_into().map_err(|_| DecodeError::IntegerOverflow)
    }

}


// TODO review pubs
#[derive(Debug)]
pub struct Chunk<'src> {
    pub chunk_type: ChunkType,
    pub data: &'src [u8],
    pub start: usize,
    pub end: usize,
    pub _crc: u32,
}

impl<'src> Chunk<'src> {
    pub fn from_bytes(bytes: &'src [u8], start: usize, check_crc: bool) -> Result<Self, DecodeError> {
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
}

pub struct DecodedScanline<'buf> {
    scanline_data: &'buf [u8],
    pass: Option<usize>,
    y: usize,
}

impl<'buf> DecodedScanline<'buf> {
    pub fn new_linear(scanline_data: &'buf [u8], y: usize) -> Self {
        DecodedScanline { scanline_data, pass: None, y }
    }
    pub fn new_interlaced(scanline_data: &'buf [u8], y: usize, pass: usize) -> Self {
        DecodedScanline { scanline_data, pass: Some(pass), y }
    }
    pub fn get_xy(&self, x: usize) -> (usize, usize) {
        match self.pass {
            None => (x, self.y),
            Some(1) => (x * 8, self.y * 8),
            Some(2) => (x * 8 + 4, self.y * 8),
            Some(3) => (x * 4, self.y * 8 + 4),
            Some(4) => (x * 4 + 2, self.y * 4),
            Some(5) => (x * 2, self.y * 4 + 2),
            Some(6) => (x * 2 + 1, self.y * 2),
            Some(7) => (x, self.y * 2 + 1),
            _ => (0, 0),
        }
    }
    pub fn get_pixel(&self, x: usize) -> bool {
        self.pass.is_none()
    }

}

pub struct PngReader<'src, 'buf> {
    header: PngHeader,
    decompressor: ChunkDecompressor<'src, 'buf>,
    pixel_type: PixelType,
    palette: Option<&'src [u8]>,
    transparency: Option<TransparencyChunk<'src>>,
    background: Option<&'src [u8]>,
    last_scanline: &'buf mut [u8],
    next_pass: usize, // only for interlace method
    next_y: usize,
}

impl<'src, 'buf> PngReader<'src, 'buf> {
    // we assume that last_scanline is zero initialized
    pub fn from_parsed_png(parsed_png: ParsedPng<'src>, buffer: &'buf mut [u8],
                           buffer_extra: &'buf mut [u8], last_scanline: &'buf mut [u8]) -> Result<Self, DecodeError> {
        let ParsedPng { header, pixel_type,
            palette, transparency,
            background, crc_checked, data_chunks } = parsed_png;

        let decompressor = ChunkDecompressor::new(
            data_chunks, buffer, buffer_extra, crc_checked
        );
        Ok(PngReader {
            header,
            decompressor,
            pixel_type,
            palette,
            transparency,
            background,
            last_scanline,
            next_y: 0,
            next_pass: 1,
        })
    }

    // TODO: understand why we need 'a here
    // self.next_y and self.next_pass must be kept before calling this, they are invalid just after (todo ?)
    pub fn next_scanline<'a: 'buf>(&'a mut self) -> Result<DecodedScanline<'buf>, DecodeError> {
        let bytes_per_pixel = self.header.bytes_per_pixel();
        match self.header.interlace_method {
            InterlaceMethod::None => {
                let bytes_per_scanline = self.header.bytes_per_scanline_max()?; // TODO remove error case
                if let Some(scanline) = self.decompressor.get_scanline(bytes_per_scanline)? {
                    let y = self.next_y;
                    let pixels_per_scanline = bytes_per_scanline / bytes_per_pixel;
                    self.next_y += 1;
                    // store defiltered data into last_scanline
                    scanline.decode(self.last_scanline, pixels_per_scanline, bytes_per_pixel);
                    Ok(DecodedScanline::new_linear(self.last_scanline, y))
                } else {
                    Err(DecodeError::MissingBytes) // TODO Missing scanline err
                }
            },
            InterlaceMethod::Adam7 => {
                // Adam7 Interlacing Pattern
                // 1 6 4 6 2 6 4 6
                // 7 7 7 7 7 7 7 7
                // 5 6 5 6 5 6 5 6
                // 7 7 7 7 7 7 7 7
                // 3 6 4 6 3 6 4 6
                // 7 7 7 7 7 7 7 7
                // 5 6 5 6 5 6 5 6
                // 7 7 7 7 7 7 7 7
                let pass = self.next_pass;
                let (pass_width, pass_height) = self.header.pass_info(pass);
                let bytes_per_scanline = pass_width as usize * bytes_per_pixel;
                if let Some(scanline) = self.decompressor.get_scanline(bytes_per_scanline)? {
                    let y = self.next_y;
                    let pixels_per_scanline = bytes_per_scanline / bytes_per_pixel;

                    self.next_y += 1;
                    if self.next_y > pass_height {
                        loop {
                            self.next_pass += 1;
                            if self.next_pass == 8 {
                                self.next_pass = 1;
                            }
                            // Skip empty passes.
                            let (pass_width, pass_height) = self.header.pass_info(self.next_pass);
                            if pass_width != 0 && pass_height != 0 {
                                // TODO reset last scanline on new passes
                                break;
                            }
                        };
                    }

                    scanline.decode(self.last_scanline, pixels_per_scanline, bytes_per_pixel);
                    Ok(DecodedScanline::new_interlaced(self.last_scanline, y, pass))
                } else {
                    Err(DecodeError::MissingBytes) // TODO Missing scanline err
                }
            },
        }
    }
}
