use core::convert::{TryFrom, TryInto};
use core::cmp::min;
use std::io::ErrorKind::NetworkDown;
use crc32fast::Hasher;
use log::{error, info};
use png_decoder::Scanline2;
use crate::error::DecodeError;
use crate::inflate::{ChunkDecompressor, ScanlineData};
use crate::read_u32;
use crate::types::*;

const PNG_MAGIC_BYTES: &[u8] = &[137, 80, 78, 71, 13, 10, 26, 10];

pub struct ParsedPng<'a> {
    pub header: PngHeader,
    pub pixel_type: PixelType,
    pub palette: Option<&'a [u8]>,
    pub transparency: Option<TransparencyChunk<'a>>,
    pub background: Option<&'a [u8]>,
    pub crc_checked: bool,
    // no need to store all data chunks separately, they HAVE TO be consecutive
    data_chunks: &'a[u8],
}

impl<'a> ParsedPng<'a> {
    pub fn from_bytes(bytes: &'a [u8], check_crc: bool) -> Result<ParsedPng, DecodeError> {
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
pub struct Chunk<'a> {
    pub chunk_type: ChunkType,
    pub data: &'a [u8],
    pub start: usize,
    pub end: usize,
    pub _crc: u32,
}

impl<'a> Chunk<'a> {
    pub fn from_bytes(bytes: &'a [u8], start: usize, check_crc: bool) -> Result<Self, DecodeError> {
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

fn defilter(filter_type: FilterType, top_left: u8, top: u8, left: u8, current: u8) -> u8 {
    match filter_type {
        FilterType::None => current,
        FilterType::Sub => current.wrapping_add(left),
        FilterType::Up => current.wrapping_add(top),
        FilterType::Average => {
            // we can either work wit u16 or with u8 and a carry
            // let's choose u16
            let average = (left as u16 + top as u16) / 2;
            current.wrapping_add(average as u8)
        },
        FilterType::Paeth => {
            let a = left as i16;
            let b = top as i16;
            let c = top_left as i16;
            let p = a + b - c;      // initial estimate
            let pa = (p - a).abs(); // distances to a, b, c
            let pb = (p - b).abs();
            let pc = (p - c).abs();
            // return nearest of a,b,c,
            // breaking ties in order a,b,c.
            let predictor = if pa <= pb && pa <= pc {
                left
            } else if pb <= pc {
                top
            } else {
                top_left
            };
            current.wrapping_add(predictor)
        },
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
    pub fn next_scanline<'a: 'buf>(&'a mut self) -> Result<&'buf [u8], DecodeError> {
        match self.header.interlace_method {
            InterlaceMethod::None => {
                let bytes_per_pixel = self.header.bytes_per_pixel();
                let bytes_per_scanline = self.header.bytes_per_scanline_max()?; // TODO remove error case
                let pixels_per_scanline = bytes_per_scanline / bytes_per_pixel;
                if let Some(scanline) = self.decompressor.get_scanline(bytes_per_scanline)? {
                    let filter_type = scanline.filter_type;
                    // store defiltered data into last_scanline
                    // since it already contains last scanline data,
                    // we need to keep 3 pixels elsewhere for defiltering
                    // and write them later (left, top, top-left)

                    // rust array must be allocated at compile time, so we use the max bpp value
                    let mut left = [0_u8; 8];
                    let mut top_left = [0_u8; 8];
                    let mut top = [0_u8; 8];

                    // defilter is applied on bytes, but for byes of the same pixel
                    // so we do a double iteration
                    for i in 0..pixels_per_scanline {
                        let pos = i * bytes_per_pixel;
                        for j in 0..bytes_per_pixel {
                            top[j] = self.last_scanline[pos+j];
                            let current = scanline.get(pos+j);
                            let defiltered = defilter(filter_type, top_left[j], top[j], left[j], current);
                            self.last_scanline[pos+j] = defiltered;
                            top_left[j] = top[j];
                            left[j] = defiltered;
                        }
                    }
                    self.next_y += 1;
                    Ok(self.last_scanline)
                } else {
                    Err(DecodeError::MissingBytes) // TODO Missing scanline err
                }
            },
            InterlaceMethod::Adam7 => {
                let bytes_per_pixel = self.header.bytes_per_pixel();
                let (pass_width, pass_height) = loop {
                    let (pass_width, pass_height) = self.header.pass_info(self.next_pass);
                    self.next_pass += 1;
                    if self.next_pass == 8 {
                        self.next_pass = 1;
                    }
                    // Skip empty passes.
                    if pass_width != 0 && pass_height != 0 {
                        break (pass_width, pass_height);
                    }
                };
                let bytes_per_scanline = pass_width as usize * bytes_per_pixel;
                let pixels_per_scanline = bytes_per_scanline / bytes_per_pixel;

/*                for y in 0..pass_height {
                    let filter_type = FilterType::try_from(self.scanline_data[cursor])
                        .map_err(|_| DecodeError::InvalidFilterType)?;
                    cursor += 1;

                    let current_scanline =
                        &mut self.scanline_data[cursor..(cursor + bytes_per_scanline)];

                    for x in 0..(bytes_per_scanline) {
                        let unfiltered_byte = defilter(
                            filter_type,
                            bytes_per_pixel,
                            x,
                            current_scanline,
                            last_scanline,
                        );
                        current_scanline[x] = unfiltered_byte;
                    }
  */              todo!()
                /*
                                // Adam7 Interlacing Pattern
                                // 1 6 4 6 2 6 4 6
                                // 7 7 7 7 7 7 7 7
                                // 5 6 5 6 5 6 5 6
                                // 7 7 7 7 7 7 7 7
                                // 3 6 4 6 3 6 4 6
                                // 7 7 7 7 7 7 7 7
                                // 5 6 5 6 5 6 5 6
                                // 7 7 7 7 7 7 7 7

                                for pass in 1..=7 {
                                    let (pass_width, pass_height) = header.pass_info(pass);


                                    let bytes_per_scanline = pass_width as usize * bytes_per_pixel;
                                    let last_scanline = &mut last_scanline[..(bytes_per_scanline)];
                                    for byte in last_scanline.iter_mut() {
                                        *byte = 0;
                                    }

                                    for y in 0..pass_height {
                                        let filter_type = FilterType::try_from(self.scanline_data[cursor])
                                            .map_err(|_| DecodeError::InvalidFilterType)?;
                                        cursor += 1;

                                        let current_scanline =
                                            &mut self.scanline_data[cursor..(cursor + bytes_per_scanline)];

                                        for x in 0..(bytes_per_scanline) {
                                            let unfiltered_byte = defilter(
                                                filter_type,
                                                bytes_per_pixel,
                                                x,
                                                current_scanline,
                                                last_scanline,
                                            );
                                            current_scanline[x] = unfiltered_byte;
                                        }

                                        let scanline_iter = ScanlineIterator::new(
                                            pass_width,
                                            self.pixel_type,
                                            current_scanline,
                                            & self.ancillary_chunks,
                                        );

                                        let xy_calculator = XYCalculator::new_interlaced(pass);
                                        scan(scanline_iter, xy_calculator, y as usize);

                                        last_scanline.copy_from_slice(current_scanline);

                                        cursor += bytes_per_scanline;
                                    }
                                }*/
            },
        }
    }
}
