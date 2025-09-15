use core::convert::{TryFrom, TryInto};
use crc32fast::Hasher;
use embedded_graphics_core::draw_target::DrawTarget;
use embedded_graphics_core::pixelcolor::{Argb8888, PixelColor, Rgb888};
use embedded_graphics_core::prelude::{Point, Size};
use embedded_graphics_core::primitives::Rectangle;
use log::info;
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

//    fn linear_draw<T: DrawTarget>(&self, decompressor: &mut ChunkDecompressor, scanline_buf: &mut [u8], target: T) -> Result<(), DecodeError> {
    pub fn linear_draw<T: DrawTarget<Color=Argb8888>>(&self,
                                    buffer: &mut [u8], buffer_extra: &mut [u8],
                                  scanline_buf: &mut [u8], target: &mut T) -> Result<(), DecodeError> {
        let mut  decompressor = ChunkDecompressor::new(
            self.data_chunks, buffer, buffer_extra, self.crc_checked
        );
        // assume InterlaceMethod is None
        let bytes_per_pixel = self.header.bytes_per_pixel();
        let bytes_per_scanline = self.header.bytes_per_scanline_max()?; // TODO remove error case
        info!("Pixel type {:?}", self.pixel_type);
        for y in 0..self.header.height {
            decompressor.decode_next_scanline(scanline_buf, bytes_per_pixel)?;
            let mut it = RgbAlpha8Iterator{ scanline: scanline_buf, pos: 0 };
            let line = Rectangle::new(Point::new(0,y as i32), Size::new(1280,1));
            target.fill_contiguous(&line, it).map_err(|_| DecodeError::MissingBytes);
 //           for x in 0..self.header.width {
 //               let (r,g,b,a) =  get_color(self, scanline_buf, x);
 //               let rgb = Rgb888::new(r,g,b);
 //           }
        }
        Ok(())
    }
    //fn interlaced_drawer(&self) -> InterlacedReader {    }
}

struct RgbAlpha8Iterator<'a> {
    scanline: &'a [u8],
    pos: usize,
}

impl <'a> Iterator for RgbAlpha8Iterator<'a> {
    type Item = Argb8888;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.scanline.len() {
            None
        } else {
            let r = self.scanline[self.pos];
            let g = self.scanline[self.pos + 1];
            let b = self.scanline[self.pos + 2];
            let a = self.scanline[self.pos + 3];
            self.pos += 4;

            Some(Argb8888::new(r, g, b, a))
        }
    }
}

pub fn get_color(png: &ParsedPng, scanline: &[u8], x: usize) -> (u8,u8,u8,u8) {
    match png.pixel_type {
        PixelType::Grayscale1 => {
            let byte = scanline[x / 8];
            let bit_offset = 7 - x % 8;
            let grayscale_val = (byte >> bit_offset) & 1;

            let alpha = match png.transparency {
                Some(TransparencyChunk::Grayscale(transparent_val))
                if grayscale_val == transparent_val =>
                    {
                        0
                    },
                _ => 255,
            };

            let pixel_val = grayscale_val * 255;

            (pixel_val, pixel_val, pixel_val, alpha)
        },
        PixelType::Grayscale2 => {
            let byte = scanline[x / 4];
            let bit_offset = 6 - ((x % 4) * 2);
            let grayscale_val = (byte >> bit_offset) & 0b11;

            let alpha = match png.transparency {
                Some(TransparencyChunk::Grayscale(transparent_val))
                if grayscale_val == transparent_val =>
                    {
                        0
                    },
                _ => 255,
            };

            // TODO - use a lookup table
            let pixel_val = ((grayscale_val as f32 / 3.0) * 255.0) as u8;

            (pixel_val, pixel_val, pixel_val, alpha)
        },
        PixelType::Grayscale4 => {
            let byte = scanline[x / 2];
            let bit_offset = 4 - ((x % 2) * 4);
            let grayscale_val = (byte >> bit_offset) & 0b1111;

            let alpha = match png.transparency {
                Some(TransparencyChunk::Grayscale(transparent_val))
                if grayscale_val == transparent_val =>
                    {
                        0
                    },
                _ => 255,
            };

            // TODO - use a lookup table
            let pixel_val = ((grayscale_val as f32 / 15.0) * 255.0) as u8;
            (pixel_val, pixel_val, pixel_val, alpha)
        },
        PixelType::Grayscale8 => {
            let byte = scanline[x];

            let alpha = match png.transparency {
                Some(TransparencyChunk::Grayscale(transparent_val))
                if byte == transparent_val =>
                    {
                        0
                    },
                _ => 255,
            };
            (byte, byte, byte, alpha)
        },
        PixelType::Grayscale16 => {
            let offset = x * 2;
            let grayscale_val =
                u16::from_be_bytes([scanline[offset], scanline[offset + 1]]);

            let pixel_val = u16_to_u8(grayscale_val);

            // TODO(bschwind) - This may need to be compared to the original
            //                  16-bit transparency value, instead of the transformed
            //                  8-bit value.
            let alpha = match png.transparency {
                Some(TransparencyChunk::Grayscale(transparent_val))
                if pixel_val == transparent_val =>
                    {
                        0
                    },
                _ => 255,
            };

            (pixel_val, pixel_val, pixel_val, alpha)
        },
        PixelType::Rgb8 => {
            let offset = x * 3;
            let r = scanline[offset];
            let g = scanline[offset + 1];
            let b = scanline[offset + 2];

            let alpha = match png.transparency {
                Some(TransparencyChunk::Rgb(t_r, t_g, t_b))
                if r == t_r && g == t_g && b == t_b =>
                    {
                        0
                    },
                _ => 255,
            };

            (r, g, b, alpha)
        },
        PixelType::Rgb16 => {
            let offset = x * 6;
            let r = u16::from_be_bytes([scanline[offset], scanline[offset + 1]]);
            let g = u16::from_be_bytes([scanline[offset + 2], scanline[offset + 3]]);
            let b = u16::from_be_bytes([scanline[offset + 4], scanline[offset + 5]]);

            let r = u16_to_u8(r);
            let g = u16_to_u8(g);
            let b = u16_to_u8(b);

            let alpha = match png.transparency {
                Some(TransparencyChunk::Rgb(t_r, t_g, t_b))
                if r == t_r && g == t_g && b == t_b =>
                    {
                        0
                    },
                _ => 255,
            };

            (r, g, b, alpha)
        },
        PixelType::Palette1 => {
            let byte = scanline[x / 8];
            let bit_offset = 7 - x % 8;
            let palette_idx = ((byte >> bit_offset) & 1) as usize;

            let offset = palette_idx * 3;

            let palette = png.palette.unwrap();
            let r = palette[offset];
            let g = palette[offset + 1];
            let b = palette[offset + 2];

            let alpha: u8 = match png.transparency {
                Some(TransparencyChunk::Palette(data)) => {
                    *data.get(palette_idx).unwrap_or(&255)
                },
                Some(_) | None => 255,
            };

            (r, g, b, alpha)
        },
        PixelType::Palette2 => {
            let byte = scanline[x / 4];
            let bit_offset = 6 - ((x % 4) * 2);
            let palette_idx = ((byte >> bit_offset) & 0b11) as usize;

            let offset = palette_idx * 3;

            let palette = png.palette.unwrap();
            let r = palette[offset];
            let g = palette[offset + 1];
            let b = palette[offset + 2];

            let alpha: u8 = match png.transparency {
                Some(TransparencyChunk::Palette(data)) => {
                    *data.get(palette_idx).unwrap_or(&255)
                },
                Some(_) | None => 255,
            };

            (r, g, b, alpha)
        },
        PixelType::Palette4 => {
            let byte = scanline[x / 2];
            let bit_offset = 4 - ((x % 2) * 4);
            let palette_idx = ((byte >> bit_offset) & 0b1111) as usize;

            let offset = palette_idx * 3;

            let palette = png.palette.unwrap();
            let r = palette[offset];
            let g = palette[offset + 1];
            let b = palette[offset + 2];

            let alpha: u8 = match png.transparency {
                Some(TransparencyChunk::Palette(data)) => {
                    *data.get(palette_idx).unwrap_or(&255)
                },
                Some(_) | None => 255,
            };

            (r, g, b, alpha)
        },
        PixelType::Palette8 => {
            let offset = scanline[x] as usize * 3;

            let palette = png.palette.unwrap();
            let r = palette[offset];
            let g = palette[offset + 1];
            let b = palette[offset + 2];

            let alpha: u8 = match png.transparency {
                Some(TransparencyChunk::Palette(data)) => *data.get(offset).unwrap_or(&255),
                Some(_) | None => 255,
            };

            (r, g, b, alpha)
        },
        PixelType::GrayscaleAlpha8 => {
            let offset = x * 2;
            let grayscale_val = scanline[offset];
            let alpha = scanline[offset + 1];

            (grayscale_val, grayscale_val, grayscale_val, alpha)
        },
        PixelType::GrayscaleAlpha16 => {
            let offset = x * 4;
            let grayscale_val =
                u16::from_be_bytes([scanline[offset], scanline[offset + 1]]);
            let alpha =
                u16::from_be_bytes([scanline[offset + 2], scanline[offset + 3]]);

            let grayscale_val = u16_to_u8(grayscale_val);
            let alpha = u16_to_u8(alpha);

            (grayscale_val, grayscale_val, grayscale_val, alpha)
        },
        PixelType::RgbAlpha8 => {
            let offset = x * 4;
            let r = scanline[offset];
            let g = scanline[offset + 1];
            let b = scanline[offset + 2];
            let a = scanline[offset + 3];

            (r, g, b, a)
        },
        PixelType::RgbAlpha16 => {
            let offset = x * 8;
            let r = u16::from_be_bytes([scanline[offset], scanline[offset + 1]]);
            let g = u16::from_be_bytes([scanline[offset + 2], scanline[offset + 3]]);
            let b = u16::from_be_bytes([scanline[offset + 4], scanline[offset + 5]]);
            let a = u16::from_be_bytes([scanline[offset + 6], scanline[offset + 7]]);

            let r = u16_to_u8(r);
            let g = u16_to_u8(g);
            let b = u16_to_u8(b);
            let a = u16_to_u8(a);

            (r, g, b, a)
        },
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



// TODO remove
#[inline(always)]
fn u16_to_u8(val: u16) -> u8 {
    (val >> 8) as u8
}

pub struct DecodedScanline<'a, 'src, 'buf> {
    png: &'a ParsedPng<'src>,
    scanline_data: &'buf [u8],
    pass: Option<usize>,
    y: usize,
}

impl<'a, 'src, 'buf> DecodedScanline<'a, 'src, 'buf> {
    pub fn new_linear(png: &'a ParsedPng<'src>, scanline_data: &'buf [u8], y: usize) -> Self {
        DecodedScanline { png, scanline_data, pass: None, y }
    }
    pub fn new_interlaced(png: &'a ParsedPng<'src>, scanline_data: &'buf [u8], y: usize, pass: usize) -> Self {
        DecodedScanline { png, scanline_data, pass: Some(pass), y }
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
    pub fn get_color(&self, x: usize) -> Option<(u8,u8,u8,u8)> {
        match self.png.pixel_type {
            PixelType::Grayscale1 => {
                let byte = self.scanline_data[x / 8];
                let bit_offset = 7 - x % 8;
                let grayscale_val = (byte >> bit_offset) & 1;

                let alpha = match self.png.transparency {
                    Some(TransparencyChunk::Grayscale(transparent_val))
                    if grayscale_val == transparent_val =>
                        {
                            0
                        },
                    _ => 255,
                };

                let pixel_val = grayscale_val * 255;

                Some((pixel_val, pixel_val, pixel_val, alpha))
            },
            PixelType::Grayscale2 => {
                let byte = self.scanline_data[x / 4];
                let bit_offset = 6 - ((x % 4) * 2);
                let grayscale_val = (byte >> bit_offset) & 0b11;

                let alpha = match self.png.transparency {
                    Some(TransparencyChunk::Grayscale(transparent_val))
                    if grayscale_val == transparent_val =>
                        {
                            0
                        },
                    _ => 255,
                };

                // TODO - use a lookup table
                let pixel_val = ((grayscale_val as f32 / 3.0) * 255.0) as u8;

                Some((pixel_val, pixel_val, pixel_val, alpha))
            },
            PixelType::Grayscale4 => {
                let byte = self.scanline_data[x / 2];
                let bit_offset = 4 - ((x % 2) * 4);
                let grayscale_val = (byte >> bit_offset) & 0b1111;

                let alpha = match self.png.transparency {
                    Some(TransparencyChunk::Grayscale(transparent_val))
                    if grayscale_val == transparent_val =>
                        {
                            0
                        },
                    _ => 255,
                };

                // TODO - use a lookup table
                let pixel_val = ((grayscale_val as f32 / 15.0) * 255.0) as u8;
                Some((pixel_val, pixel_val, pixel_val, alpha))
            },
            PixelType::Grayscale8 => {
                let byte = self.scanline_data[x];

                let alpha = match self.png.transparency {
                    Some(TransparencyChunk::Grayscale(transparent_val))
                    if byte == transparent_val =>
                        {
                            0
                        },
                    _ => 255,
                };
                Some((byte, byte, byte, alpha))
            },
            PixelType::Grayscale16 => {
                let offset = x * 2;
                let grayscale_val =
                    u16::from_be_bytes([self.scanline_data[offset], self.scanline_data[offset + 1]]);

                let pixel_val = u16_to_u8(grayscale_val);

                // TODO(bschwind) - This may need to be compared to the original
                //                  16-bit transparency value, instead of the transformed
                //                  8-bit value.
                let alpha = match self.png.transparency {
                    Some(TransparencyChunk::Grayscale(transparent_val))
                    if pixel_val == transparent_val =>
                        {
                            0
                        },
                    _ => 255,
                };

                Some((pixel_val, pixel_val, pixel_val, alpha))
            },
            PixelType::Rgb8 => {
                let offset = x * 3;
                let r = self.scanline_data[offset];
                let g = self.scanline_data[offset + 1];
                let b = self.scanline_data[offset + 2];

                let alpha = match self.png.transparency {
                    Some(TransparencyChunk::Rgb(t_r, t_g, t_b))
                    if r == t_r && g == t_g && b == t_b =>
                        {
                            0
                        },
                    _ => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::Rgb16 => {
                let offset = x * 6;
                let r = u16::from_be_bytes([self.scanline_data[offset], self.scanline_data[offset + 1]]);
                let g = u16::from_be_bytes([self.scanline_data[offset + 2], self.scanline_data[offset + 3]]);
                let b = u16::from_be_bytes([self.scanline_data[offset + 4], self.scanline_data[offset + 5]]);

                let r = u16_to_u8(r);
                let g = u16_to_u8(g);
                let b = u16_to_u8(b);

                let alpha = match self.png.transparency {
                    Some(TransparencyChunk::Rgb(t_r, t_g, t_b))
                    if r == t_r && g == t_g && b == t_b =>
                        {
                            0
                        },
                    _ => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::Palette1 => {
                let byte = self.scanline_data[x / 8];
                let bit_offset = 7 - x % 8;
                let palette_idx = ((byte >> bit_offset) & 1) as usize;

                let offset = palette_idx * 3;

                let palette = self.png.palette.unwrap();
                let r = palette[offset];
                let g = palette[offset + 1];
                let b = palette[offset + 2];

                let alpha: u8 = match self.png.transparency {
                    Some(TransparencyChunk::Palette(data)) => {
                        *data.get(palette_idx).unwrap_or(&255)
                    },
                    Some(_) | None => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::Palette2 => {
                let byte = self.scanline_data[x / 4];
                let bit_offset = 6 - ((x % 4) * 2);
                let palette_idx = ((byte >> bit_offset) & 0b11) as usize;

                let offset = palette_idx * 3;

                let palette = self.png.palette.unwrap();
                let r = palette[offset];
                let g = palette[offset + 1];
                let b = palette[offset + 2];

                let alpha: u8 = match self.png.transparency {
                    Some(TransparencyChunk::Palette(data)) => {
                        *data.get(palette_idx).unwrap_or(&255)
                    },
                    Some(_) | None => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::Palette4 => {
                let byte = self.scanline_data[x / 2];
                let bit_offset = 4 - ((x % 2) * 4);
                let palette_idx = ((byte >> bit_offset) & 0b1111) as usize;

                let offset = palette_idx * 3;

                let palette = self.png.palette.unwrap();
                let r = palette[offset];
                let g = palette[offset + 1];
                let b = palette[offset + 2];

                let alpha: u8 = match self.png.transparency {
                    Some(TransparencyChunk::Palette(data)) => {
                        *data.get(palette_idx).unwrap_or(&255)
                    },
                    Some(_) | None => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::Palette8 => {
                let offset = self.scanline_data[x] as usize * 3;

                let palette = self.png.palette.unwrap();
                let r = palette[offset];
                let g = palette[offset + 1];
                let b = palette[offset + 2];

                let alpha: u8 = match self.png.transparency {
                    Some(TransparencyChunk::Palette(data)) => *data.get(offset).unwrap_or(&255),
                    Some(_) | None => 255,
                };

                Some((r, g, b, alpha))
            },
            PixelType::GrayscaleAlpha8 => {
                let offset = x * 2;
                let grayscale_val = self.scanline_data[offset];
                let alpha = self.scanline_data[offset + 1];

                Some((grayscale_val, grayscale_val, grayscale_val, alpha))
            },
            PixelType::GrayscaleAlpha16 => {
                let offset = x * 4;
                let grayscale_val =
                    u16::from_be_bytes([self.scanline_data[offset], self.scanline_data[offset + 1]]);
                let alpha =
                    u16::from_be_bytes([self.scanline_data[offset + 2], self.scanline_data[offset + 3]]);

                let grayscale_val = u16_to_u8(grayscale_val);
                let alpha = u16_to_u8(alpha);

                Some((grayscale_val, grayscale_val, grayscale_val, alpha))
            },
            PixelType::RgbAlpha8 => {
                let offset = x * 4;
                let r = self.scanline_data[offset];
                let g = self.scanline_data[offset + 1];
                let b = self.scanline_data[offset + 2];
                let a = self.scanline_data[offset + 3];

                Some((r, g, b, a))
            },
            PixelType::RgbAlpha16 => {
                let offset = x * 8;
                let r = u16::from_be_bytes([self.scanline_data[offset], self.scanline_data[offset + 1]]);
                let g = u16::from_be_bytes([self.scanline_data[offset + 2], self.scanline_data[offset + 3]]);
                let b = u16::from_be_bytes([self.scanline_data[offset + 4], self.scanline_data[offset + 5]]);
                let a = u16::from_be_bytes([self.scanline_data[offset + 6], self.scanline_data[offset + 7]]);

                let r = u16_to_u8(r);
                let g = u16_to_u8(g);
                let b = u16_to_u8(b);
                let a = u16_to_u8(a);

                Some((r, g, b, a))
            },
        }
    }

}

pub struct PngReader<'a, 'src, 'buf> {
    png: &'a ParsedPng<'src>,
    decompressor: ChunkDecompressor<'src, 'buf>,
    last_scanline: &'buf mut [u8],
    next_pass: usize, // only for interlace method
    next_y: usize,
/*    header: PngHeader,
    pixel_type: PixelType,
    palette: Option<&'src [u8]>,
    transparency: Option<TransparencyChunk<'src>>,
    background: Option<&'src [u8]>, // TODO
    last_scanline: &'buf mut [u8],
    next_pass: usize, // only for interlace method
    next_y: usize,*/
}

impl<'a, 'src, 'buf> PngReader<'a, 'src, 'buf> {
    // we assume that last_scanline is zero initialized
    pub fn from_parsed_png(parsed_png: &'a ParsedPng<'src>, buffer: &'buf mut [u8],
                           buffer_extra: &'buf mut [u8], last_scanline: &'buf mut [u8]) -> Result<Self, DecodeError> {
        let ParsedPng { header, pixel_type,
            palette, transparency,
            background, crc_checked, data_chunks } = parsed_png;

        let decompressor = ChunkDecompressor::new(
            data_chunks, buffer, buffer_extra, parsed_png.crc_checked
        );
        Ok(PngReader {
            /*            header,
                        decompressor,
                        pixel_type,
                        palette,
                        transparency,
                        background,
                        last_scanline,
                        next_y: 0,
                        next_pass: 1,
            */
            png: parsed_png,
            decompressor,
            last_scanline,
            next_pass: 0,
            next_y: 1,
        })
    }

    /*
    fn get_scanline<'b: 'buf>(&'b mut self, bytes_per_scanline: usize, bytes_per_pixel: usize) -> Result<(), DecodeError>  {
        if let Some(scanline) = self.decompressor.get_scanline(bytes_per_scanline+1)? {
            log::info!("None2");
            self.next_y += 1;
            // store defiltered data into last_scanline
            scanline.decode(self.last_scanline, bytes_per_pixel);
            Ok(())
        } else {
            log::info!("None ERR");
            Err(DecodeError::MissingBytes) // TODO Missing scanline err
        }

    }*/
/*
    // TODO: understand why we need 'a here
    // self.next_y and self.next_pass must be kept before calling this, they are invalid just after (todo ?)
    pub fn next_scanline<'b>(&'b mut self) -> Result<DecodedScanline<'a, 'src, 'buf>, DecodeError> {
        let bytes_per_pixel = self.png.header.bytes_per_pixel();
        match self.png.header.interlace_method {
            InterlaceMethod::None => {
                log::info!("None");
                let bytes_per_scanline = self.png.header.bytes_per_scanline_max()?; // TODO remove error case
                log::info!("None1");
                let y = self.next_y;
                self.decompressor.decode_next_scanline(&mut self.last_scanline, bytes_per_pixel)?;
                self.next_y += 1;
                Ok(DecodedScanline::new_linear(self.png, self.last_scanline, y))
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
                let (pass_width, pass_height) = self.png.header.pass_info(pass);
                let bytes_per_scanline = pass_width * bytes_per_pixel;
                let y = self.next_y;
                self.decompressor.decode_next_scanline(&mut self.last_scanline, bytes_per_pixel)?;
                self.next_y += 1;
                if self.next_y > pass_height {
                    loop {
                        self.next_pass += 1;
                        if self.next_pass == 8 {
                            self.next_pass = 1; // this means the end
                        }
                        // Skip empty passes (the reason for loop existence)
                        let (pass_width, pass_height) = self.png.header.pass_info(self.next_pass);
                        if pass_width != 0 && pass_height != 0 {
                            // TODO reset last scanline on new passes
                            break;
                        }
                    };
                }
                // TODO limit scanline width
                Ok(DecodedScanline::new_interlaced(self.png, self.last_scanline, y, pass))
            },
        }
    }*/
}


/*
pub struct ColorIterator<'src, 'buf> {
    reader: PngReader<'src, 'buf>,
    scanline: Option<DecodedScanline<'buf>,
    x: usize,
}

impl<'src, 'buf> Iterator for ColorIterator<'src, 'buf> {
    // first try
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

pub struct PixelIterator<'src, 'buf> {
    reader: PngReader<'src, 'buf>,
    x: usize,
}
 */