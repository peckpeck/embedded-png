use core::convert::{TryFrom, TryInto};
use crc32fast::Hasher;
use embedded_graphics_core::draw_target::DrawTarget;
use embedded_graphics_core::Pixel;
use embedded_graphics_core::pixelcolor::{Argb8888, PixelColor, Rgb888};
use embedded_graphics_core::prelude::{Point, Size};
use embedded_graphics_core::primitives::Rectangle;
use log::info;
use crate::colors::{AlphaHandler, AlphaColor, PixelsIterator, DontDraw, ReturnC};
use crate::error::DecodeError;
use crate::inflate::ChunkDecompressor;
use crate::{read_u32, IgnoreAlpha};
use crate::types::*;

const PNG_MAGIC_BYTES: &[u8] = &[137, 80, 78, 71, 13, 10, 26, 10];

pub struct ParsedPng<'src, H> {
    pub header: PngHeader,
    pub pixel_type: PixelType<'src>,
    pub palette: Option<&'src [u8]>,
    //pub transparency: Option<TransparencyChunk<'src>>,
    pub background: Option<&'src [u8]>,
    pub crc_checked: bool,
    // no need to store all data chunks separately, they HAVE TO be consecutive
    pub data_chunks: &'src [u8], // TODO pub ?
    pub alpha_handler: H,
}

impl<'src, H> ParsedPng<'src, H> {
    pub fn from_bytes(bytes: &'src [u8], check_crc: bool, alpha_handler: H) -> Result<Self, DecodeError> {
        if bytes.len() < PNG_MAGIC_BYTES.len() {
            return Err(DecodeError::MissingBytes);
        }

        if &bytes[0..PNG_MAGIC_BYTES.len()] != PNG_MAGIC_BYTES {
            return Err(DecodeError::InvalidMagicBytes);
        }

        let header_chunk = Chunk::from_bytes(bytes, PNG_MAGIC_BYTES.len(), check_crc)?;
        let header = PngHeader::from_chunk(&header_chunk)?;

        let mut palette = None;
        let mut transparency = None;
        let mut background = None;
        let mut data_start = None;
        let mut data_end = None;

        let mut start = header_chunk.end;

        while !bytes.is_empty() {
            let chunk = Chunk::from_bytes(bytes, start, check_crc)?;

            match chunk.chunk_type {
                ChunkType::ImageData => {
                    if data_start.is_none() { data_start = Some(chunk.start); }
                    data_end = Some(chunk.end);
                },
                ChunkType::Palette => palette = Some(chunk.data),
                ChunkType::Transparency => transparency = Some(chunk.data),
                ChunkType::Background => background = Some(chunk.data),
                ChunkType::ImageEnd => break,
                _ => {},
            }

            start = chunk.end;
        }
        let pixel_type = PixelType::new(header.color_type, header.bit_depth, palette, transparency)?;

        if let (Some(data_start), Some(data_end)) = (data_start, data_end) {
            Ok(ParsedPng {
                header,
                data_chunks: &bytes[data_start..data_end],
                palette,
                pixel_type,
                background,
                //transparency,
                crc_checked: check_crc,
                alpha_handler,
            })
        } else {
            Err(DecodeError::MissingBytes)
        }
    }

    //    fn linear_draw<T: DrawTarget>(&self, decompressor: &mut ChunkDecompressor, scanline_buf: &mut [u8], target: T) -> Result<(), DecodeError> {
    fn draw_inner<F, C>(&self,
                        buffer: &mut [u8], buffer_extra: &mut [u8],
                        scanline_buf: &mut [u8], mut writer: F) -> Result<(), DecodeError>
    where
        F: FnMut(usize, usize, PixelsIterator<C, H>) -> Result<(), DecodeError>,
        H: AlphaHandler<C>,
    {
        let mut decompressor = ChunkDecompressor::new_ref(
            self.data_chunks, buffer, buffer_extra, self.crc_checked
        );
        let bytes_per_pixel = self.header.bytes_per_pixel();
        match self.header.interlace_method {
            InterlaceMethod::None => {
                let bytes_per_scanline = self.header.bytes_per_scanline_max()?; // TODO remove error case
                //let mut line = Rectangle::new(Point::new(0,0), Size::new(1280,1));
                for y in 0..self.header.height {
                    // just in case the buffer is not the exact size
                    decompressor.decode_next_scanline(&mut scanline_buf[..bytes_per_scanline], bytes_per_pixel)?;
                    let it = PixelsIterator::new(self, scanline_buf);
                    writer(y, 0, it)?;
                    //line.top_left.y = y as i32;
                    //target.fill_contiguous(&line, it).map_err(|_| DecodeError::MissingBytes)?;
                }
            }
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
                for pass in 1..=7 {
                    let (pass_width, pass_height) = self.header.pass_info(pass);
                    // Skip empty passes.
                    if pass_width == 0 || pass_height == 0 {
                        continue;
                    }
                    info!("pass {}: {} x {}", pass, pass_width, pass_height);

                    let bytes_per_scanline = pass_width * bytes_per_pixel;
                    info!("scanline {}", bytes_per_scanline);
                    let last_scanline = &mut scanline_buf[..(bytes_per_scanline)];

                    for y in 0..pass_height {
                        decompressor.decode_next_scanline(last_scanline, bytes_per_pixel)?;
                        let it = PixelsIterator::new(self, last_scanline);
                        writer(y, pass, it)?;
                    }
                }
            }
        }
        /*
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
        // assume InterlaceMethod is None
 */
        /*        let bytes_per_pixel = self.header.bytes_per_pixel();
                let bytes_per_scanline = self.header.bytes_per_scanline_max()?; // TODO remove error case
                info!("Pixel type {:?}", self.pixel_type);
                for y in 0..self.header.height {
                    match decompressor.decode_next_scanline(scanline_buf, bytes_per_pixel) {
                        Ok(_) => {}
                        Err(e) => {info!("scanline error {:?}",e); return Err(e) },
                    }
                    //let it = PixelsIterator::new(self, scanline_buf);
                    //let it = RgbAlpha8Iterator{ scanline: scanline_buf, pos: 0 };
                    //let line = Rectangle::new(Point::new(0,y as i32), Size::new(1280,1));
                    //target.fill_contiguous(&line, it).map_err(|_| DecodeError::MissingBytes)?;
         //           for x in 0..self.header.width {
         //               let (r,g,b,a) =  get_color(self, scanline_buf, x);
         //               let rgb = Rgb888::new(r,g,b);
         //           }
                }
        */        Ok(())
    }

    pub fn draw_to_fn<F, C>(&self,
                            buffer: &mut [u8], buffer_extra: &mut [u8],
                            scanline_buf: &mut [u8], mut write: F) -> Result<(), DecodeError>
    // TODO Result
    where
            for<'a> F: FnMut(usize, usize, <PixelsIterator<'a, C, H> as Iterator>::Item) -> Result<(), DecodeError>,
            for<'a> PixelsIterator<'a, C, H>: Iterator,
            H: AlphaHandler<C>,
    {
        self.draw_inner(buffer, buffer_extra, scanline_buf, |y: usize, pass: usize, it: PixelsIterator<C, H>| {
            for (x, c) in it.enumerate() {
                let (x,y) = get_xy(pass, x, y);
                write(x, y, c)?;
            }
            Ok(())
        })
    }
}

impl<'src, H: ReturnC> ParsedPng<'src, H> {
    pub fn draw_to_target<T, C>(&self,
                                buffer: &mut [u8], buffer_extra: &mut [u8],
                                scanline_buf: &mut [u8], target: &mut T) -> Result<(), DecodeError>
    where for<'a> T: DrawTarget<Color=C>,
          for<'a> PixelsIterator<'a, C, H>: Iterator<Item=C>,
          C: PixelColor,
          H: AlphaHandler<C>,
    {
        self.draw_inner(buffer, buffer_extra, scanline_buf, |y: usize, pass: usize, it: PixelsIterator<C, H>| {
            if pass == 0 {
                let line = Rectangle::new(Point::new(0,y as i32), Size::new(self.header.width as u32,1));
                target.fill_contiguous(&line, it).map_err(|_| DecodeError::MissingBytes)
            } else {
                target.draw_iter(it.enumerate()
                    .map(|(x, c)| {
                        let (x,y) = get_xy(pass, x, y);
                        Pixel(Point::new(x as i32,y as i32), c)
                    })).map_err(|e| DecodeError::MissingBytes)
            }
        })
    }
}

impl<'src> ParsedPng<'src, DontDraw> {
    pub fn draw_to_target<T, C>(&self,
                                buffer: &mut [u8], buffer_extra: &mut [u8],
                                scanline_buf: &mut [u8], target: &mut T) -> Result<(), DecodeError>
    where for<'a> T: DrawTarget<Color=C>,
          for<'a> PixelsIterator<'a, C, DontDraw>: Iterator<Item=(u8,C)>,
          C: PixelColor,
    {
        self.draw_inner(buffer, buffer_extra, scanline_buf, |y: usize, pass: usize, it: PixelsIterator<C, DontDraw>| {
            target.draw_iter(it.enumerate()
                .filter_map(|(x, (a,c))| {
                    if a >= 128 {
                        let (x, y) = get_xy(pass, x, y);
                        Some(Pixel(Point::new(x as i32, y as i32), c))
                    } else {
                        None
                    }
                })).map_err(|e| DecodeError::MissingBytes)
        })
    }
}

fn get_xy(pass: usize, x: usize, y: usize) -> (usize, usize) {
    match pass {
        0 => (x, y),
        1 => (x * 8, y * 8),
        2 => (x * 8 + 4, y * 8),
        3 => (x * 4, y * 8 + 4),
        4 => (x * 4 + 2, y * 4),
        5 => (x * 2, y * 4 + 2),
        6 => (x * 2 + 1, y * 2),
        7 => (x, y * 2 + 1),
        _ => (0, 0),
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

    // TODO this is probably wrong with greyscale
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn parse_png() {
        let bytes = fs::read("sekiro.png").unwrap();
        let png = ParsedPng::from_bytes(&bytes, true, AlphaColor);
        assert!(png.is_ok(), "Png decoded incorrectly");
        let png = png.unwrap();
        assert_eq!(png.data_chunks.len(), 1_154_975, "Png chunks incorrect");
    }

    // TODO test decoding
}
