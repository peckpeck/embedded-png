use core::convert::{TryFrom, TryInto};
use crc32fast::Hasher;
use embedded_graphics_core::draw_target::DrawTarget;
use embedded_graphics_core::pixelcolor::{Argb8888, PixelColor, Rgb888};
use embedded_graphics_core::prelude::{Point, Size};
use embedded_graphics_core::primitives::Rectangle;
use log::info;
use crate::colors::PixelsIterator;
use crate::error::DecodeError;
use crate::inflate::ChunkDecompressor;
use crate::read_u32;
use crate::types::*;

const PNG_MAGIC_BYTES: &[u8] = &[137, 80, 78, 71, 13, 10, 26, 10];

pub struct ParsedPng<'src> {
    pub header: PngHeader,
    pub pixel_type: PixelType<'src>,
    pub palette: Option<&'src [u8]>,
    //pub transparency: Option<TransparencyChunk<'src>>,
    pub background: Option<&'src [u8]>,
    pub crc_checked: bool,
    // no need to store all data chunks separately, they HAVE TO be consecutive
    pub data_chunks: &'src [u8], // TODO pub ?
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
            })
        } else {
            Err(DecodeError::MissingBytes)
        }
    }

//    fn linear_draw<T: DrawTarget>(&self, decompressor: &mut ChunkDecompressor, scanline_buf: &mut [u8], target: T) -> Result<(), DecodeError> {
    pub fn linear_draw<T: DrawTarget<Color=Argb8888>>(&self,
                                    buffer: &mut [u8], buffer_extra: &mut [u8],
                                  scanline_buf: &mut [u8], target: &mut T) -> Result<(), DecodeError> {
        let mut  decompressor = ChunkDecompressor::new_ref(
            self.data_chunks, buffer, buffer_extra, self.crc_checked
        );
        // assume InterlaceMethod is None
        let bytes_per_pixel = self.header.bytes_per_pixel();
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
        Ok(())
    }
    //fn interlaced_drawer(&self) -> InterlacedReader {    }
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
        let png = ParsedPng::from_bytes(&bytes, true);
        assert!(png.is_ok(), "Png decoded incorrectly");
        let png = png.unwrap();
        assert_eq!(png.data_chunks.len(), 1_154_975, "Png chunks incorrect");
    }
}
