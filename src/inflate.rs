use core::cmp::min;
use miniz_oxide::inflate::core::{decompress, DecompressorOxide};
use miniz_oxide::inflate::core::inflate_flags::{TINFL_FLAG_COMPUTE_ADLER32, TINFL_FLAG_HAS_MORE_INPUT, TINFL_FLAG_PARSE_ZLIB_HEADER};
use miniz_oxide::inflate::TINFLStatus;
use crate::error::DecodeError;
use crate::png::Chunk;
use crate::types::{ChunkType, FilterType};

/// The decompressor is implemented as a Q buffer
/// Q because it is a circular buffer + a linear buffer, which if you draw them, looks like a Q
///
/// Data is always decompressed in the circular buffer, but since the whole circular buffer
/// might be needed  when extracting new data, we temporarily store remaining data
/// in the linear buffer.
///
/// The linear part would not be needed if we could control the maximum data that must be decompressed
pub struct ChunkDecompressor<'src, T> {
    // internal miniz decompressor data
    decompressor: DecompressorOxide,
    // slice containing all ImageData chunks
    data_chunks: &'src [u8],
    // position of the next chunk in this slice
    next_chunk_start: Option<usize>,
    // slice of the current data chunks
    current_chunk: Option<&'src [u8]>,
    // true when all data have been taken from chunks
    chunk_end: bool,
    // circular buffer where decompressed data is written
    buffer: T,
    // first waiting decompressed byte
    data_pos: usize,
    // size of waiting decompressed data
    buffer_count: usize,
    // non-circular buffer data that must be kept out of circular buffer during decompress call
    buffer_extra: T,
    // size of waiting data in buffer_extra
    extra_count: usize,
    // common flags for decompression
    flags: u32,
    total_decompressed: usize, // TODO remove
    // TODO should we have a next_scanline_size here ?
}

impl<'src, 'buf> ChunkDecompressor<'src, &'buf mut [u8]> {
    // buffer size must be >= min(decompression_window(32k), total_output_size)
    // buffer extra size must be >= max scanline bytes
    pub fn new_ref(data_chunks: &'src [u8], buffer: &'buf mut [u8], buffer_extra: &'buf mut [u8], check_crc: bool) -> Self {
        let decompressor = DecompressorOxide::new();
        // png has zlib header, always pass has more input, it doesn't matter if it's false
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
            data_pos: 0,
            buffer_count: 0,
            buffer_extra,
            extra_count: 0,
            flags,
            total_decompressed: 0,
        }
    }
}

// TODO alloc only
impl<'src> ChunkDecompressor<'src, Vec<u8>> {
    // buffer size must be >= min(decompression_window(32k), total_output_size)
    // buffer extra size must be >= max scanline bytes
    pub fn new_vec(data_chunks: &'src [u8], check_crc: bool) -> Self {
        let decompressor = DecompressorOxide::new();
        // png has zlib header, always pass has more input, it doesn't matter if it's false
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
            buffer: vec![0_u8; 1024<<5],
            data_pos: 0,
            buffer_count: 0,
            buffer_extra: vec![0_u8; 1024<<5], // TODO
            extra_count: 0,
            flags,
            total_decompressed: 0,
        }
    }
    pub fn update_buffers(&mut self) {}
}

impl<'src> ChunkDecompressor<'src, [u8; 1024<<5]> {
    // buffer size must be >= min(decompression_window(32k), total_output_size)
    // buffer extra size must be >= max scanline bytes
    pub fn new_static(data_chunks: &'src [u8], check_crc: bool) -> Self {
        let decompressor = DecompressorOxide::new();
        // png has zlib header, always pass has more input, it doesn't matter if it's false
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
            buffer: [0_u8; 1024<<5],
            data_pos: 0,
            buffer_count: 0,
            buffer_extra: [0_u8; 1024<<5], // TODO
            extra_count: 0,
            flags,
            total_decompressed: 0,
        }
    }
}


impl<'src, T> ChunkDecompressor<'src, T>
where T: AsRef<[u8]> + AsMut<[u8]>
{
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
                let next_chunk = Chunk::from_bytes(self.data_chunks, next_start, false).unwrap();
                if next_chunk.end < self.data_chunks.len() {
                    self.next_chunk_start = Some(next_chunk.end);
                } else {
                    // TODO smelly, we assign none at 2 different steps
                    self.next_chunk_start = None;
                }
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

    // get the next scanline, extracting data with the decompressor if needed
    fn get_enough_data(&mut self, size: usize) -> Result<(), DecodeError> {
        debug_assert!(size <= self.buffer.as_ref().len(), "Decompression buffer too small (need {})", size);
        debug_assert!(size >= self.extra_count, "Decompression extra buffer too big (need {})", size);
        // we already have enough data
        if self.buffer_count + self.extra_count >= size {
            return Ok(());
        }

        // now we need to decompress some bytes
        // position where to start decompressing
        let mut buffer_pos = self.buffer_count + self.data_pos;

        // since decompress() does not cross the buffer wrap but can grow through the end
        // we must save any data that is after buffer_pos and before buffer.len()
        if buffer_pos >= self.buffer.as_ref().len() {
            buffer_pos -= self.buffer.as_ref().len();
            // save what's after the new buffer_pos (between data_pos and buffer.len())
            let data_count = self.buffer.as_ref().len() - self.data_pos;
            let new_extra_count = self.extra_count + data_count;
            // there is no overflow since buffer_extra has enough data for a single scanline
            self.buffer_extra.as_mut()[self.extra_count..new_extra_count].copy_from_slice(&self.buffer.as_ref()[self.data_pos..self.buffer.as_ref().len()]);
            // update info accordingly
            self.data_pos = 0;
            self.extra_count = new_extra_count;
            self.buffer_count = buffer_pos;
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
                       self.buffer.as_mut(),
                       buffer_pos,
                       self.flags
            );
        println!("decompressed {}", out_count);

        // account for byte read
        if let Some(chunk) = &mut self.current_chunk {
            *chunk = &(*chunk)[in_count..];
            if chunk.is_empty() && self.next_chunk_start.is_none() {
                self.chunk_end = true;
            }
        }

        // account for bytes written
        self.buffer_count += out_count;
        self.total_decompressed += out_count;
        debug_assert!(buffer_pos + out_count <= self.buffer.as_ref().len(), "decompress wrapped around");

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

        // rerun to avoid duplicating logic
        self.get_enough_data(size)
    }

    // remove size bytes from buffer
    fn remove_data(&mut self, size: usize) {
        debug_assert!(size >= self.extra_count, "Decompression extra buffer too big to remove (need {})", size);
        let main_buffer_count = size - self.extra_count;
        self.data_pos += main_buffer_count;
        if self.data_pos >= self.buffer.as_ref().len() {
            self.data_pos -= self.buffer.as_ref().len();
        }
        self.buffer_count -= main_buffer_count;
        self.extra_count = 0;
    }

    // extract a filter type from first data byte
    fn filter_type(&mut self) -> Result<FilterType, DecodeError> {
        let byte = if self.extra_count > 0 {
            self.buffer_extra.as_ref()[0]
        } else {
            self.buffer.as_ref()[self.data_pos]
        };
        FilterType::try_from(byte).map_err(|_| DecodeError::InvalidFilterType)
    }

    // copy the whole scanline_data to slice,
    // starting at decompressed pos + 1 : because we do not copy the filter type
    // we copy target len bytes
    fn copy_to_slice(&self, target: &mut [u8]) {
        let count = target.len();
        debug_assert!(count + 1 <= self.buffer_count + self.extra_count, "copy_to_slice, error slice too big {} > {}", count + 1, self.buffer_count + self.extra_count);
        debug_assert!(count + 1 >= self.extra_count, "copy_to_slice error, extra too big {} < {}", count + 1, self.extra_count);
        if self.extra_count > 1 {
            // first extra_buffer
            let next_count = self.extra_count-1;
            target[..next_count].copy_from_slice(&self.buffer_extra.as_ref()[1..self.extra_count]);
            // then first half of circular buffer
            let count = count - next_count;
            if count > 0 {
                let next_pos = next_count;
                let buffer_end = min(self.data_pos + count, self.buffer.as_ref().len());
                let next_count = buffer_end - self.data_pos;
                target[next_pos..next_pos + next_count].copy_from_slice(&self.buffer.as_ref()[self.data_pos..buffer_end]);
                // finally second half if needed
                let count = count - next_count;
                if count > 0 {
                    let next_pos = next_pos + next_count;
                    target[next_pos..].copy_from_slice(&self.buffer.as_ref()[..count]);
                }
            }
        } else {
            // first half of circular buffer
            let buffer_end = min(self.data_pos + 1 + count, self.buffer.as_ref().len());
            let next_count = buffer_end - self.data_pos - 1;
            target[..next_count].copy_from_slice(&self.buffer.as_ref()[self.data_pos+1..buffer_end]);
            // finally second half if needed
            let count = count - next_count;
            if count > 0 {
                let next_pos = next_count;
                target[next_pos..].copy_from_slice(&self.buffer.as_ref()[..count]);
            }
        }
    }

    fn enumerate(&self, count: usize) -> impl Iterator<Item = (usize, u8)> {
        let main_count = count + 1 - self.extra_count;
        let end = min(self.data_pos + main_count, self.buffer.as_ref().len());
        self.buffer_extra.as_ref()[..self.extra_count].iter()
            .chain(self.buffer.as_ref()[self.data_pos..end].iter())
            .chain(if end == self.buffer.as_ref().len() {
                self.buffer.as_ref()[0..main_count-(self.buffer.as_ref().len()-self.data_pos)].iter()
            } else {
                [].iter()
            })
            .skip(1)
            .copied()
            .enumerate()
    }

    pub fn decode_next_scanline(&mut self, last_scanline: &mut [u8], bytes_per_pixel: usize) -> Result<(), DecodeError> {
        self.get_enough_data(last_scanline.len()+1)?;
        let filter_type = self.filter_type()?;

        // decode scanline directly into last scanline
        match filter_type {
            FilterType::None => self.copy_to_slice(last_scanline),
            FilterType::Sub => {
                let mut left_pixel = [0_u8; 8];
                self.enumerate(last_scanline.len())
                    .fold(0, |byte, (i,value)| {
                        let left = left_pixel[byte];
                        last_scanline[i] = value.wrapping_add(left);
                        left_pixel[byte] = last_scanline[i];
                        (byte+1) % bytes_per_pixel
                    });
            }
            FilterType::Up => for (i,value) in self.enumerate(last_scanline.len()) {
                last_scanline[i] = value.wrapping_add(last_scanline[i]);
            }
            FilterType::Average => {
                let mut left_pixel = [0_u8; 8];
                self.enumerate(last_scanline.len())
                    .fold(0, |byte, (i,value)| {
                        let left = left_pixel[byte];
                        let top = last_scanline[i];
                        // we can either work wit u16 or with u8 and a carry
                        // let's choose u16
                        let average = (left as u16 + top as u16) / 2;
                        last_scanline[i] = value.wrapping_add(average as u8);
                        left_pixel[byte] = last_scanline[i];
                        (byte+1) % bytes_per_pixel
                    });
            }
            FilterType::Paeth => {
                let mut top_left_pixel = [0_u8; 8];
                let mut left_pixel = [0_u8; 8];
                self.enumerate(last_scanline.len())
                    .fold(0, |byte, (i,value)| {
                        let a = left_pixel[byte] as i16;
                        let b = last_scanline[i] as i16;
                        let c = top_left_pixel[byte] as i16;
                        let p = a + b - c;      // initial estimate
                        let pa = (p - a).abs(); // distances to a, b, c
                        let pb = (p - b).abs();
                        let pc = (p - c).abs();
                        // return nearest of a,b,c,
                        // breaking ties in order a,b,c.
                        let predictor = if pa <= pb && pa <= pc {
                            left_pixel[byte]
                        } else if pb <= pc {
                            last_scanline[i]
                        } else {
                            top_left_pixel[byte]
                        };
                        top_left_pixel[byte] = last_scanline[i];
                        last_scanline[i] = value.wrapping_add(predictor);
                        left_pixel[byte] = last_scanline[i];
                        (byte+1) % bytes_per_pixel
                    });
            }
        }
        // accounting
        self.remove_data(last_scanline.len() + 1);
        Ok(())
    }

    pub fn reset(&mut self) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use png_decoder::*;
    use crate::colors::AlphaColor;
    use crate::ParsedPng;
    use super::*;

    #[test]
    fn list_chunks() {
        let bytes = fs::read("sekiro.png").unwrap();
        let png = ParsedPng::from_bytes(&bytes, true, AlphaColor).unwrap();

        let mut decompressor = ChunkDecompressor::new_vec(png.data_chunks, true);

        for _ in 0..35 {
            decompressor.current_chunk = None;
            decompressor.check_chunk_data();
            assert!(decompressor.current_chunk.is_some(), "Missing chunk");
            assert!(!decompressor.chunk_end, "Decompression ended early");
            assert_eq!(decompressor.current_chunk.unwrap().len(), 32_768, "Incorrect chunk size");
        }
        decompressor.current_chunk = None;
        decompressor.check_chunk_data();
        assert!(decompressor.current_chunk.is_some(), "Missing chunk");
        assert!(!decompressor.chunk_end, "Decompression ended early");
        assert_eq!(decompressor.current_chunk.unwrap().len(), 7_663, "Incorrect chunk size");
        decompressor.current_chunk = None;
        decompressor.check_chunk_data();
        assert!(decompressor.chunk_end, "Decompression ended late");
    }

    #[test]
    fn read_chunks() {
        let bytes = fs::read("sekiro.png").unwrap();
        let png = ParsedPng::from_bytes(&bytes, true, AlphaColor).unwrap();

        //let mut undecoded = pre_decode(&bytes).unwrap();

        let mut decompressor = ChunkDecompressor::new_vec(png.data_chunks,true);
        let mut scanline = vec![0_u8; 5120];

        for i in 0..720 {
            let r = decompressor.get_enough_data(5121);
            assert!(r.is_ok(), "Get data Error");
            decompressor.copy_to_slice(&mut scanline);
            assert_eq!(decompressor.enumerate(5120).count(), 5120, "Enumerate can't count");
            let enumeration: Vec<u8> = decompressor.enumerate(5120).map(|(_,x)| x).collect();
            assert_eq!(enumeration, scanline, "Enumerate misaligned with copy to slice");
            //assert_eq!(scanline, &undecoded.scanline_data[5121*i+1..5121*(i+1)], "Incorrect data at {}", i);
            decompressor.remove_data(5121);
        }
        assert_eq!(decompressor.buffer_count, 0, "Main buffer left");
        assert!(decompressor.chunk_end, "Decompression left some data");
        assert_eq!(decompressor.extra_count, 0, "Extra buffer left");
    }

    #[test]
    fn decode() {
        let bytes = fs::read("sekiro.png").unwrap();
        let png = ParsedPng::from_bytes(&bytes, true, AlphaColor).unwrap();

        /*let mut undecoded = pre_decode(&bytes).unwrap();
        let mut image = vec![0_u8; 1280*720*4];
        undecoded.process_scanlines(
            |scanline_iter,xy_calculator,y| {
                for (idx, (r, g, b, a)) in scanline_iter.enumerate() {
                    let (x, y) = xy_calculator.get_xy(idx, y);
                    let idx = (x + y * 1280)*4;
                    image[idx] = r;
                    image[idx+1] = g;
                    image[idx+2] = b;
                    image[idx+3] = a;
                }
            }).unwrap();
*/
        let mut decompressor = ChunkDecompressor::new_vec(png.data_chunks, true);
        let mut scanline = vec![0_u8; 5120];
        for i in 0..720 {
            let r = decompressor.decode_next_scanline(&mut scanline, 4);
            assert!(r.is_ok(), "Get data Error");
            //assert_eq!(scanline, &image[i*5120..i*5120 + 5120], "Incorrect image at {}", i);
        }
        assert_eq!(decompressor.buffer_count, 0, "Main buffer left");
        assert!(decompressor.chunk_end, "Decompression left some data");
        assert_eq!(decompressor.extra_count, 0, "Extra buffer left");
    }
    
    // TODO test buffer limits (max size +-1)
}
