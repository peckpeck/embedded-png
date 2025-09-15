use core::cmp::min;
use log::{error, info};
use miniz_oxide::inflate::core::{decompress, DecompressorOxide};
use miniz_oxide::inflate::core::inflate_flags::{TINFL_FLAG_COMPUTE_ADLER32, TINFL_FLAG_HAS_MORE_INPUT, TINFL_FLAG_PARSE_ZLIB_HEADER};
use miniz_oxide::inflate::TINFLStatus;
use crate::error::DecodeError;
use crate::png::Chunk;
use crate::types::{ChunkType, FilterType};

/// A single PNG scanline that points to decompressor buffer data
pub struct ScanlineData<'buf> {
    // scanline filter type cache (todo remove)
    filter_type: FilterType,
    // a scanline spans at most 3 buffers because the decompressor uses a Q buffer
    buffer1: &'buf [u8],
    buffer2: &'buf [u8],
    buffer3: &'buf [u8],
    // optimization to avoid computing position all the time
    buffer3_pos: usize,
    // used for bound checking only, TODO maybe remove
    total_size: usize,
}

/// A single PNG scanline that points to decompressor buffer data
pub struct ScanlineRef {
    extra_buffer_start: usize,
    main_buffer_start: usize,
}

impl<'buf> ScanlineData<'buf> {
    // create a scanline from the Q buffer
    // returns the size of the consumed data from main buffer
    fn new(extra_buffer: &'buf [u8], main_buffer: &'buf [u8], main_pos: usize, size: usize) -> Result<(usize, Self), DecodeError> {
        // data is taken from extra_buffer them from main_buffer
        // - extra_buffer if fully consumed
        // - remaining bytes are taken from main buffer
        // - in potentially 2 steps if it spans past the end of the allocated memory
        if extra_buffer.len() > size { error!("Extra buffer too big {}", extra_buffer.len()); } // debug_assert
        // first consume extra buffer into buffer 1
        let size_rest = size - extra_buffer.len();
        // since we take 2 mut pointers from main_buffer, we have to split_at_mut
        let (b1, b2) = main_buffer.split_at(main_pos);
        // first part (in circular order) of main_buffer into buffer2
        let buffer2_end = min(size_rest, b2.len());
        let buffer2 = &b2[..buffer2_end];
        // second part (in circular order) of main_buffer into buffer3
        let size_rest = size_rest.saturating_sub(buffer2_end);
        let buffer3 = &b1[..size_rest];
        let buffer3_pos = extra_buffer.len() + buffer2.len();
        // also return consumed data for the caller to update its index
        let consumed = buffer2.len() + buffer3.len();
        let mut scanline = ScanlineData {
            filter_type: FilterType::None, // default value, will be overridden
            buffer1: extra_buffer,
            buffer2,
            buffer3,
            buffer3_pos,
            total_size: size,
        };
        scanline.update_filter_type()?;
        Ok((consumed, scanline))
    }
    
    // this cannot be done in new because we need a scanline object to call get()
    fn update_filter_type(&mut self) -> Result<(), DecodeError> {
        self.filter_type = FilterType::try_from(self.get(0))
            .map_err(|_| DecodeError::InvalidFilterType)?;
        Ok(()) 
    }

    // get a single byte of the buffer
    // TODO remove ?
    fn get(&self, index: usize) -> u8 {
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
    fn copy_to_slice(&self, slice: &mut [u8]) {
        if slice.len() > self.total_size { info!("copy_to_slice error {} >= {}", slice.len(), self.total_size); }
        if self.buffer1.len() > 0 {
            slice[..self.buffer1.len()-1].copy_from_slice(&self.buffer1[1..]);
            slice[self.buffer1.len()-1..self.buffer3_pos-1].copy_from_slice(self.buffer2);

        } else if self.buffer2.len() > 0 {
            slice[..self.buffer3_pos-1].copy_from_slice(&self.buffer2[1..]);
            slice[self.buffer3_pos-1..].copy_from_slice(self.buffer3);
        } else {
            slice.copy_from_slice(&self.buffer3[1..]);
        }
    }

    fn enumerate(&self) -> impl Iterator<Item = (usize, u8)> {
        self.buffer1.iter()
            .chain(self.buffer2.iter())
            .chain(self.buffer3.iter())
            .skip(1)
            .copied()
            .enumerate()
    }

    // decode scanline data into last scanline buffer
    pub fn decode(&self, last_scanline: &mut [u8], bytes_per_pixel: usize) {
        match self.filter_type {
            FilterType::None => self.copy_to_slice(last_scanline),
            FilterType::Sub => {
                let mut left_pixel = [0_u8; 8];
                self.enumerate()
                    .fold(0, |byte, (i,value)| {
                        let left = left_pixel[byte];
                        last_scanline[i] = value.wrapping_add(left);
                        left_pixel[byte] = last_scanline[i];
                        (byte+1) % bytes_per_pixel
                    });
            }
            FilterType::Up => for (i,value) in self.enumerate() {
                                  last_scanline[i] = value.wrapping_add(last_scanline[i]);
                              }
            FilterType::Average => {
                let mut left_pixel = [0_u8; 8];
                self.enumerate()
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
                self.enumerate()
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
                            left_pixel[byte]
                        };
                        top_left_pixel[byte] = last_scanline[i];
                        last_scanline[i] = value.wrapping_add(predictor);
                        left_pixel[byte] = last_scanline[i];
                        (byte+1) % bytes_per_pixel
                    });
            }
        }
    }
}

/// The decompressor is implemented as a Q buffer
/// Q because it is a circular buffer + a linear buffer, which if you draw them, looks like a Q
/// Data is always decompressed in the circular buffer, but since the whole circular buffer
/// might be needed  when extracting new data, we temporarily store remaining data
/// in the linear buffer.
/// All of this would not be needed if we could control the maximum data that must be decompressed
pub struct ChunkDecompressor<'src, 'buf> {
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
    data_pos: usize,
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
    pub fn new(data_chunks: &'src [u8], buffer: &'buf mut [u8], buffer_extra: &'buf mut [u8], check_crc: bool) -> Self {
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
            data_pos: 0,
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
                let next_chunk = Chunk::from_bytes(self.data_chunks, next_start, false).unwrap();
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

    /*
    // TODO lifetime
    // produce a single scanline of given size from the buffer, assuming it has enough data
    fn scanline<'a: 'buf>(&'a mut self, size: usize) -> Result<ScanlineData<'buf>, DecodeError> {
        let start_pos = if self.buffer_count > self.buffer_pos {
            self.buffer.len() - self.buffer_count + self.buffer_pos
        } else {
            self.buffer_pos - self.buffer_count
        };
        let (consumed, scanline) =
            ScanlineData::new(&self.buffer_extra[..self.extra_count],
                              self.buffer,
                              start_pos, size)?;
        //self.extra_count = 0;
        //self.buffer_count -= consumed;
        Ok(scanline)
    }*/

    // get the next scanline, extracting data with the decompressor if needed
    fn get_enough_data(&mut self, size: usize) -> Result<(), DecodeError> {
        // we already have enough data
        if self.buffer_count + self.extra_count >= size {
            return Ok(());
        }

        // now we need to decompress some bytes
        // position where to start decompressing
        let mut buffer_pos = self.buffer_count + self.data_pos;

        // since decompress() does not cross the buffer wrap but can grow through the end
        // we must save any data that is after buffer_pos
        if buffer_pos > self.buffer.len() {
            buffer_pos -= self.buffer.len();
            // save what's after the new buffer_pos
            let data_count = self.buffer.len() - self.data_pos;
            let new_extra_count = self.extra_count + data_count;
            // there is no overflow since buffer_extra has enough data for a single scanline
            self.buffer_extra[self.extra_count..new_extra_count].copy_from_slice(&self.buffer[self.data_pos..self.buffer.len()]);
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
                       &mut self.buffer,
                       buffer_pos,
                       self.flags
            );

        // account for byte read
        self.current_chunk.as_mut().map(|chunk| &chunk[in_count..]);
        self.check_for_end();

        // account for bytes written
        self.buffer_count += out_count;
        if buffer_pos + out_count > self.buffer.len() { info!("decompress wrapped around"); } // debbug_assert

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

    // extract a filter type from first data byte
    fn filter_type(&mut self) -> Result<FilterType, DecodeError> {
        let byte = if self.extra_count > 0 {
            self.buffer_extra[0]
        } else {
            self.buffer[self.data_pos]
        };
        FilterType::try_from(byte).map_err(|_| DecodeError::InvalidFilterType)
    }

    // copy the whole scanline_data to slice
    fn copy_to_slice(&self, target: &mut [u8]) {
        let count = target.len();
        if count + 1 > self.buffer_count + self.extra_count { info!("copy_to_slice, error slice too big {} > {}", count + 1, self.buffer_count + self.extra_count); }
        if count + 1 < self.extra_count { info!("copy_to_slice error, extra too big {} < {}", count + 1, self.extra_count); }
        if self.extra_count > 1 {
            // first extra_buffer
            let next_count = self.extra_count-1;
            target[..next_count].copy_from_slice(&self.buffer_extra[1..]);
            // then first half of circular buffer
            let count = count - next_count;
            if count > 0 {
                let next_pos = next_count;
                let buffer_end = min(self.data_pos + count, self.buffer.len());
                let next_count = buffer_end - self.data_pos;
                target[next_pos..next_pos + next_count].copy_from_slice(&self.buffer[self.data_pos..buffer_end]);
                // finally second half if needed
                let count = count - next_count;
                if count > 0 {
                    let next_pos = next_pos + next_count;
                    target[next_pos..].copy_from_slice(&self.buffer[..count]);
                }
            }
        } else {
            // first half of circular buffer
            let buffer_end = min(self.data_pos + count, self.buffer.len());
            let next_count = buffer_end - self.data_pos - 1;
            target[..next_count].copy_from_slice(&self.buffer[self.data_pos+1..buffer_end]);
            // finally second half if needed
            let count = count - next_count;
            if count > 0 {
                let next_pos = next_count;
                target[next_pos..].copy_from_slice(&self.buffer[..count]);
            }
        }
    }

    fn enumerate(&self, count: usize) -> impl Iterator<Item = (usize, u8)> {
        let end = min(self.data_pos + count, self.buffer.len());
        self.buffer_extra[..self.extra_count].iter()
            .chain(self.buffer[self.data_pos..end].iter())
            .chain(if end == self.buffer.len() {
                self.buffer[0..count-(self.buffer.len()-self.data_pos)].iter()
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
                            left_pixel[byte]
                        };
                        top_left_pixel[byte] = last_scanline[i];
                        last_scanline[i] = value.wrapping_add(predictor);
                        left_pixel[byte] = last_scanline[i];
                        (byte+1) % bytes_per_pixel
                    });
            }
        }

        Ok(())
    }
}

