//! DOC not suitable for 16bits usize

mod png;
mod error;
mod types;
mod inflate;

use embedded_graphics_core::{pixelcolor::PixelColor,
                             pixelcolor::Rgb888,
                             draw_target::DrawTarget,
                             geometry::OriginDimensions,
                             image::ImageDrawable,
                             prelude::{Dimensions, Size},
                             primitives::Rectangle};
use embedded_graphics_core::geometry::Point;
use png_decoder::{pre_decode, PngHeader, DecodeError, UndecodedPng};
pub use crate::png::ParsedPng;
pub use crate::png::PngReader;

pub type Png<C> = BufferedPng<C>;

pub struct BufferedPng<C> {
    pub header: PngHeader,
    pub data: Vec<C>,
}

impl<C: PixelColor + Default + From<Rgb888>> BufferedPng<C> {
    // TODO from_bytes
    pub fn from_slice(bytes: &[u8]) -> Result<Self, DecodeError> {
        // TODO decode / uncompress using iterators
        let mut undecoded = pre_decode(bytes)?;

        // For now, output data is always RGBA, 1 byte per channel.
        let mut data = vec![C::default(); undecoded.header.width as usize * undecoded.header.height as usize];
        let width =  undecoded.header.width as usize;

        undecoded.process_scanlines(|scanline_iter,xy_calculator,y| {
                for (idx, (r, g, b, a)) in scanline_iter.enumerate() {
                    let (x, y) = xy_calculator.get_xy(idx, y);
                    let i = (y * width) + x;
                    data[i] = C::from(Rgb888::new(r, g, b));
                }
            },
        )?;

        Ok(Png {header: undecoded.header, data })

    }
}

impl<C: PixelColor> OriginDimensions for BufferedPng<C> {
    fn size(&self) -> Size {
        Size::new(self.header.width, self.header.height)
    }
}

impl<C: PixelColor> ImageDrawable for BufferedPng<C> {
    type Color = C;

    fn draw<D>(&self, target: &mut D) -> Result<(), D::Error> where D: DrawTarget<Color=Self::Color> {
        let area = self.bounding_box();
        target.fill_contiguous(&area, self.data.iter().copied())
    }

    fn draw_sub_image<D>(&self, target: &mut D, area: &Rectangle) -> Result<(), D::Error> where D: DrawTarget<Color=Self::Color> {
        let mut index = area.top_left.x + area.top_left.y * self.header.width as i32;
        let line_size = Size::new(area.size.width, 1);
        for i in 0 ..  area.size.height as i32 {
            let pos = Point::new(0, i);
            let target_area = Rectangle::new(pos, line_size);
            let data = &self.data[index as usize .. index as usize + area.size.width as usize];
            target.fill_contiguous(&target_area, data.iter().copied())?;
            index += self.header.width as i32;
        }
        Ok(())
    }
}

pub fn dump_png(bytes: &[u8]) {
    log::info!("dumping");
    let data = match ParsedPng::from_bytes(bytes, true) {
        Ok(x) => {log::info!("okok"); log::info!("Ok {:?}", x.header)},
        Err(e) => {log::info!("Err {:?}",e)}
    };
}
/*
pub struct EncodedPng<'a> {
    pub encoded: UndecodedPng<'a>
}

impl<'a> EncodedPng<'a> {
    pub fn from_bytes(bytes: &'a [u8]) -> Result<Self, DecodeError> {
        let encoded = pre_decode(bytes)?;
        Ok(EncodedPng { encoded })
    }
}

impl<'a> OriginDimensions for EncodedPng<'a> {
    fn size(&self) -> Size {
        Size::new(self.encoded.header.width, self.encoded.header.height)
    }
}

impl<C: PixelColor> ImageDrawable for EncodedPng<'a> {
    type Color = C;

    fn draw<D>(&self, target: &mut D) -> Result<(), D::Error> where D: DrawTarget<Color=Self::Color> {
        let area = self.bounding_box();
        target.draw_iter()
        target.fill_contiguous(&area, self.data.iter().copied())
    }

    fn draw_sub_image<D>(&self, target: &mut D, area: &Rectangle) -> Result<(), D::Error> where D: DrawTarget<Color=Self::Color>
    {}
}
*/

fn read_u32(bytes: &[u8], offset: usize) -> u32 {
    u32::from_be_bytes([bytes[offset], bytes[offset + 1], bytes[offset + 2], bytes[offset + 3]])
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        //let result = add(2, 2);
        assert_eq!(4, 4);
    }
}
