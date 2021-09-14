use std::{hash::Hasher, num::Wrapping};

pub struct XLangHasher(Wrapping<u64>);

const PRIME: u64 = 1099511628211;

impl XLangHasher {
    pub const fn new() -> Self {
        Self(Wrapping(14695981039346656037))
    }
}

impl Hasher for XLangHasher {
    fn finish(&self) -> u64 {
        self.0 .0
    }

    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.0 ^= Wrapping(b as u64);
            self.0 *= Wrapping(PRIME);
        }
    }

    fn write_u8(&mut self, i: u8) {
        self.write(&[i])
    }

    fn write_u16(&mut self, i: u16) {
        self.write(&i.to_ne_bytes())
    }

    fn write_u32(&mut self, i: u32) {
        self.write(&i.to_ne_bytes())
    }

    fn write_u64(&mut self, i: u64) {
        self.write(&i.to_ne_bytes())
    }

    fn write_u128(&mut self, i: u128) {
        self.write(&i.to_ne_bytes())
    }

    fn write_usize(&mut self, i: usize) {
        self.write(&i.to_ne_bytes())
    }

    fn write_i8(&mut self, i: i8) {
        self.write_u8(i as u8)
    }

    fn write_i16(&mut self, i: i16) {
        self.write(&i.to_ne_bytes())
    }

    fn write_i32(&mut self, i: i32) {
        self.write(&i.to_ne_bytes())
    }

    fn write_i64(&mut self, i: i64) {
        self.write(&i.to_ne_bytes())
    }

    fn write_i128(&mut self, i: i128) {
        self.write(&i.to_ne_bytes())
    }

    fn write_isize(&mut self, i: isize) {
        self.write(&i.to_ne_bytes())
    }
}

impl Default for XLangHasher {
    fn default() -> XLangHasher {
        Self::new()
    }
}
