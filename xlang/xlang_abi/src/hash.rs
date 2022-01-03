use std::{
    hash::{BuildHasher, Hasher},
    marker::PhantomData,
    num::Wrapping,
};

///
/// A hasher with a consistent ABI and hash algorithm.
/// the [`XLangHasher`] hashes bytes using the `FNV-1a` 64-bit hash
#[repr(C)]
#[derive(Debug)]
pub struct XLangHasher(Wrapping<u64>);

/// A [`BuildHasher`] with a consistent ABI that returns Default values for the given Hasher.
#[repr(C)]
#[derive(Debug)]
pub struct BuildHasherDefault<H>(PhantomData<H>);

impl<H: Default + Hasher> BuildHasher for BuildHasherDefault<H> {
    type Hasher = H;
    fn build_hasher(&self) -> H {
        Default::default()
    }
}

impl<H> Clone for BuildHasherDefault<H> {
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl<H> Default for BuildHasherDefault<H> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

const PRIME: u64 = 1_099_511_628_211;

impl XLangHasher {
    /// Returns a new [`XLangHasher`]
    ///
    /// Each [`XLangHasher`] is initialized with the same value, for consistency.
    #[must_use]
    pub const fn new() -> Self {
        Self(Wrapping(14_695_981_039_346_656_037))
    }
}

impl Hasher for XLangHasher {
    fn finish(&self) -> u64 {
        self.0 .0
    }

    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.0 ^= Wrapping(u64::from(b));
            self.0 *= Wrapping(PRIME);
        }
    }

    fn write_u8(&mut self, i: u8) {
        self.write(&[i]);
    }

    fn write_u16(&mut self, i: u16) {
        self.write(&i.to_ne_bytes());
    }

    fn write_u32(&mut self, i: u32) {
        self.write(&i.to_ne_bytes());
    }

    fn write_u64(&mut self, i: u64) {
        self.write(&i.to_ne_bytes());
    }

    fn write_u128(&mut self, i: u128) {
        self.write(&i.to_ne_bytes());
    }

    fn write_usize(&mut self, i: usize) {
        self.write(&i.to_ne_bytes());
    }

    #[allow(clippy::cast_sign_loss)]
    fn write_i8(&mut self, i: i8) {
        self.write_u8(i as u8);
    }

    fn write_i16(&mut self, i: i16) {
        self.write(&i.to_ne_bytes());
    }

    fn write_i32(&mut self, i: i32) {
        self.write(&i.to_ne_bytes());
    }

    fn write_i64(&mut self, i: i64) {
        self.write(&i.to_ne_bytes());
    }

    fn write_i128(&mut self, i: i128) {
        self.write(&i.to_ne_bytes());
    }

    fn write_isize(&mut self, i: isize) {
        self.write(&i.to_ne_bytes());
    }
}

impl Default for XLangHasher {
    fn default() -> Self {
        Self::new()
    }
}
