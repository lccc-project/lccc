use std::{
    hash::{BuildHasher, Hasher},
    marker::PhantomData,
    num::Wrapping,
};

use crate::span::Span;

#[cfg(not(any(miri, test)))]
xlang_host::rustcall! {
    extern "rustcall"{
        /// Hashes the bytes in the given span
        ///
        /// # Safety
        /// This function is always safe to call
        pub fn xlang_hash_bytes(x: Span<u8>) -> u64;

        /// A u8 that, when hashed with [`xlang_hash_bytes`] produces a value suitable for use
        pub static XLANG_HASH_SEED: u8;
    }
}
#[cfg(any(miri, test))]
xlang_host::rustcall! {
    ///
    pub unsafe extern "rustcall" fn xlang_hash_bytes(x: Span<u8>) -> u64{
        // When running tests, we don't want to bring in lazy_static to xlang_abi, but since hash seeds will be randomized at runtime, we want to choose a random seed when running miri
        #[cfg(not(miri))]
        static SEED: u64 = 14_695_981_039_346_656_037;
        #[cfg(miri)]
        static mut SEED: u64 = 0;

        #[cfg(miri)]
        {
            static SEED_ONCE: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

            if SEED_ONCE.load(std::sync::atomic::Ordering::Acquire)&1==0{
                while (SEED_ONCE.fetch_or(0x100,std::sync::atomic::Ordering::Acquire)&0x100)!=0{}
                if SEED_ONCE.load(std::sync::atomic::Ordering::Acquire)&1==0{
                    SEED = (&SEED_ONCE as *const _ as usize as u64)^14_695_981_039_346_656_037;
                }
                SEED_ONCE.store(0x1,std::sync::atomic::Ordering::Release); // Release lock and set init bit
            }

        }

        let mut hash = SEED;

        for b in x{
            hash ^= (*b) as u64;
            hash =hash.wrapping_mul(PRIME);
        }

        hash
    }
}

#[cfg(any(miri, test))]
///
pub static XLANG_HASH_SEED: u8 = 1;

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

    /// Returns a new [`XLangHasher`] with a given seed
    ///
    ///
    #[must_use]
    pub const fn from_seed(seed: u64) -> Self {
        Self(Wrapping(seed))
    }
}

impl Hasher for XLangHasher {
    fn finish(&self) -> u64 {
        self.0 .0
    }

    fn write(&mut self, bytes: &[u8]) {
        self.0 ^= Wrapping(unsafe { xlang_hash_bytes(Span::from(bytes)) });
        self.0 *= Wrapping(PRIME);
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
        // SAFETY: xlang_hash_bytes has no undefined behaviour
        Self::from_seed(unsafe { xlang_hash_bytes(Span::from_ref(&XLANG_HASH_SEED)) })
    }
}
