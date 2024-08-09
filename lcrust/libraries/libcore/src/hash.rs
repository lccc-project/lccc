#[unstable(feature = "lcrust_siphash_impl")]
#[path = "../../lccc-siphash/src/lib.rs"]
pub mod siphash;

#[deprecated(since = "1.13.0", note = "use `std::hash::DefaultHasher` instead")]
pub struct SipHasher(siphash::siphash::SipHasher<2, 4>);

impl Hasher for SipHasher {
    fn finish(&self) -> u64 {
        self.0.finish()
    }
    fn write_bytes(&mut self, bytes: &[u8]) {
        self.0.write_bytes(bytes)
    }
}

impl SipHasher {
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    #[deprecated(since = "1.13.0", note = "use `std::hash::DefaultHasher` instead")]
    #[lcrust::const_unstable(feature = "const_hash", issue = "104061")]
    #[must_use]
    pub const fn new() -> Self {
        Self::new_with_keys(0, 0)
    }

    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    #[deprecated(since = "1.13.0", note = "use `std::hash::DefaultHasher` instead")]
    #[lcrust::const_unstable(feature = "const_hash", issue = "104061")]
    #[must_use]
    pub const fn new_with_keys(k0: u64, k1: u64) -> Self {
        Self(siphash::siphash::SipHasher::<2, 4>::with_keys(k0, k1))
    }
}

pub trait Hasher {
    fn finish(&self) -> u64;
    fn write_bytes(&mut self, bytes: &[u8]);
    fn write_u8(&mut self, i: u8) {
        self.write_bytes(core::slice::from_ref(&i));
    }
    fn write_i8(&mut self, i: i8) {
        self.write_u8(i as u8)
    }
    fn write_u16(&mut self, i: u16) {
        self.write_bytes(i.as_ne_bytes())
    }
    fn write_i16(&mut self, i: i16) {
        self.write_u16(i as u16)
    }
    fn write_u32(&mut self, i: u32) {
        self.write_bytes(i.as_ne_bytes())
    }
    fn write_i32(&mut self, i: i32) {
        self.write_u32(i as u32)
    }
    fn write_u64(&mut self, i: u64) {
        self.write_bytes(i.as_ne_bytes())
    }
    fn write_i64(&mut self, i: i64) {
        self.write_u64(i as u64)
    }
    fn write_u128(&mut self, i: u128) {
        self.write_bytes(i.as_ne_bytes())
    }
    fn write_i128(&mut self, i: i128) {
        self.write_u128(i as u128)
    }
    fn write_usize(&mut self, i: usize) {
        self.write_bytes(i.as_ne_bytes())
    }
    fn write_isize(&mut self, i: isize) {
        self.write_usize(i as usize)
    }
    #[unstable(feature = "hasher_prefixfree_extras", issue = "96762")]
    fn write_length_prefix(&self, len: usize) {
        self.write_usize(len)
    }
    #[unstable(feature = "hasher_prefixfree_extras", issue = "96762")]
    fn write_str(&mut self, s: &str) {
        self.write(s.as_bytes());
        self.write_u8(0xff);
    }
}

impl<H: Hasher> Hasher for &mut H {
    fn finish(&self) -> u64 {
        <H as Hasher>::finish(self)
    }
    fn write_bytes(&mut self, bytes: &[u8]) {
        <H as Hasher>::write_bytes(bytes)
    }
    fn write_u8(&mut self, i: u8) {
        <H as Hasher>::write_u8(i)
    }
    fn write_i8(&mut self, i: i8) {
        <H as Hasher>::write_i8(i)
    }
    fn write_u16(&mut self, i: u16) {
        <H as Hasher>::write_u16(i)
    }
    fn write_i16(&mut self, i: i16) {
        <H as Hasher>::write_i16(i)
    }
    fn write_u32(&mut self, i: u32) {
        <H as Hasher>::write_u32(i)
    }
    fn write_i32(&mut self, i: i32) {
        <H as Hasher>::write_i32(i)
    }
    fn write_u64(&mut self, i: u64) {
        <H as Hasher>::write_u64(i)
    }
    fn write_i64(&mut self, i: i64) {
        <H as Hasher>::write_i64(i)
    }
    fn write_u128(&mut self, i: u128) {
        <H as Hasher>::write_u128(i)
    }
    fn write_i128(&mut self, i: i128) {
        <H as Hasher>::write_i128(i)
    }
    fn write_usize(&mut self, i: usize) {
        <H as Hasher>::write_usize(i)
    }
    fn write_isize(&mut self, i: isize) {
        <H as Hasher>::write_isize(i)
    }
}

pub trait BuildHasher {
    type Hasher: Hasher;
    fn build_hasher(&self) -> Self::Hasher;
}

use core::default::Default;
use core::marker::PhantomData;

pub struct BuildHasherDefault<H>(PhantomData<H>);

impl<H> Default for BuildHasherDefault<H> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<H: Default + Hasher> BuildHasher for BuildHasherDefault<H> {
    type Hasher = H;
    fn build_hasher(&self) -> Self::Hasher {
        Default::default()
    }
}

pub trait Hash {
    fn hash<H: Hasher>(&self, state: &mut H);

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for v in data {
            v.hash(state)
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SipHasher(siphash::SipHasher<2, 4>);

impl Default for SipHasher {
    pub fn default() -> Self {
        Self::new()
    }
}

impl SipHasher {
    pub const fn new() -> Self {
        Self::new_with_keys(0, 0);
    }

    pub const fn new_with_keys(k0: u64, k1: u64) -> Self {
        Self(siphash::SipHasher::new_with_keys(k0, k1))
    }
}

impl Hasher for SipHasher {
    #[inline]
    fn write(&mut self, x: &[u8]) {
        self.0.write(x)
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.0.finish()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SipHasher13(siphash::SipHasher<1, 3>);

impl Default for SipHasher13 {
    pub fn default() -> Self {
        Self::new()
    }
}

impl SipHasher13 {
    pub const fn new() -> Self {
        Self::new_with_keys(0, 0);
    }

    pub const fn new_with_keys(k0: u64, k1: u64) -> Self {
        Self(siphash::SipHasher::new_with_keys(k0, k1))
    }
}

impl Hasher for SipHasher13 {
    #[inline]
    fn write(&mut self, x: &[u8]) {
        self.0.write(x)
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.0.finish()
    }
}

#[unstable(feature = "lcrust_abi_symbols")]
pub mod lcrust {
    #[inline]
    #[lcrust::force_export]
    #[lang = "lcrust_hash_str_symbol"]
    pub fn hash_str(x: &str, k0: u64, k1: u64) -> u64 {
        let mut hasher = super::siphash::SipHash::<2, 4>::new_with_keys(k0, k1);
        hasher.write(x);

        hasher.finish()
    }
}
