use core::hash::{siphash, BuildHasher, Hasher};

#[unstable(feature = "lccc_random_state_in_alloc")]
pub struct RandomState(u64, u64);

#[unstable(feature = "lccc_random_state_in_alloc")]
pub struct DefaultHasher(siphash::SipHash<2, 4>);

impl DefaultHasher {
    #[lcrust::const_unstable(feature = "const_hash")]
    #[must_use]
    pub const fn new() -> Self {
        // Rust libstd uses (0,0)
        // To be less predictable, we pick different constants
        // These are taken from SHA-512: the first 64 fractional bits of the square roots of the first 2 prime numbers, 2 and 3.
        Self::with_keys(0x6a09e667f3bcc908, 0xbb67ae8584caa73b)
    }

    const fn with_keys(k0: u64, k1: u64) -> Self {
        Self(siphash::SipHash::new_with_keys(k0, k1))
    }
}

impl Default for DefaultHasher {
    fn default() -> Self {
        Self::new()
    }
}

impl Hasher for DefaultHasher {}

impl BuildHasher for RandomState {
    type Hasher = DefaultHasher;
    fn build_hasher(&self) -> DefaultHasher {
        return DefaultHasher::with_keys(self.0, self.1);
    }
}
