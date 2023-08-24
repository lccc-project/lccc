use super::*;

#[derive(Copy, Clone, Debug)]
pub struct SipHashState(u64, u64, u64, u64);

impl SipHashState {
    #[inline]
    pub fn from_keys(k0: u64, k1: u64) -> Self {
        Self(
            k0 ^ SIPHASH_MAG1,
            k1 ^ SIPHASH_MAG2,
            k0 ^ SIPHASH_MAG3,
            k1 ^ SIPHASH_MAG4,
        )
    }

    #[inline]
    fn halfround(
        mut v0: u64,
        mut v1: u64,
        mut v2: u64,
        mut v3: u64,
        rotate1: u32,
        rotate2: u32,
    ) -> (u64, u64, u64, u64) {
        v0 = v0.wrapping_add(v1);
        v2 = v2.wrapping_add(v3);
        v1 = v1.rotate_left(rotate1) ^ v0;
        v3 = v3.rotate_left(rotate2) ^ v2;

        v0 = v0.rotate_left(32);

        (v2, v1, v0, v3)
    }

    #[inline]
    pub fn round(&mut self) {
        let (v2, v1, v0, v3) = Self::halfround(self.0, self.1, self.2, self.3, 13, 16);
        (self.0, self.1, self.2, self.3) = Self::halfround(v2, v1, v0, v3, 17, 21);
    }

    #[inline]
    pub fn update_before_rounds(&mut self, word: u64) {
        self.3 ^= word;
    }

    #[inline]
    pub fn update_after_rounds(&mut self, word: u64) {
        self.0 ^= word;
    }

    #[inline]
    pub fn update_before_final(&mut self) {
        self.2 ^= 0xff;
    }

    #[inline]
    pub fn finish(&self) -> u64 {
        self.0 ^ self.1 ^ self.2 ^ self.3
    }
}
