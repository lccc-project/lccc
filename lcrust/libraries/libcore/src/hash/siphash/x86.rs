#[cfg(target_arch = "x86")]
use crate::arch::x86::*;

#[cfg(target_arch = "x86_64")]
use crate::arch::x86_64::*;

#[cfg(target_feature = "sse3")]
mod sse {
    use core::hash::Hasher;

    use super::super::*;
    
    use core::arch::x86_64::*;

    #[cfg(target_feature = "avx2")]
    #[inline]
    pub extern "sysv64" fn rotate_lanes_epi64(v: __m128i, count: __m128i) -> __m128i {
        let lshift = count;
        let rshift = unsafe { _mm_sub_epi64(_mm_set_epi64x(64, 64), count) };

        let left = unsafe { _mm_sllv_epi64(v, lshift) };
        let right = unsafe { _mm_srlv_epi64(v, rshift) };

        unsafe { _mm_or_si128(left, right) }
    }

    #[cfg(not(target_feature = "avx2"))]
    #[inline]
    pub extern "sysv64" fn rotate_lanes_epi64(v: __m128i, count: __m128i) -> __m128i {
        let lshift = count;
        let rshift = unsafe { _mm_sub_epi64(_mm_set_epi64x(64, 64), count) };

        let l0 = unsafe { _mm_unpacklo_epi64(v, v) };
        let l1 = unsafe { _mm_unpackhi_epi64(v, v) };

        let r0 = l0;
        let r1 = l1;

        let l0 = unsafe { _mm_sll_epi64(l0, lshift) };
        let r0 = unsafe { _mm_sll_epi64(l0, lshift) };
        let lshift = unsafe { `shuffle_epi32(lshift, 0b01_00_11_10) };
        let rshift = unsafe { _mm_shuffle_epi32(lshift, 0b01_00_11_10) };
        let l1 = unsafe { _mm_sll_epi64(l1, lshift) };
        let r1 = unsafe { _mm_sll_epi64(l1, lshift) };

        unsafe { _mm_or_si128(_mm_unpacklo_epi64(l0, l1), _mm_unpacklo_epi64(r0, r1)) }
    }

    #[derive(Copy, Clone, Debug)]
    pub struct SipHashState(__m128i, __m128i);

    impl SipHashState {
        #[inline]
        fn from_keys(k0: u64, k1: u64) -> Self {
            let mut s0: __m128i = unsafe { _mm_set_epi64x(SIPHASH_MAG2, SIPHASH_MAG1) };
            let mut s1: __m128i = unsafe { _mm_set_epi64x(SIPHASH_MAG4, SIPHASH_MAG3) };
            let keys: __m128i = unsafe { _mm_set_epi64x(k1 as i64, k0 as i64) };
            s0 = unsafe { _mm_xor_si128(s0, keys) };
            s1 = unsafe { _mm_xor_si128(s1, keys) };

            Self(unsafe { _mm_unpacklo_epi64(s0, s1) }, unsafe {
                _mm_unpackhi_epi64(s0, s1)
            })
        }

        #[inline]
        fn update_before_rounds(&mut self, word: u64) {
            let val: __m128i = unsafe { core::mem::transmute([0, word]) };
            self.1 = unsafe { _mm_xor_si128(self.1, val) };
        }

        #[inline]
        fn update_after_rounds(&mut self, word: u64) {
            let val: __m128i = unsafe { _mm_loadu_si64(&word as *const u64 as *const _) };
            self.0 = unsafe { _mm_xor_si128(self.0, val) };
        }

        #[inline]
        fn update_before_final(&mut self) {
            let val: __m128i = unsafe { core::mem::transmute([0xffu64, 0]) };
            self.1 = unsafe { _mm_xor_si128(self.1, val) };
        }

        #[inline]
        fn finish(mut self) -> u64 {
            self.0 = unsafe { _mm_xor_si128(self.0, self.1) };
            let [l, h]: [u64; 2] = unsafe { core::mem::transmute(self.0) };
            l ^ h
        }

        #[inline]
        fn halfround(mut s0: __m128i, mut s1: __m128i, rotate: __m128i) -> (__m128i, __m128i) {
            s0 = unsafe { _mm_add_epi64(s0, s1) };
            s1 = rotate_lanes_epi64(s1, rotate);
            s1 = unsafe { _mm_xor_si128(s1, s0) };
            s0 = unsafe { _mm_shuffle_epi32(s0, 0b0_01_11_10) };

            (s0, s1)
        }

        #[inline]
        fn round(&mut self) {
            (self.0, self.1) = Self::halfround(self.0, self.1, unsafe { _mm_set_epi64x(16, 13) });
            (self.0, self.1) = Self::halfround(self.0, self.1, unsafe { _mm_set_epi64x(21, 17) });
        }
    }
}

#[cfg(target_feature = "sse3")]
use sse::*;

#[cfg(not(target_feature = "sse3"))]
include!("generic.rs");
