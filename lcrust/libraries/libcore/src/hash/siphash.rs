const SIPHASH_MAG1: u64 = 0x736f6d6570736575;
const SIPHASH_MAG2: u64 = 0x646f72616e646f6d;
const SIPHASH_MAG3: u64 = 0x6c7967656e657261;
const SIPHASH_MAG4: u64 = 0x7465646279746573;

#[cfg_attr(target_arch = "clever", path = "siphash/clever.rs")]
#[cfg_attr(any(target_arch = "x86", target_arch = "x86_64")), path = "siphash/x86.rs"]
#[cfg_attr(not(any(target_arch = "clever", target_arch = "x86", target_arch = "x86_64")), path = "siphash/generic.rs")]
mod sys;

pub struct SipHasher<const C: usize, const D: usize>{state: SipHashState, tail: u64, ntail: usize};

impl<const C: usize, const D: usize> SipHasher<C, D> {
    pub fn new_with_keys(k0: u64, k1: u64) -> Self {
        Self(sys::SipHashState::from_keys(k0, k1))
    }

    fn update(&mut self, word: u64) {
        self.0.update_before_rounds(word);
        for _ in 0..C {
            self.0.round();
        }
        self.0.update_after_rounds(word);
    }
}

impl<const C: usize, const D: usize> Hasher for SipHasher<C, D> {
    fn write(&mut self, mut s: &[u8]) {


        let chunks_exact = s.chunks_exact(8);
        let remainder = chunks_exact.remainder();
        for c in chunks_exact {
            let word = u64::from_ne_bytes(unsafe { *(c as *const [u8] as *const [u8; 8]) });

            self.update(word);
        }

        if !remainder.is_empty() {
            let mut bytes = [0u8; 8];
            bytes[..remainder.len()].copy_from_slice(remainder);
            let word = u64::from_ne_bytes(bytes);
            self.update(word);
        }
    }

    fn finish(&self) -> u64 {
        let mut state = self.0;

        state.update_before_final();

        for _ in 0..D {
            state.round();
        }

        state.finish().to_le()
    }
}