#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::missing_safety_doc)] // Safety Docs are on definition site.
use std::{alloc::Layout, hash::Hasher, sync::atomic::AtomicUsize};

use lccc_siphash::SipHasher;
use xlang_abi::{const_sv, span::Span, string::StringView};

static RAND_GEN: xlang_abi::sync::OnceCell<std::sync::Mutex<SipHasher<2, 4>>> =
    xlang_abi::sync::OnceCell::new();

static PTR: AtomicUsize = AtomicUsize::new(0);

static STEPS: [u64; 16] = [
    0x428a2f98d728ae22,
    0x7137449123ef65cd,
    0xb5c0fbcfec4d3b2f,
    0xe9b5dba58189dbbc,
    0x3956c25bf348b538,
    0x59f111f1b605d019,
    0x923f82a4af194f9b,
    0xab1c5ed5da6d8118,
    0xd807aa98a3030242,
    0x12835b0145706fbe,
    0x243185be4ee4b28c,
    0x550c7dc3d5ffb4e2,
    0x72be5d74f27b896f,
    0x80deb1fe3b1696b1,
    0x9bdc06a725c71235,
    0xc19bf174cf692694,
];

xlang_host::rustcall! {
    #[no_mangle]
    pub extern "rustcall" fn __xlang_driver_init_rng(key: u64){

        RAND_GEN.get_or_insert_with(||{
            let mut keygen = SipHasher::<2,4>::new_with_keys(key ^ 0x6a09e667f3bcc908, key ^ 0xbb67ae8584caa73b);
            keygen.write_u64(0x3c6ef372fe94f82b);
            keygen.write_u64(0xa54ff53a5f1d36f1);
            let k0 = keygen.finish();
            keygen.write_u64(0x510e527fade682d1);
            keygen.write_u64(0x9b05688c2b3e6c1f);
            let k1 = keygen.finish();

            let hasher = SipHasher::<2,4>::new_with_keys(k0 ^ 0x1f83d9abfb41bd6b, k1 ^ 0x5be0cd19137e2179);
            std::sync::Mutex::new(hasher)
        });
    }

}

xlang_host::rustcall! {
    #[no_mangle]
    pub extern "rustcall" fn xlang_gen_rand() -> u64 {
        let mut guard = RAND_GEN.get().unwrap().lock().unwrap();
        let val = PTR.fetch_add(1, std::sync::atomic::Ordering::Relaxed) & 15;

        guard.write_u64(STEPS[val]);

        guard.finish()
    }
}

#[no_mangle]
pub extern "C" fn xlang_allocate(size: usize) -> *mut core::ffi::c_void {
    if size == 0 {
        return 32usize as *mut core::ffi::c_void;
    }
    unsafe {
        xlang_allocate_aligned(
            size,
            if size > 32 {
                32
            } else {
                size.next_power_of_two()
            },
        )
    }
}

#[no_mangle]
pub unsafe extern "C" fn xlang_allocate_aligned(
    size: usize,
    align: usize,
) -> *mut core::ffi::c_void {
    if size == 0 {
        return align as *mut core::ffi::c_void;
    }
    let size = size + (align - size % align) % align;
    let layout = Layout::from_size_align_unchecked(size, align);
    std::alloc::alloc(layout).cast::<_>()
}

#[no_mangle]
pub unsafe extern "C" fn xlang_deallocate(ptr: *mut core::ffi::c_void, size: usize) {
    if size == 0 {
        return;
    }
    #[allow(unused_unsafe)]
    unsafe {
        xlang_deallocate_aligned(
            ptr,
            size,
            if size > 32 {
                32
            } else {
                size.next_power_of_two()
            },
        );
    }
}

#[no_mangle]
pub unsafe extern "C" fn xlang_deallocate_aligned(
    ptr: *mut core::ffi::c_void,
    size: usize,
    align: usize,
) {
    if size == 0 || ptr.is_null() {
        return;
    }
    let size = size + (align - size % align) % align;
    let layout = Layout::from_size_align_unchecked(size, align);
    std::alloc::dealloc(ptr.cast::<_>(), layout);
}

#[no_mangle]
pub extern "C" fn xlang_on_allocation_failure(size: usize, align: usize) -> ! {
    eprintln!(
        "Failed to allocate with size: {}, and alignment: {}",
        size, align
    );
    std::process::abort()
}

xlang_abi::rustcall! {
    #[no_mangle]
    #[allow(clippy::missing_const_for_fn)] // const extern fn is unstable
    pub extern "rustcall" fn xlang_get_version() -> StringView<'static> {
        const_sv!("0.1")
    }
}

#[no_mangle]
pub static XLANG_HASH_SEED: u8 = 254u8;

const PRIME: u64 = 1_099_511_628_211;

lazy_static::lazy_static! {
    static ref HASH_SEED_ACTUAL: u64 = xlang_gen_rand();
}

xlang_host::rustcall! {
    #[no_mangle]
    pub extern "rustcall" fn xlang_hash_bytes(bytes: Span<u8>) -> u64{
        let mut hash = *HASH_SEED_ACTUAL;

        for &byte in bytes{
            hash ^= u64::from(byte);
            hash = hash.wrapping_mul(PRIME);
        }
        hash
    }
}

#[no_mangle]
pub extern "C" fn xlang_yield_now() {
    std::thread::yield_now()
}
