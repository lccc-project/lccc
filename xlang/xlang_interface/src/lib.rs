#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::missing_safety_doc)] // FIXME: Remove this allow
use std::alloc::Layout;

use xlang_abi::{const_sv, span::Span, string::StringView};
use xlang_targets::properties::TargetProperties;

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

#[no_mangle]
pub extern "C" fn xlang_get_target_properties(
    targ: StringView,
) -> Option<&'static TargetProperties<'static>> {
    #[allow(deprecated)]
    xlang_targets::properties::__get_properties(targ)
}

#[no_mangle]
#[allow(clippy::missing_const_for_fn)] // const extern fn is unstable
pub extern "C" fn xlang_get_version() -> StringView<'static> {
    const_sv!("0.1")
}

#[no_mangle]
pub static XLANG_HASH_SEED: u8 = 254u8;

const PRIME: u64 = 1_099_511_628_211;

lazy_static::lazy_static! {
    static ref HASH_SEED_ACTUAL: u64 = rand::random::<u64>()^14_695_981_039_346_656_037;
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
