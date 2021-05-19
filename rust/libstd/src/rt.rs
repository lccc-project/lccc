extern "Rust" {
    #[no_mangle]
    fn __lccc_main() -> i32;
}

#[repr(transparent)]
struct ArgWrapper(pub *mut crate::os::raw::c_char);
unsafe impl Send for ArgWrapper {}
unsafe impl Sync for ArgWrapper {}

pub(crate) static ARGS: crate::lazy::SyncLazy<&[ArgWrapper]> = crate::lazy::SyncLazy::new();
#[__lccc::visibility("default")]
#[__lccc::weak]
#[no_mangle]
pub unsafe extern "C" fn main(argc: i32, argv: *mut *mut crate::os::raw::c_char) -> i32 {
    //
    // Safety: Invariants are kept as prescribed by the C Standard.
    ARGS.set(unsafe { core::slice::from_raw_parts(argv as *mut _, argc) });

    //
    // Safety: __lccc_main() is defined by the `main` symbol, or the `#[main]` attribute
    // In both cases, the function is guaranteed not to be unsafe, and be rectified to the correct signature
    unsafe { __lccc_main() }
}
