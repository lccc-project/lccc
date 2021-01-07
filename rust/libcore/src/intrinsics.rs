#![unstable(feature = "core_intrinsics", issue = "none")]

use crate::Sized;

extern "rust-intrinsic" {
    #[rustc_const_unstable(feature = "lccc_const_transmute", issue = "none")]
    pub fn transmute<T, U>(val: T) -> U;

    pub fn transmute_copy<T, U>(val: &T) -> U;

    #[rustc_const_stable(feature = "const_size_of", since = "1.40.0")]
    pub fn size_of<T>() -> usize;

    pub fn size_of_val<T: ?Sized>(v: &T) -> usize;

    #[rustc_const_unstable(feature = "lccc_const_zeroed", issue = "none")]
    pub fn init<T>() -> T;

    #[rustc_const_unstable(feature = "lccc_const_zeroed", issue = "none")]
    pub fn panic_if_uninhabited<T>();

    #[rustc_const_unstable(feature = "lccc_const_align_of", issue = "none")]
    pub fn align_of<T>() -> usize;

    #[rustc_const_unstable(feature = "lccc_const_align_of", issue = "none")]
    pub fn align_of_val<T: ?Sized>(val: &T) -> usize;

    pub fn read<T>(ptr: *const T) -> T;
    pub fn write<T>(ptr: *mut T, val: T);

    pub fn read_volatile<T>(ptr: *const T) -> T;
    pub fn write_volatile<T>(ptr: *mut T, val: T);

    pub fn copy<T>(x: *const T, y: *mut T, v: usize);
    pub fn copy_nonoverlapping<T>(x: *const T, y: *mut T, v: usize);
}
