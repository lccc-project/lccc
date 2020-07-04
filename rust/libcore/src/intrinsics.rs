#![unstable(feature="core_intrinsics", issue="none")]


use crate::Sized;

extern"rust-intrinsic" {
    #[cfg_attr(feature="lccc_const_transmute",rustc_const_unstable(feature="lccc_const_transmute",issue="none"))]
    pub fn transmute<T, U>(val: T) -> U;

    #[rustc_const_stable(feature = "const_size_of", since = "1.40.0")]
    pub fn size_of<T>()->usize;

    #[rustc_const_unstable(feature="lccc_const_zeroed",issue="none")]
    pub fn init<T>()->T;

    #[rustc_const_unstable(feature="lccc_const_zeroed",issue="none")]
    pub fn panic_if_uninhabited<T>();

    #[rustc_const_unstable(feature="lccc_const_align_of",issue="none")]
    pub fn min_align_of<T>()->usize;

    #[rustc_const_unstable(feature="lccc_const_align_of",issue="none")]
    pub fn pref_align_of<T>()->usize;

    #[rustc_const_unstable(feature="lccc_const_align_of",issue="none")]
    pub fn min_align_of_val<T: ?Sized>(val: &T)->usize;

    #[rustc_const_unstable(feature="lccc_const_align_of",issue="none")]
    pub fn pref_align_of_val<T: ?Sized>(val: &T)->usize;

    pub fn unreachable()->!;

    pub fn read<T>(ptr: *const T) -> T;
    pub fn write<T>(ptr: *mut T,val: T);

    pub fn read_volatile<T>(ptr: *const T) -> T;
    pub fn write_volatile<T>(ptr: *mut T,val: T);
}