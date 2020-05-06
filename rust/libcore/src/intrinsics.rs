#![cfg_attr(feature="enable_stability_attributes",unstable(feature="core_intrinsics", issue="none"))]


extern"rust-intrinsic" {
    #[rustc_const_unstable(feature="lccc_const_transmute",issue="none")]
    pub fn transmute<T, U>(val: T) -> U;

    #[rustc_const_stable(feature = "const_size_of", since = "1.40.0")]
    pub fn size_of<T>()->usize;

    #[rustc_const_unstable(feature="lccc_const_zeroed",issue="none")]
    pub fn init<T>()->T;

    #[rustc_const_unstable(feature="lccc_const_zeroed",issue="none")]
    pub fn panic_if_uninhabited<T>();
}