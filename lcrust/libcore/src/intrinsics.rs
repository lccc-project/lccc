#![cfg_attr(feature="enable_stability_attributes",unstable(feature="core_intrinsics", issue="none"))]


extern"rust-intrinsic" {
    #[rustc_const_unstable(feature="lcrust_const_transmute",issue="none")]
    pub fn transmute<T, U>(val: T) -> U;
}