#![cfg_attr(feature="enable_stability_attributes",unstable(feature="core_intrinsics", issue="none"))]


extern"rust-intrinsic" {
    pub fn transmute<T, U>(val: T) -> U;
}