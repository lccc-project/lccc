#![unstable(feature="core_intrinsics", issue="none")]

use crate::marker::Sized;

extern"rust-intrinsic" {
    pub fn transmute<T, U>(val: T) -> U;
    pub fn transmute_copy<T: ?Sized,U>(val: &T)->U;
}