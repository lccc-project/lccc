#![allow(non_camel_case_types)]

/// A type that has the maximum fundamental alignment
#[repr(C, align(16))]
pub struct max_align_t(u128);
