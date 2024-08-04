#![allow(non_camel_case_types)]

/// A type that has the maximum fundamental alignment
#[repr(C, align(16))]
#[allow(dead_code)]
pub struct max_align_t(u128);
