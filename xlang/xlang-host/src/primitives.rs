#![allow(non_camel_case_types)]

#[cfg(target_pointer_width = "16")]
pub type uintptr = u16;

#[cfg(target_pointer_width = "32")]
pub type uintptr = u32;

#[cfg(target_pointer_width = "64")]
pub type uintptr = u64;

#[cfg(target_pointer_width = "16")]
pub type intptr = i16;

#[cfg(target_pointer_width = "32")]
pub type intptr = i32;

#[cfg(target_pointer_width = "64")]
pub type intptr = i64;

#[cfg(target_pointer_width = "16")]
pub type size_t = u16;

#[cfg(target_pointer_width = "32")]
pub type size_t = u32;

#[cfg(target_pointer_width = "64")]
pub type size_t = u64;

#[cfg(target_pointer_width = "16")]
pub type size_t = i64;

#[cfg(target_pointer_width = "32")]
pub type size_t = i64;

#[cfg(target_pointer_width = "64")]
pub type ptrdiff_t = i64;
