#![allow(non_camel_case_types)]

#[cfg(doc)]
mod unspecified {
    pub struct unspecified_int;
    pub struct unspecified_uint;
}

cfg_match::cfg_match! {
    doc => #[doc(no_inline)] pub type uintptr = unspecified::unspecified_uint;
    target_pointer_width = "16" => #[doc = ""] pub type uintptr = u16;
    target_pointer_width = "32" => #[doc = ""] pub type uintptr = u32;
    target_pointer_width = "64" => #[doc = ""] pub type uintptr = u64;
    target_pointer_width = "128" => #[doc = ""] pub type uintptr = u128;
}

cfg_match::cfg_match! {
    doc => #[doc(no_inline)] pub type size_t = unspecified::unspecified_uint;
    target_pointer_width = "16" => #[doc = ""] pub type size_t = u16;
    target_pointer_width = "32" => #[doc = ""] pub type size_t = u32;
    target_pointer_width = "64" => #[doc = ""] pub type size_t = u64;
    target_pointer_width = "128" => #[doc = ""] pub type size_t = u128;
}

cfg_match::cfg_match! {
    doc => #[doc(no_inline)] pub type intptr = unspecified::unspecified_int;
    target_pointer_width = "16" => #[doc = ""] pub type intptr = i16;
    target_pointer_width = "32" => #[doc = ""] pub type intptr = i32;
    target_pointer_width = "64" => #[doc = ""] pub type intptr = i64;
    target_pointer_width = "128" => #[doc = ""] pub type intptr = i128;
}

cfg_match::cfg_match! {
    doc => #[doc(no_inline)] pub type ptrdiff_t = unspecified::unspecified_int;
    target_pointer_width = "16" => #[doc = ""] pub type ptrdiff_t = i16;
    target_pointer_width = "32" => #[doc = ""] pub type ptrdiff_t = i32;
    target_pointer_width = "64" => #[doc = ""] pub type ptrdiff_t = i64;
    target_pointer_width = "128" => #[doc = ""] pub type ptrdiff_t = i128;
}

/// A type that has the maximum fundamental alignment
#[repr(C, align(16))]
pub struct max_align_t(u128);
