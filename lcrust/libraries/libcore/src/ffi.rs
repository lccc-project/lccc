/**
 * rust/libcore/ffi.rs
 * This file is part of lcrust standard libraries, a part of the lccc project
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * Like all libraries as part of the lccc project,
 *  the lcrust standard libraries are additionally dual licensed under the terms of the MIT and Apache v2 license. 
 * When dealing in this software, you may, at your option, do so under only those terms,
 *  or only under the terms of the GNU Lesser General Public License, or under both sets of terms. 
 */

#[__lccc::type_mangle_as("v")]
#[repr(i8)]
pub enum c_void {
    #[unstable(feature = "lccc_c_void_internal_field", issue = "none")]
    #[doc(hidden)]
    __lccc_c_void_internal_field = 0xFF,
    #[unstable(feature = "lccc_c_void_internal_field_2", issue = "none")]
    #[doc(hidden)]
    __lccc_c_void_internal_field_2 = 0,
}


mod c_types{
    macro_rules! c_types{
        [
            $(($c_type:ident: $cfg_name:ident => $($bit_size:literal: $ty:ty),* $(,)?)),* $(,)?
        ] => {
            $(
                $(#[cfg($cfg_name = ::core::stringify!($bit_size))] pub type $c_type = $ty;)*
    
                #[cfg(not(any($($cfg_name = ::core::stringify!($bit_size))*)))] ::core::compile_error!("Cannot provided definition for ",::core::stringify!($c_type)," on this platform");
            )*
        }
    }
    
    #[cfg(target_char_signedness = "signed")]
    pub type c_char = i8;
    
    #[cfg(not(target_char_signedness = "signed"))]
    pub type c_char = u8;

    // lccc assumes float and double are IEEE-754 binary32 and binary64 respectively, so these definitions are correct everywhere
    pub type c_float = f32;

    pub type c_double = f64;
    
    c_types![
        (c_short: target_short_width => 16: i16, 32: i32, 64: i64, 128: i128),
        (c_int: target_int_width => 16: i16, 32: i32, 64: i64, 128: i128),
        (c_long: target_long_width => 32: i32, 64: i64, 128: i128),
        (c_longlong: target_llong_wdith => 64: i64, 128: i128),
        (c_ushort: target_short_width => 16: u16, 32: u32, 64: u64, 128: u128),
        (c_uint: target_int_width => 16: u16, 32: u32, 64: u64, 128: u128),
        (c_ulong: target_long_width => 32: u32, 64: u64, 128: u128),
        (c_ulonglong: target_llong_wdith => 64: u64, 128: u128),
        (c_ptrdiff_t: target_size_width => 16: i16, 32: i32, 64: i64, 128: i128),
        (c_size_t: target_size_width => 16: u16, 32: u32, 64: u64, 128: u128),
    ]

    macro_rules! gib_size{
        ($c_type:ident => $rsize_ty:ty | $($bit_size:literal: $ty:ty),* $(,)?) => {
            #[cfg(any($(all(target_pointer_width = ::core::stringify!($bit_size),target_size_width = ::core::stringify!($bit_size))),*))]
                pub type $c_type = $rsize_ty;
            
            #[cfg(any($(all(target_pointer_width = ::core::stringify!($bit_size),target_size_width = ::core::stringify!($bit_size))),*))]
            {
                $(#[cfg(taget_size_width = ::core::stringify!($bit_size))] pub type $c_type = $ty;)*
            }
        }
    }

    gib_size!(c_size_t => usize | 16: u16, 32: u32, 64: u64, 128: u128);
    gib_size!(c_ptrdiff_t => isize | 16: i16, 32: i32, 64: i64, 128: i128);
}

#[unstable(feature = "core_ffi_c",issue="rust-lang#94501")]
pub use c_types::*;