/**
 * rust/libcore/lib.rs
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

#![feature(lccc_intrinsic_crate)]
#![deny(__lccc::unprefixed_features)]
#![allow(__lccc::rustc_incomplete_features)]
#![feature(
    lang_items,
    intrinsics,
    no_core,
    auto_traits,
    negative_trait_bounds
)]
#![feature(const_fn, reciever_trait)]
#![feature(
    lccc_const_transmute,
    lccc_slice_layout,
    lccc_const_zeroed,
    lccc_slice_layout,
    lccc_unique_ptr
)]
#![feature(unsize, negative_impls, no_niche, untagged_unions, prelude_import)]
#![feature(lccc_borrowck_helpers, lccc_trait_object, lccc_lang_items)]
#![feature(fn_traits, cell_update, unboxed_closures)]
#![feature(lccc_stability_attributes, lccc_same_trait)]
#![feature(or_patterns)]
#![no_core]
#![__lccc::mangle_as("std")]
#![__lccc::abi_tag("lccc_stdlib")]

extern crate self as core;

#[prelude_import]
pub use prelude::v1::*;

mod bool;
mod unit;

pub mod any;
pub mod alloc;
pub mod borrow;
pub mod cell;
pub mod clone;
pub mod cmp;
pub mod convert;
pub mod default;
pub mod ffi;
pub mod hash;
pub mod hint;
pub mod intrinsics;
pub mod iter;
pub mod marker;
pub mod mem;
pub mod ops;
pub mod option;
pub mod pin;
pub mod prelude;
pub mod primitive;
pub mod ptr;
pub mod raw;
pub mod result;
pub mod slice;
pub mod sync;

#[__lccc::builtin_macro]
#[allow_internal_unstable(core_intrinsics)]
pub macro panic($($input:tt),*) {}

#[__lccc::builtin_macro]
pub macro env($name:lit) {}

#[__lccc::builtin_macro]
pub macro option_env($name:lit) {}
