/**
 * rust/liballoc/alloc.rs
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
#![allow(lccc::pendantic)]
#![feature(lccc_intrinsic_crate)]
#![feature(allocator_api, lccc_stability_attributes)]
#![feature(lccc_unique_ptr)]
#![feature(lccc_lang_items,lccc_deref_patterns)]
#![feature(slice_ptr_get, slice_ptr_len)]
#![feature(fn_traits, unboxed_closures)]
#![no_std]
#![__lccc::mangle_as("5alloc")]

extern crate libcore as core;

extern crate self as alloc;

pub mod alloc;
pub mod boxed;
pub mod rc;
pub mod vec;
