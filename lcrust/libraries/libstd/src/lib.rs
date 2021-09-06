/**
 * rust/libstd/lib.rs
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
#![feature(prelude_import,lccc_intrinsic_crate)]
#![__lccc::mangle_as("std")]
#![__lccc::abi_tag("lccc_stdlib")]

extern crate libcore as core;
extern crate liballoc as alloc;
extern crate self as std;



pub mod prelude;

#[prelude_import]
pub use std::prelude::v1::*;

pub mod os;

pub mod lazy;

pub mod thread;

mod rt;
