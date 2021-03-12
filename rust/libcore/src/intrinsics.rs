/**
 * rust/libcore/intrsinics.rs
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

#![unstable(feature = "core_intrinsics", issue = "none")]


#[stable]
pub use ::__lccc::builtins::rust::transmute;
#[stable]
pub use ::__lccc::builtins::rust::transmute_copy;
#[stable]
pub use ::__lccc::builtins::rust::copy;
#[stable]
pub use ::__lccc::builtins::rust::copy_nonoverlapping;

pub use ::__lccc::builtins::C::__builtin_trap;
pub use ::__lccc::builtins::C::__builtin_trap as abort;


#[track_caller]
pub fn caller_location() -> &'static std::panicking::Location<'static>{
    ::__lccc::builtins::rust::__builtin_caller_location()
}