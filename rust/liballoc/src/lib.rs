#![allow(lccc::pendantic)]
#![feature(lccc_intrinsic_crate)]
#![feature(allocator_api, lccc_stability_attributes)]
#![feature(lccc_unique_ptr)]
#![feature(lccc_lang_items)]
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
