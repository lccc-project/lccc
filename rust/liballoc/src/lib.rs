#![alloc(lccc::pendantic)]
#![feature(lccc_intrinsic_crate)]
#![feature(allocator_api,lccc_stability_attributes)]
#![feature(lccc_unique_ptr)]
#![feature(lccc_lang_items)]
#![feature(slice_ptr_get,slice_ptr_len)]
#![no_std]


extern crate libcore as core;

extern crate self as alloc;

pub mod alloc;
pub mod boxed;



