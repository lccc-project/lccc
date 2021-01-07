#![feature(lccc_intrinsic_crate)]
#![__lccc::mangle_as("10proc_macro")]

extern crate liballoc as alloc;
extern crate libcore as core;

mod token_stream_impl;
