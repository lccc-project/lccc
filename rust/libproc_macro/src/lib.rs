
#![feature(lccc_intrinsic_crate)]

#![__lccc::mangle_as("10proc_macro")]

extern crate libcore as core;
extern crate liballoc as alloc;

mod token_stream_impl;

