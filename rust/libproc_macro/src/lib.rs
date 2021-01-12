// This library is garbage, and needs to chat with xlangrust.so
// Which is, of course, written in C++. 
#![feature(lccc_intrinsic_crate)]
#![__lccc::mangle_as("10proc_macro")]


extern crate liballoc as alloc;
extern crate libcore as core;
extern crate libstd as std;
extern crate self as proc_macro;

mod token_stream_impl;
mod span_impl;