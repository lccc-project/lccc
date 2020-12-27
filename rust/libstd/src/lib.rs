#![feature(prelude_import,lccc_intrinsic_crate)]
#![__lccc::mangle_as("St",needs_nested=false)]

extern crate libcore as core;
extern crate liballoc as alloc;
extern crate self as std;



pub mod prelude;

#[prelude_import]
pub use std::prelude::v1::*;

