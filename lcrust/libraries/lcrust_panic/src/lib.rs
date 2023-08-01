#![no_std]
#![feature(lcrust_attributes, thread_local)]
#![lcrust::mangle_as = "std"]

extern crate alloc;

pub(crate) mod rt;

#[cfg(feature="unwind")]
pub mod panic {
    pub mod foreign;
}

pub mod panicking {
    pub mod lcrust;
}

