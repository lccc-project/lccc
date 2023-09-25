#![no_std]
#![feature(lcrust_attributes, thread_local, unchecked_math)]
#![cfg_attr(define_lang_items, lcrust_lang_items)]
#![lcrust::mangle_as = "std"]

extern crate alloc;

pub(crate) mod rt;

#[cfg(feature = "unwind")]
pub mod panic {
    pub fn always_abort() {}
    pub mod foreign;
}

pub mod panicking {
    pub mod lcrust;
}
