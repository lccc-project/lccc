
#![feature(lccc_intrinsic_crate,lccc_stability_attributes)]

#![unstable(feature="lccc_stdos_crate",issue="none",reason="The stdos crate is an implementation detail of lccc, use the exposed interfaces in std instead")]

#![no_std]

extern crate alloc;

#[cfg(target_os="linux")]
pub mod linux;

#[cfg(target_os="phantom")]
pub mod phantom;
