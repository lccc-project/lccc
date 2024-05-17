#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery, missing_docs)]
#![allow(unexpected_cfgs, unknown_lints)] // This is a dumbass lint we can't work arround in general

//!
//! The `xlang_host` crate provides access to the host target's apis, depending on the interfaces available from the OS
//! The definition is such that unknown operating systems that provide the required interfaces will function properly on xlang.

///
/// Interfaces to load and access dynamic libraries on the host, used for finding xlang plugins
pub mod dso;
/// Primitive types defined by the OS (such as the size type and the pointer-sized type).
pub mod primitives;

/// Synchronization Primitives, like `wait_by_address`
pub mod sync;

mod rustcall;
