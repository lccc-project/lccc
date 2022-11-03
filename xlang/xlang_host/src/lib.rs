#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery, missing_docs)]

#![cfg_attr(has_feature_c_unwind ="feature",feature(c_unwind))]

//!
//! The `xlang_host` crate provides access to the host target's apis, depending on the interfaces available from the OS
//! The definition is such that unknown operating systems that provide the required interfaces will function properly on xlang.

///
/// Interfaces to load and access dynamic libraries on the host, used for finding xlang plugins
pub mod dso;
/// Primitive types defined by the OS (such as the size type and the pointer-sized type).
pub mod primitives;


mod rustcall;

