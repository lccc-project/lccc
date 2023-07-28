//!
//! Library to describe the behaviours of targets in xlang
//! Most properties of targets are exposed via this interface, and plugins (including frontends and backends), as well as drivers, should query the properties in this library,
//! rather than maintaining their own list of properties.
//!
//! This library also provides an ABI safe version of the [`target_tuples::Target`] type.

#![deny(warnings, missing_docs, clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]

/// Module containing structures and functions to query the properties of xlang targets
pub mod properties;
