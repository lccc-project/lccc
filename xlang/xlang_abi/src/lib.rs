#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery, missing_docs)]
#![allow(
    clippy::module_name_repetitions,
    clippy::needless_borrow,
    clippy::missing_const_for_fn,
    clippy::type_repetition_in_bounds
)]
//! A crate that provides abi-safe and stable wrappers of language and library constructs that don't have fixed abi
//! This provides shims for otherwise unusable constructs that can validly be passed accross ABI bounderies.
//! All publically exposed types in this api have a stable layout and abi, that may only change on semver major versions
//! Thus, it is valid to use these types accross versions
//!
//! ## Interface
//! This crate relies on a number of special symbols, provided by an interface library, which is typically a cdylib (so that there is one copy at runtime).
//! This interface is typically provided by the `xlang_interface` crate (which is linked by using the `xlang` crate), otherwise, it must be provided and linked separately.
//!
//! The symbols that this crate expects the interface library to provide are considered unstable, but all start with either the `xlang_` or `XLANG_` prefix.
//!
//! The symbols are publically available in the module that makes primary of them.
//!
//! The current list is:
//! * [`alloc::xlang_allocate`]
//! * [`alloc::xlang_allocate_aligned`]
//! * [`alloc::xlang_deallocate`]
//! * [`alloc::xlang_deallocate_aligned`]
//! * [`alloc::xlang_on_allocation_failure`]
//! * [`hash::xlang_hash_bytes`]
//! * [`hash::XLANG_HASH_SEED`]
//!

/// Defines an ABI safe equivalent for [`std::sync::Mutex`] and [`std::sync::RwLock`] for all ypes
pub mod sync;

/// Defines an ABI safe equivalent for [`std::option::Option`] for all types.
pub mod option;

/// Defines an ABI safe interface for trait objects, by emulating the vtable and fat pointer
pub mod traits;

/// Try stuff
pub mod ops;

/// ABI Safe Interface for allocation. Also provides allocator functions that can be used accross modules (on windows, the system allocator cannot be safely used to do so)
pub mod alloc;
/// Implementation of `std::boxed::Box` that uses the [`alloc::Allocator`] trait, and is ABI safe for all allocators
pub mod boxed;
/// Implementation of various collections that use the [`alloc::Allocator`] trait and that have Stable ABI
pub mod collection;
/// Defines Hashers for xlang that perform consistent operations
pub mod hash;
mod internal;
/// Defines io traits and types that are ABI Safe and can be used with [`traits::DynRef`] (et. al)
pub mod io;
/// Defines a Pair type, which is an ABI safe two-tuple
pub mod pair;
/// prelude for `xlang_abi`
pub mod prelude;
/// Defines pointer types and interfaces that are abi safe.
pub mod ptr;
/// Defines an ABI safe equivalent to [`std::result::Result`].
pub mod result;
/// Helper traits for slices
pub mod slice;
/// Defines ABI safe equivalents for `&[T]` and `&mut [T]`
pub mod span;
/// Implementation of `std::string::String` that uses [`alloc::Allocator`], and is ABI safe.
/// Also defines an ABI safe equivalent for `&str` (but not `&mut str`)
pub mod string;
/// Utility functions
pub mod util;
/// Implementation of `std::string::String` that uses [`alloc::Allocator`], and is ABI safe.
pub mod vec;

pub use xlang_host::primitives;

pub use xlang_host::rustcall;
