#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery, missing_docs)]
#![allow(clippy::module_name_repetitions, clippy::needless_borrow)]
//! A crate that provides abi-safe and stable wrappers of language and library constructs that don't have fixed abi
//! This provides shims for otherwise unusable constructs that can validly be passed accross ABI bounderies.
//! All publically exposed types in this api have a stable layout and abi, that may only change on semver major versions
//! Thus, it is valid to use these types accross versions

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
/// Defines ABI safe equivalents for `&[T]` and `&mut [T]`
pub mod span;
/// Implementation of `std::string::String` that uses [`alloc::Allocator`], and is ABI safe.
/// Also defines an ABI safe equivalent for `&str` (but not `&mut str`)
pub mod string;
/// Implementation of `std::string::String` that uses [`alloc::Allocator`], and is ABI safe.
pub mod vec;
