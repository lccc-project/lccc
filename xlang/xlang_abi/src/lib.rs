#![allow(clippy::missing_safety_doc)] // FIXME: Remove this allow
pub mod option;
pub mod traits;

pub mod alloc;
pub mod boxed;
pub mod collection;
pub mod hash;
mod internal;
pub mod pair;
pub mod prelude;
pub mod ptr;
pub mod result;
pub mod span;
pub mod string;
pub mod vec;

pub use xlang_abi_macro::xlang_trait;
