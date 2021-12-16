#![allow(clippy::module_name_repetitions)]
#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
mod exports;
pub mod plugin;
pub mod prelude;
pub mod visit;

pub use xlang_abi as abi;

pub use abi::vec;
