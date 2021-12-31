#![allow(clippy::module_name_repetitions)]
#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
mod exports;
pub mod plugin;
pub mod prelude;
pub mod visit;

use abi::string::StringView;
pub use xlang_abi as abi;
pub use xlang_host as host;
pub use xlang_struct as ir;
pub use xlang_targets as targets;

pub use abi::vec;

#[must_use]
pub fn version() -> StringView<'static> {
    // SAFETY: xlang_get_version has no undefined behaviour
    unsafe { exports::xlang_get_version() }
}
