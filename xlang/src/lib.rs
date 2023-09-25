#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions, clippy::needless_borrow)]

mod exports;
pub mod plugin;
pub mod prelude;
pub mod visit;

use std::cmp::Ordering;
use std::str::FromStr;

use abi::string::StringView;
pub use xlang_abi as abi;
pub use xlang_host as host;
pub use xlang_struct as ir;
pub use xlang_targets as targets;

pub use abi::vec;

#[repr(transparent)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct XLangVersion<'a>(StringView<'a>);

impl<'a> XLangVersion<'a> {
    /// Constructs a version from it's string representation
    ///
    /// Functions on this type may have unpredictable results if this is not a string consisting solely of period separated integers
    #[must_use]
    pub const fn from_string(str: StringView<'a>) -> Self {
        Self(str)
    }

    #[must_use]
    pub const fn version_string(&self) -> StringView {
        self.0
    }
}

impl<'a> PartialOrd for XLangVersion<'a> {
    fn partial_cmp(&self, other: &XLangVersion) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for XLangVersion<'a> {
    fn cmp(&self, other: &XLangVersion) -> Ordering {
        let lhs_components = self
            .0
            .split('.')
            .map(u64::from_str)
            .chain(core::iter::repeat(Ok(0)));
        let rhs_components = other
            .0
            .split('.')
            .map(u64::from_str)
            .chain(core::iter::repeat(Ok(0)));

        for (l, r) in lhs_components.zip(rhs_components) {
            match l.unwrap_or(0).cmp(&r.unwrap_or(0)) {
                Ordering::Equal => {}
                ord => return ord,
            }
        }

        unreachable!("Infinite iterators should not break from for loop")
    }
}

impl<'a> core::fmt::Display for XLangVersion<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.0.fmt(f)
    }
}

#[must_use]
pub fn gen_rand() -> u64 {
    // SAFETY: xlang_gen_rand prescribes no undefined behaviour
    unsafe { exports::xlang_gen_rand() }
}

#[must_use]
pub fn version() -> XLangVersion<'static> {
    // SAFETY: xlang_get_version has no undefined behaviour
    XLangVersion::from_string(unsafe { exports::xlang_get_version() })
}

#[macro_export]
macro_rules! plugin_abi_version {
    ($ver:literal) => {
        #[used]
        #[no_mangle]
        static XLANG_PLUGIN_VERSION: $crate::XLangVersion =
            $crate::XLangVersion::from_string($crate::abi::const_sv!($ver));
    };
}
