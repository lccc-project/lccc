#![unstable(feature = "lcrust_panicking_abi")]

use core::fmt::FormatArgs;
use core::panic::Location;

pub use lcrust_panic::panicking::lcrust::*;

#[lang = "lcrust_abort_no_message_symbol"]
pub fn abort_no_message() -> ! {
    self::process::abort()
}

#[lang = "lcrust_abort_fmt_symbol"]
pub fn abort_fmt(panic_origin: &Location, fmt_args: FormatArgs) -> ! {
    crate::eprintln!(
        "Program Exited with Panic at {}: {}",
        panic_origin,
        fmt_args
    );

    abort_no_message()
}
