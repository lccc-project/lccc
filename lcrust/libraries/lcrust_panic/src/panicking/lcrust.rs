use core::fmt::FormatArgs;
use core::panic::Location;

extern "lcrust-v0" {
    #[lcrust::mangled_import]
    pub fn abort_fmt(panic_origin: &Location, args: FormatArgs) -> !;
    #[lcrust::mangled_import]
    pub fn abort_no_message() -> !;
}

struct PrintOrDefault<'a>(Option<&'a str>, &'a str);

impl core::fmt::Display for PrintOrDefault<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if let Some(st) = self.0 {
            f.write_str(st)
        } else {
            f.write_str(self.1)
        }
    }
}

pub extern "lcrust-v0" fn abort(panic_origin: &Location, msg: Option<&str>) -> ! {
    // SAFETY: std::panicking::lcrust::abort_fmt is a safe function
    unsafe {
        abort_fmt(
            panic_origin,
            format_args!("{}", PrintOrDefault(msg, "Explicit Panic")),
        )
    }
}

#[cfg(feature = "unwind")]
include!("unwind.rs");
