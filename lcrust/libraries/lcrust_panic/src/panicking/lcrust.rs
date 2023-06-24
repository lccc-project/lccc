
use core::panic::Location;
use core::fmt::FormatArgs;

extern "lcrust-v0"{
    #[link_name = "_ZNSt9panicking5lcrust7abort_fmtERK_ZNSt5panic8LocationE_ZNSt3fmt10FormatArgsE"]
    #[lcrust::unsafe_import]
    pub fn abort_fmt(panic_origin: &Location, args: FormatArgs) -> !;
    #[link_name = "_ZNSt9panicking5lcrust14abort_no_messagev"]
    #[]
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

pub extern "lcrust-v0" fn abort(panic_origin: &Location, msg: Option<&str>) -> !{

    // SAFETY: std::panicking::lcrust::abort_fmt is a safe function
    unsafe{abort_fmt(panic_origin, format_args!("{}",PrintOrDefault(msg,"Explicit Panic")))}
}


#[cfg(feature="unwind")]
include!("unwind.rs");