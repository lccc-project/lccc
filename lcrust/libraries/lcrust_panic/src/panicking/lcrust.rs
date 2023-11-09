use core::fmt::FormatArgs;
use core::panic::Location;

extern "lcrust-v0" {
    #[lcrust::mangled_import]
    pub fn abort_fmt(panic_origin: &Location, args: FormatArgs) -> !;
    #[lcrust::mangled_import]
    pub fn abort_no_message() -> !;

    #[lcrust::mangled_import]
    #[lcrust::weak_import]
    pub static PANIC_ABORT_OVERRIDE: usize;
}

struct PrintOrDefault<'a, T>(Option<T>, &'a str);

impl<T: core::fmt::Display> core::fmt::Display for PrintOrDefault<'_, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if let Some(st) = &self.0 {
            st.fmt(f)
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

#[lcrust::weak_def]
pub fn panic_call_hook(_: &PanicInfo<'_>) {}

pub(crate) static ALWAYS_ABORT: AtomicUsize = AtomicUsize::new(0);

#[thread_local]
static PANIC_COUNT: Cell<usize> = Cell::new(0);

#[cfg_attr(define_lang_items, lang = "lcrust_increment_panic_count_symbol")]
pub unsafe fn increment_panic_count() -> usize {
    PANIC_COUNT.replace(PANIC_COUNT.get().add_unchecked(1))
}

#[cfg_attr(define_lang_items, lang = "lcrust_decrement_panic_count_symbol")]
pub unsafe fn decrement_panic_count() -> usize {
    PANIC_COUNT.replace(PANIC_COUNT.get().sub_unchecked(1))
}

#[cfg_attr(define_lang_items, lang = "lcrust_panic_count_symbol")]
pub fn panic_count() -> usize {
    PANIC_COUNT.get()
}

#[cfg_attr(define_lang_items, lang = "lcrust_begin_panic_symbol")]
#[cfg_attr(define_lang_items, lcrust::weak_def)]
#[cfg_attr(not(any(define_lang_items, feature = "unwind")), panic_handler)]
#[cold]
#[inline(never)]
pub extern "lcrust-v0" fn begin_panic_abort(panic: &PanicInfo<'_>) -> ! {
    let location = panic.location.unwrap_or(Location::caller()); // This shows here but we don't want ABI to have future issues
    if increment_panic_count() == 0 {
        panic_call_hook(panic);

        abort(location, Some("Aborting after panic"))
    } else {
        abort(
            location,
            Some("Panic in panic runtime. Aborting immediately"),
        )
    }
}

#[cfg(feature = "unwind")]
include!("unwind.rs");
