pub use core::panic::*;



fn box_erased<T>(x: T) -> *mut ! {
    Box::into_raw(Box::new(x)) as *mut !
}

fn unwrap_erased_unchecked<T>(p: *mut !) -> T {
    *Box::from_raw(p as *mut T)
}

pub fn catch_unwind<F: UnwindSafe + FnOnce() -> T>(f: F) -> Result<T, Box<dyn Any>> {
    let mut slot = MaybeUninit::new()
    let mut wrapper = ManuallyDrop::new(move || box_erased(f));
    catch_unwind_erased(
        core::mem::transmute(addr_of_mut!(wrapper) as *mut ManuallyDrop<dyn FnOnce() -> *mut !>),
        core::ptr::null_mut(),
    )
    .map(unwrap_erased_unchecked)
}

/**
 * ```
 * fn catch_unwind_personality(p: *mut ForeignExceptionType) -> _Unwind_Reason{
 *     SetRegister([param 1],p);
 *     SetIp(catch_unwind_landing_pad as *mut c_void);
 *     _URC_INSTALL_CONTEXT
 * }
 * ```
 */
#[lang = "lcrust_catch_unwind_fn"]
#[inline(never)]
fn catch_unwind_erased(f: *mut dyn FnOnce() -> *mut !, _: *mut !) -> Result<*mut !, Box<dyn Any>> {
    Ok(FnOnce::call_once_unsized(f, ()))
}


#[track_caller]
pub fn panic_any<M: 'static + Send + Sync>(m: M) -> !{

}