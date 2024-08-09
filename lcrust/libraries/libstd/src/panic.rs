pub use core::panic::*;



fn box_erased<T>(x: T) -> *mut ! {
    Box::into_raw(Box::new(x)) as *mut !
}

fn unwrap_erased_unchecked<T>(p: *mut !) -> T {
    *Box::from_raw(p as *mut T)
}

pub fn catch_unwind<F: UnwindSafe + FnOnce() -> T>(f: F) -> Result<T, Box<dyn Any>> {
    let mut slot = MaybeUninit::new()
    let mut wrapper = ManuallyDrop::new(move || 
        unsafe{
            core::intrinsics::construct_in_place(
                slot.as_mut_ptr(), 
                FnOnce::call_once, 
                (f,),)
        }
    );
    let (raw, metadata) = (addr_of_mut!(wrapper) as *mut ManuallyDrop<dyn FnOnce()>).to_raw_parts();
    catch_unwind_erased(
        core::ptr::from_raw_parts_mut(raw, metadata),
        core::ptr::null_mut(),
    )
    .map(|()| unsafe{slot.assume_init()})
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
fn catch_unwind_erased(f: *mut dyn FnOnce(), _: *mut !) -> Result<(), Box<dyn Any>> {
    Ok(FnOnce::call_once_unsized(f, ()))
}


#[track_caller]
pub fn panic_any<M: 'static + Send + Sync>(m: M) -> !{

}