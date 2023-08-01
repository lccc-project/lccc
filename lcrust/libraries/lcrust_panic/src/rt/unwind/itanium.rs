type _UnwindReasonCode = i32;

const _URC_NO_REASON: _UnwindReasonCode = 0;
const _URC_FOREIGN_EXCEPTION_CAUGHT: _UnwindReasonCode = 1;
const _URC_FATAL_PHASE2_ERROR: _UnwindReasonCode = 2;
const _URC_FATAL_PHASE1_ERROR: _UnwindReasonCode = 3;
const _URC_NORMAL_STOP: _UnwindReasonCode = 4;
const _URC_END_OF_STACK: _UnwindReasonCode = 5;
const _URC_HANDLER_FOUND: _UnwindReasonCode = 6;
const _URC_INSTALL_CONTEXT: _UnwindReasonCode = 7;
const _URC_CONTINUE_UNWIND: _UnwindReasonCode = 8;

// Opaque
#[repr(C)]
pub struct _UnwindContet([(); 0]);

pub const EXCEPT_CLASS: u64 = u64::from_le_bytes(*b"LCRSLCCC");

#[repr(C)]
pub struct _UnwindException {
    pub cl: u64,
    pub exception_cleanup: Option<unsafe extern "C" fn(_UnwindReasonCode, *mut _UnwindException)>,
    pub private1: u64,
    pub private2: u64,
}

#[repr(lcrust_v0)]
struct AnyVtable {
    size: usize,
    align: usize,
    destructor: Option<unsafe extern "lcrust-v0" fn(*mut ())>,
    reserved_dealloc: Option<unsafe extern "lcrust-v0" fn(*mut ())>,
    typeid: unsafe extern "lcrust-v0" fn(*mut ()) -> core::any::TypeId,
}

unsafe extern "C" fn cleanup_exception(x: _UnwindReasonCode, except: *mut _UnwindException) {
    let except = except.cast::<PanicUnwindInfo>();

    let vt = (*except).vtable;

    let vt = core::mem::transmute::<&'static AnyVtable>(vt);

    let meta_end = except.cast::<u8>().add((*except).tail_size);

    let align = vt.align;

    let obj_start = meta_end.map_addr(|addr| (addr + align.sub_unchecked(1)) & !align);

    if let Some(dtor) = vt.destructor {
        dtor(obj_start.cast());
    }

    crate::panicking::lcrust::__deallocate_exception(except);
}

pub type ForeignExceptionType = _UnwindException;

#[repr(C)]
pub struct PanicUnwindInfo {
    pub except: _UnwindException,
    pub abi_ver: u64,
    pub panic_origin: Location<'static>,
    pub message: Option<String>,
    pub impl_info: usize,
    pub vtable: DynMetadata<dyn Any>,
    pub tail_size: usize,
}

fn init_panic_info(x: &mut PanicUnwindInfo) {
    x.except.cl = EXCEPT_CLASS;
    x.except.exception_cleanup = Some(cleanup_exception);
    x.abi_ver = env_int!("LCRUST_ABI_VERSION");
    x.tail_size = 0;
    x.impl_info = 0;
}

extern "C" {
    pub fn _Unwind_RaiseException(except: *mut _Unwind_Exception) -> _UnwindReasonCode;
}

#[track_caller]
#[inline(always)]
pub unsafe fn __begin_unwind(p: *mut PanicUnwindInfo) -> ! {
    init_panic_info(&mut *p);

    _Unwind_RaiseException(p.cast());

    crate::panicking::lcrust::abort()
}
