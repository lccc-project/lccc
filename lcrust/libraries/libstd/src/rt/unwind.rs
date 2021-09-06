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

unsafe extern "C" fn cleanup_exception(x: _UnwindReasonCode, except: *mut _UnwindException) {
    drop(Box::from_raw(except))
}

#[repr(C)]
pub struct PanicUnwindInfo {
    pub except: _UnwindException,
    pub abi_ver: u64,
    pub panic_origin: Location<'static>,
    pub message: Option<String>,
    pub impl_info: [usize; 2],
}

fn init_panic_info(x: &mut PanicUnwindInfo) {
    x.except.cl = EXCEPT_CLASS;
    x.except.exception_cleanup = Some(cleanup_exception);
}
