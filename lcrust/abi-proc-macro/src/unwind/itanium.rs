#![allow(non_camel_case_types)]

use std::os::raw::c_int;

pub type _Unwind_Reason_Code = c_int;

pub const _URC_NO_REASON: _Unwind_Reason_Code = 0;
pub const _URC_FOREIGN_EXCEPTION_CAUGHT: _Unwind_Reason_Code = 1;
pub const _URC_FATAL_PHASE2_ERROR: _Unwind_Reason_Code = 2;
pub const _URC_FATAL_PHASE1_ERROR: _Unwind_Reason_Code = 3;
pub const _URC_NORMAL_STOP: _Unwind_Reason_Code = 4;
pub const _URC_END_OF_STACK: _Unwind_Reason_Code = 5;
pub const _URC_HANDLER_FOUND: _Unwind_Reason_Code = 6;
pub const _URC_INSTALL_CONTEXT: _Unwind_Reason_Code = 7;
pub const _URC_CONTINUE_UNWIND: _Unwind_Reason_Code = 8;

#[repr(C)]
pub struct _Unwind_Exception {
    exception_class: u64,
    exception_cleanup: Option<unsafe extern "C" fn(_Unwind_Reason_Code, *mut _Unwind_Exception)>,
    private_1: u64,
    private_2: u64,
}

pub type __personality_routine =
    Option<unsafe extern "C" fn(version: c_int, action: _Unwind_Action)>;

extern "C" {
    pub fn _Unwind_RaiseException(x: *mut _Unwind_Exception) -> _Unwind_Reason_Code;
}
