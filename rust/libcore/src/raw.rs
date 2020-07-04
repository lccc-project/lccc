#![unstable(feature="raw",issue="27751")]

#[repr(C)]
#[derive(Copy,Clone)]
pub struct TraitObject{
    pub data: *mut (),
    pub vtable: *mut ()
}

#[unstable(feature="lccc_slice_layout")]
pub use crate::slice::RawSlice;