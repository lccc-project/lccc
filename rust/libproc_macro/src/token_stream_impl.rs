
pub struct CXXAllocator;

extern"C"{
    pub fn _ZnwPvmm(sz: usize,align: usize) -> *mut core::ffi::c_void;
    pub fn _ZdlvPv(p: *mut core::ffi::c_void);
}




