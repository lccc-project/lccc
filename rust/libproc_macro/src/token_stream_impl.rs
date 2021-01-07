use alloc::alloc::Allocator;

#[derive(Copy, Clone)]
pub struct CXXAllocator;

struct NoThrowT {
    _padding: ::core::mem::MaybeUninit<u8>,
}

const no_throw: NoThrowT = NoThrow {
    _padding: MaybeUninit::uninit(),
};

extern "C" {

    pub fn _ZnwPvmRCSt6no_throw_t(sz: usize, nothrow: &NoThrowT) -> *mut core::ffi::c_void;
    pub fn _ZnwPvmmRCSt6no_throw_t(
        sz: usize,
        align: usize,
        nothrow: &NoThrowT,
    ) -> *mut core::ffi::c_void;
    pub fn _ZdlvPv(p: *mut core::ffi::c_void);
}

impl Allocator for CXXAllocator {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        if layout.size() == 0 {
            Ok(layout.dangling().cast::<[u8; 0]>())
        } else if layout.align() <= core::mem::align_of::<::__lccc::platform::max_align_t>() {
            // SAFETY:
            // The C++ Standard Library prescribes no undefined behaviour for any global replaceable operator new
            // This is nothrow, so no exceptions are propagated through it.
            // The Required Behaviour is not exempt from the C++11 concurrency requirements, so &no_throw is not modified
            let ptr = unsafe { _ZnwPvmRCSt6no_throw_t(layout.size(), &no_throw) };
            if ptr.is_null() {
                Err(AllocError)
            } else {
                Ok(unsafe {
                    NonNull::new_unchecked(core::ptr::slice_from_raw_parts(
                        ptr as *mut u8,
                        layout.size(),
                    ))
                })
            }
        } else {
            // SAFETY:
            // The C++ Standard Library prescribes no undefined behaviour for any global replaceable operator new
            // This is nothrow, so no exceptions are propagated through it.
            // The Required Behaviour is not exempt from the C++11 concurrency requirements, so &no_throw is not modified
            let ptr = unsafe { _ZnwPvmmRCSt6no_throw_t(layout.size(), layout.align(), &no_throw) };
            if ptr.is_null() {
                Err(AllocError)
            } else {
                Ok(unsafe {
                    NonNull::new_unchecked(core::ptr::slice_from_raw_parts(
                        ptr as *mut u8,
                        layout.size(),
                    ))
                })
            }
        }
    }
}
