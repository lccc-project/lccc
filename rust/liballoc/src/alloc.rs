
pub use ::core::alloc::*;


pub unsafe trait GlobalAlloc{
    unsafe fn alloc(&self,layout: Layout) -> *mut u8;
    unsafe fn dealloc(&self,ptr: *mut u8,layout: Layout);
    unsafe fn alloc_zeroed(&self,layout: Layout) -> *mut u8{
        let bytes = self.alloc(layout);
        ::__lccc::intrinsics::memset(bytes,0,layout.size());
    }
    
}
