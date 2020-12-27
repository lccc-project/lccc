pub use alloc::alloc::*;

mod internal{
    type c_size = ::__lccc::target::__size_t;
    extern"C"{
        pub fn malloc(size: c_size) -> *mut core::ffi::c_void;
        
        pub fn realloc(ptr: *mut core::ffi::c_void,size: c_size) -> *mut core::ffi::c_void;

        pub fn aligned_alloc(size: c_size,align: c_size) -> *mut core::ffi::c_void;

        pub fn free(ptr: *mut core::ffi::c_void);

    }

    struct __Malloc;

    unsafe impl GlobalAlloc for __Malloc{
        unsafe fn alloc(&self,layout: Layout) -> *mut u8{
            aligned_alloc(layout.size() as c_size,layout.align() as c_size) as *mut u8
        }
        unsafe fn alloc_zeroed(&self,layout: Layout) -> *mut u8{
            let ptr = self.alloc(layout);
            ::__lccc::buiiltins::C::__builtin_memset(ptr,0,layout.size())
            ptr
        }
        unsafe fn dealloc(&self,ptr: *mut u8,_: Layout){
            free(ptr as *mut _)
        }

        unsafe fn realloc(&self,ptr: *mut u8,old_layout: Layout,new_size: usize) -> *mut u8{
            if old_layout.align()>core::mem::align_of::<::__lccc::target::__max_align_t>(){
                let new = self.alloc(Layout::new_unchecked(new_size,old_layout.align()));
                ::__lccc::builtins::C::__builtin_memcpy(new,ptr,old_layout.size());
                self.dealloc(ptr,old_layout);
                new
            }else{
                realloc(ptr,new_size)
            }
        }
    }

    // Oh hey, _ZTVN5alloc5alloc11GlobalAllocINSt5alloc8internal8__MallocE

    #[__lccc::weak]
    #[no_mangle]
    #[__lccc::force_external]
    pub static _ZN5alloc5alloc29__lccc_rust_global_alloc_implRCu3dynIN5alloc5alloc11GlobalAllocE: &dyn GlobalAlloc = &__Malloc;
    
}
