use alloc::alloc::Global;
use core::ops::Deref;

pub struct Box<T: ?Sized,#[unstable(feature="allocator_api",issue="32838")] A = Global>{
    ptr: core::ptr::Unique<T>,
    alloc: A
}

impl<T: ?Sized> Deref for Box<T,_>{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe{&*self.ptr.as_ptr()}
    }
}
