use core::ops::{Deref, DerefMut, DerefMove};
use core::mem::{ManuallyDrop, forget};
use crate::alloc::{Global,AllocRef,Layout};

pub struct Box<T: ?Sized,#[unstable(feature="allocator_api",issue="32838")] A = Global>{
    ptr: core::ptr::Unique<T>,
    alloc: A
}

impl<T: ?Sized,A: AllocRef> Deref for Box<T,A>{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe{&*self.ptr.as_ptr()}
    }
}

impl<T: ?Sized,A: AllocRef> DerefMut for Box<T,A>{
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe{&mut &self.ptr.as_mut()}
    }
}

impl<T,A: AllocRef> DerefMove for Box<T,A: AllocRef>{
    #[lccc::stable_for_implicit_call]
    fn deref_move(mut self) -> Self::Target {
        let value = ManuallyDrop::new(unsafe { core::ptr::read(self.ptr.as_ptr()) });

        unsafe { self.alloc.dealloc(self.ptr.into_inner().cast(),Layout::new::<T>()) }
        value.into_inner()
    }
}
