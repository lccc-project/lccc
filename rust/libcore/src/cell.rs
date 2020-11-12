use crate::marker::{Sized, Sync};
use crate::ops::CoereceUnsized;

#[repr(transparent)]
pub struct UnsafeCell<T: ?Sized>{
    #[__lccc::xlang::field_attributes(mutable)]
    inner: T
}


impl<T> UnsafeCell<T>{
    pub const fn new(val: T) -> Self{
        Self{inner: val}
    }
    pub fn into_inner(self) -> T{
        self.inner
    }
}

impl<T: ?Sized> UnsafeCell<T>{
    pub const fn get(&self) -> *mut T{
        self as *const Self as *mut Self as *mut T
    }

    #[unstable(feature = "unsafe_cell_raw_get", issue = "66358")]
    pub const fn get_raw(this: *const Self) -> *mut T{
        this as *mut Self as *mut T
    }

    pub fn get_mut(&mut self) -> &mut T{
        &mut self.inner
    }
}

impl<T: ?Sized> !Sync for UnsafeCell<T>{}

impl<T: CoereceUnsized<U>,U> CoereceUnsized<UnsafeCell<U>> for UnsafeCell<T>{}


#[repr(transparent)]
pub struct Cell<T: ?Sized>{
    inner: UnsafeCell<T>
}

impl<T> Cell<T>{
    pub const fn new(x: T) -> Self{
        Self{inner: x}
    }

    pub fn set(&self,x: T){
        unsafe{crate::ptr::write(self.inner.get(),x)}
    }

    pub fn swap(&self,other: &Self){
        unsafe{crate::ptr::swap(self.inner.get(),other.inner.get())}
    }

    pub fn replace(&self,x: T) -> T{
        unsafe{
            let tmp = crate::ptr::read(self.inner.get());
            crate::ptr::write(self.inner.get(),x)
            tmp
        }
    }
}