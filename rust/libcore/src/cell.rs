use crate::marker::{Sized, Sync};
use crate::ops::CoereceUnsized;

#[repr(transparent)]
#[lang = "unsafe_cell"]
pub struct UnsafeCell<T: ?Sized>{
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
        self as *const Self as *mut T
    }

    #[unstable(feature = "unsafe_cell_raw_get", issue = "66358")]
    pub const fn get_raw(this: *const Self) -> *mut T{
        this as *mut T
    }
}

impl<T: ?Sized> !Sync for UnsafeCell<T>{}

impl<T: CoereceUnsized<U>,U> CoereceUnsized<UnsafeCell<U>> for UnsafeCell<T>{}
