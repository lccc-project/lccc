use crate::{Sized, Option, Copy};
use crate::mem::MaybeUninit;
use crate::Option::{Some, None};
use crate::clone::Clone;
use crate::convert::From;
use crate::marker::PhantomData;

pub use lccc::drop_in_place;

#[lang = "const_ptr"]
impl<T: ?Sized> *const T{
    pub const fn is_null(self)->bool{
        self as usize == 0
    }
}

#[lang = "mut_ptr"]
impl<T: ?Sized> *mut T{
    pub const fn is_null(self)->bool{
        self as usize == 0
    }
}

#[lccc_pointer_attributes(nonnull)]
#[repr(transparent)]
pub struct NonNull<T: ?Sized>{
    ptr: *const T
}

impl<T: ?Sized> Clone for NonNull<T>{
    fn clone(&self) -> Self {
        NonNull{ptr: self.ptr}
    }
}

impl<T: ?Sized> Copy for NonNull<T>{}

impl<T> NonNull<T>{
    pub const fn dangling() -> Self{
        NonNull{ptr: unsafe{crate::intrinsics::min_align_of::<T>()} as *const T}
    }
}

impl<T: ?Sized> NonNull<T>{
    pub const unsafe fn new_unchecked(ptr: *mut T) -> Self{
        NonNull{ptr}
    }
    pub fn new(ptr: *mut T) -> Option<Self>{
        if ptr.is_null(){
            Some(NonNull{ptr})
        }else{
            None
        }
    }
    pub fn as_ptr(self) -> *mut T{
        self.ptr as *mut _
    }

    pub unsafe fn as_ref(&self) -> &T{
        &*self.ptr
    }

    pub unsafe fn as_mut(&mut self) -> &mut T{
        &mut *(self.ptr as *mut _)
    }

    pub const fn cost<U>(self) -> NonNull<U>{
        NonNull{ptr: self.ptr as *const U}
    }
}

impl<T: ?Sized> From<&T> for NonNull<T>{
    fn from(t: &T) -> Self {
        NonNull{ptr: t}
    }
}

#[lccc::xlang_pointer_attributes(unique,aligned)]
#[repr(transparent)]
#[unstable(feature="lccc_unique_ptr")]
pub struct Unique<T: ?Sized>{
    ptr: NonNull<T>,
    data: PhantomData<T>
}

impl<T> Unique<T>{
    pub const fn dangling() -> Self{
        Self{ptr: NonNull::dangling(),data: PhantomData}
    }
}

impl<T: ?Sized> Unique<T>{
    pub const unsafe fn new_unchecked(ptr: *mut T) -> Unique<T>{
        Self{ptr: NonNull::new_unchecked(ptr),data: PhantomData}
    }
    pub unsafe fn new_unchecked_nullable(ptr: *mut T) -> Option<Unique<T>>{
        NonNull::new(ptr).map(|ptr|Self{ptr,data:PhantomData})
    }
}