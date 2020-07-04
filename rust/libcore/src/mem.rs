
pub use crate::intrinsics::{size_of};
use crate::Sized;
use crate::intrinsics;
use crate::default::Default;

#[rustc_const_unstable(feature="lccc_const_zeroed",issue="none")]
pub unsafe fn zeroed<T>()->T{
    intrinsics::panic_if_uninhabited::<T>();
    intrinsics::init()
}


#[repr(transparent,no_niche)]
#[lang = "maybe_uninit"]
pub union MaybeUninit<T>{
    valid: T,
    uninit: ()
}

impl<T> MaybeUninit<T>{
    pub const fn new(x: T) -> Self{
        Self{valid: x}
    }
    pub const fn uninit() -> Self{
        Self{uninit: ()}
    }
    pub const fn zeroed() -> Self{
        unsafe{self::zeroed()}
    }
    pub unsafe fn assume_init(self) -> T{
        self.valid
    }

}

#[repr(transparent)]
#[lang = "manually_drop"]
#[derive(Copy,Clone,PartialEq,Eq,PartialOrd,Ord)]
pub struct ManuallyDrop<T: ?Sized>{
    inner: T
}

impl<T> ManuallyDrop<T>{
    pub const fn new(value: T) -> ManuallyDrop<T>{
        ManuallyDrop{inner: value}
    }
    pub const fn into_inner(slot: ManuallyDrop<T>) -> T{
        slot.inner
    }
    #[must_use = "if you don't need the value, use `ManuallyDrop::drop` instead"]
    pub unsafe fn take(slot: &mut ManuallyDrop<T>) -> T{
        intrinsics::read(slot as *mut _ as *mut T)
    }
}

impl<T: ?Sized> ManuallyDrop<T>{
    pub unsafe fn drop(slot: &mut ManuallyDrop<T>){
        crate::ptr::drop_in_place(slot as *mut _ as *mut T)
    }
}


pub fn forget<T>(x: T){
    ManuallyDrop::new(x);
}

pub fn take<T: Default>(r: &mut T)->T{
    replace(r,Default::default())
}

pub fn replace<T>(r: &mut T,val: T)->T{
    unsafe{
        let ret = intrinsics::read(r);
        intrinsics::write(r,val);
        ret
    }
}