
pub use crate::intrinsics::{self,size_of};

#[rustc_const_unstable(feature="lccc_const_zeroed",issue="none")]
pub unsafe fn zeroed<T>()->T{
    intrinsics::panic_if_uninhabited::<T>();
    intrinsics::init()
}


#[repr(transparent,no_niche)]
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
}
