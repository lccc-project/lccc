
use crate::Sized;
use crate::intrinsics;
use crate::default::Default;

#[rustc_const_unstable(feature="lccc_const_zeroed",issue="none")]
pub unsafe fn zeroed<T>()->T{
    ::__lccc::builtins::rust::panic_if_uninhabited::<T>();
    ::__lccc::builtins::rust::zeroed()
}


#[repr(transparent)]
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
    #[unstable(feature="maybe_uninit_uninit_array")]
    pub fn uninit_array<const LEN: usize>() -> [Self;LEN]{
        unsafe{MaybeUninit::uninit().assume_init()}
    }
    #[unstable(feature="internal_uninit_const")]
    pub const UNINIT: Self = Self::uninit();

    pub fn write(&mut self,val: T) -> &mut T{
        unsafe {
            core::ptr::write(self.as_mut_ptr(), val);
            self.get_mut()
        }
    }

    pub fn as_ptr(&self) -> *const T{
        self as *const Self as *const T
    }

    pub fn as_mut_ptr(&mut self) -> *mut T{
        self as *mut Self as *mut T
    }

    pub unsafe fn read(&self) -> T{
        core::ptr::read(self.as_ptr())
    }

    pub unsafe fn get_ref(&self) -> &T{
        &*self.as_ptr()
    }
    pub unsafe fn get_mut(&mut self) -> &mut T{
        &mut *self.as_mut_ptr()
    }

    #[unstable(feature="maybe_uninit_slice_assume_init")]
    pub unsafe fn slice_get_ref(slice: &[Self]) -> &[T]{
        core::mem::transmute(slice)
    }

    #[unstable(feature="maybe_uninit_slice_assume_init")]
    pub unsafe fn slice_get_mut(slice: &mut [Self]) ->&mut [T]{
        core::mem::transmute(slice)
    }

    #[unstable(feature="maybe_uninit_slice",issue="63569")]
    pub fn first_ptr(slice: &[Self]) -> *const T{
        slice.as_ptr() as *const T
    }

    #[unstable(feature="maybe_uninit_slice",issue="63569")]
    pub fn first_ptr_mut(slice: &mut [Self]) -> *mut T{
        slice.as_mut_ptr() as *mut T
    }
}

unsafe impl<T> TrivialDestruction for MaybeUninit<T>{}

#[repr(transparent)]
#[derive(Copy,Clone,PartialEq,Eq,PartialOrd,Ord)]
pub struct ManuallyDrop<T: ?Sized>{
    inner: __lccc::trivial::<T>
}

impl<T> ManuallyDrop<T>{
    pub const fn new(value: T) -> ManuallyDrop<T>{
        // SAFETY:
        // convert weak from T to trivial<T> is always defined.
        // The result cannot be an invalid value of trivial<T> because  it was a valid value of T.
        ManuallyDrop{inner: unsafe{__lccc::xir!("convert weak":[value]:[yield:__lccc::trivial::<T>])}}
        
    }
    pub const fn into_inner(slot: ManuallyDrop<T>) -> T{
        // SAFETY:
        // convert weak from trivial<T> to T is always defined.
        // The result cannot be an invalid value of T because it was a valid value of trivial<T>.
        unsafe{__lccc::xir!("convert weak":[slot.inner]:[yield: T])}
    }
    #[must_use = "if you don't need the value, use `ManuallyDrop::drop` instead"]
    pub unsafe fn take(slot: &mut ManuallyDrop<T>) -> T{
        core::ptr::read(slot as *mut _ as *mut T)
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

unsafe impl<T: ?Sized> TrivialDestruction for ManuallyDrop<T>{}



#[deprecated("use MaybeUninit instead")]
pub unsafe fn uninitialized<T>() -> T{
    __lccc::xir!("const undef uninit %0"::[yield: T])
}