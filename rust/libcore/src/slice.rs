use crate::intrinsics::transmute;


// lcrust implementation detail. Might open an RFC to make this part of rust
#[repr(C)]
#[doc(hidden)]
#[unstable(feature="lccc_slice_layout")]
pub struct RawSlice<T>{
    ptr: *mut T,
    len: usize
}

pub unsafe fn from_raw_parts<'a,T>(ptr: *const T,len: usize) -> &'a [T]{
    transmute(RawSlice{ptr: ptr as *mut T,len})
}

pub fn from_mut<T>(obj: &mut T) ->&mut [T]{
    unsafe{ transmute(RawSlice{ptr: obj as *mut T,len:1})}
}

pub fn from_ref<T>(obj: &T) -> &'_ [T]{
    unsafe{ transmute(RawSlice{ptr: obj as *const T as *mut (),len:1})}
}

pub unsafe fn from_raw_parts_mut<'a,T>(ptr: *mut T,len: usize) -> &'a mut [T]{
    transmute(RawSlice{ptr,len})
}

#[lang = "slice"]
impl<T> [T]{
    pub const fn size(&self) -> usize{
        unsafe{transmute::<_,RawSlice<T>>(self).len}
    }
    pub const fn is_empty(&self) -> bool{
        unsafe{transmute::<_,RawSlice<T>>(self).len == 0}
    }
}

