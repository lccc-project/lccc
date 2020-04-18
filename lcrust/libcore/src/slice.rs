use crate::intrinsics::transmute;

// lcrust implementation detail. Might open an RFC to make this part of rust
#[repr(C)]
#[derive(Copy,Clone)]
struct RawSlice<T>{
    ptr: *mut T,
    len: usize
}

pub unsafe fn from_raw_parts<T>(ptr: *const T,len: usize) -> &[T]{
    transmute(RawSlice{ptr: ptr as *mut _,len})
}

pub fn from_mut<T>(obj: &mut T) ->&mut [T]{
    unsafe{ transmute(RawSlice{ptr: obj as *mut _,len:1})}
}

pub fn from_ref<T>(obj: &T) -> &[T]{
    unsafe{ transmute(RawSlice{ptr: obj as *mut _,len:1})}
}

pub unsafe fn from_raw_parts_mut<T>(ptr: *mut T,len: usize) -> &mut [T]{
    transmute(RawSlice{ptr,len})
}

#[lang = "slice_impl"]
impl [T]{

}
