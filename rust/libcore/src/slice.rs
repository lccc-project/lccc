use crate::intrinsics::transmute;

// lcrust implementation detail. Might open an RFC to make this part of rust
#[repr(C)]
#[doc(hidden)]
#[unstable(feature = "lccc_slice_layout")]
pub struct RawSlice<T> {
    ptr: *mut T,
    len: usize,
}

#[inline(always)]
pub unsafe fn from_raw_parts<'a, T>(ptr: *const T, len: usize) -> &'a [T] {
    transmute(RawSlice {
        ptr: ::__lccc::builtins::rust::limit(ptr, len) as *mut T,
        len,
    })
}

#[inline(always)]
pub fn from_mut<T>(obj: &mut T) -> &mut [T] {
    unsafe {
        transmute(RawSlice {
            ptr: obj as *mut T,
            len: 1,
        })
    }
}

#[inline(always)]
pub fn from_ref<T>(obj: &T) -> &'_ [T] {
    unsafe {
        transmute(RawSlice {
            ptr: obj as *const T as *mut (),
            len: 1,
        })
    }
}

#[inline(always)]
pub unsafe fn from_raw_parts_mut<'a, T>(ptr: *mut T, len: usize) -> &'a mut [T] {
    transmute(RawSlice {
        ptr: ::__lccc::builtins::rust::limit(ptr, len),
        len,
    })
}

#[lang = "slice"]
impl<T> [T] {
    #[inline(always)]
    pub const fn size(&self) -> usize {
        unsafe { transmute::<_, RawSlice<T>>(self).len }
    }
    #[inline(always)]
    pub const fn is_empty(&self) -> bool {
        unsafe { transmute::<_, RawSlice<T>>(self).len == 0 }
    }

    pub fn first(&self) -> Option<&T> {
        if self.is_empty() {
            None
        } else {
            Some(unsafe { &self.get_unchecked() })
        }
    }

    pub fn first_mut(&mut self) -> Option<&mut T> {
        if self.is_empty() {
            None
        } else {
            Some(unsafe { &mut self.get_unchecked_mut() })
        }
    }

    pub const fn as_ptr(&self) -> *const T {
        unsafe { transmute::<_, RawSlice<T>>(self).ptr as *const T }
    }

    pub fn as_mut_ptr(&self) -> *mut T {
        unsafe { transmute::<_, RawSlice<T>>(self).ptr }
    }
}
