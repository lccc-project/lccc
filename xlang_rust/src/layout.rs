
use core::marker::PhantomData;
use std::{ffi::CStr, ops::Deref};

#[repr(C)]
pub struct DynamicSpan<'a,T>{
    _m_begin: *const T,
    _m_size: usize,
    _m_phantom: PhantomData<&'a [T]>
}

impl<'a,T> From<&'a [T]> for DynamicSpan<'a,T>{
    fn from(arr: &'a [T]) -> Self{
        DynamicSpan{
            _m_begin: arr.as_ptr(),
            _m_size: arr.len(),
            _m_phantom: PhantomData
        }
    }
}

impl<'a,T> From<&'a mut [T]> for DynamicSpan<'a,T>{
    fn from(arr: &'a mut [T]) -> Self{
        DynamicSpan{
            _m_begin: arr.as_ptr(),
            _m_size: arr.len(),
            _m_phantom: PhantomData
        }
    }
}

impl<'a,T> DynamicSpan<'a,T>{
    pub fn new<S: Deref<Target=[T]>>(container: &'a S) -> Self{
        Self::from(container.deref())
    }
}

#[repr(C)]
pub struct BasicStringView<'a,T>{
    _m_begin: *const T,
    _m_end: *const T,
    _m_phantom: PhantomData<&'a [T]>
}

impl<'a,T> Clone for BasicStringView<'a,T>{
    fn clone(&self) -> Self{
        Self{..*self}
    }
}

impl<'a,T> Copy for BasicStringView<'a,T>{}

impl<'a,T> From<&'a [T]> for BasicStringView<'a,T>{
    fn from(slice: &'a [T]) -> Self{
        BasicStringView{
            _m_begin: slice.as_ptr(),
            // SAFETY:
            // The fact we have a reference to a slice implies we have provenance for [slice.as_ptr(),slice.as_ptr()+slice.len())
            // Therefore, the offset call, which returns 1-passed the end, is sound
            _m_end: unsafe{slice.as_ptr().offset(slice.len() as isize)},
            _m_phantom: PhantomData
        }
    }
}

impl<'a,T> From<&'a mut [T]> for BasicStringView<'a,T>{
    fn from(slice: &'a mut [T]) -> Self{
        BasicStringView{
            _m_begin: slice.as_ptr(),
            // SAFETY:
            // The fact we have a reference to a slice implies we have provenance for [slice.as_ptr(),slice.as_ptr()+slice.len())
            // Therefore, the offset call, which returns 1-passed the end, is sound
            _m_end: unsafe{slice.as_ptr().offset(slice.len() as isize)},
            _m_phantom: PhantomData
        }
    }
}

impl<'a> From<&'a str> for BasicStringView<'a,u8>{
    fn from(slice: &'a str) -> BasicStringView<'a,u8>{
        Self::from(slice.as_bytes())
    }
}

impl<'a> From<&'a mut str> for BasicStringView<'a,u8>{
    fn from(slice: &'a mut str) -> BasicStringView<'a,u8>{
        Self::from(slice.as_bytes())
    }
}

impl<'a,T> BasicStringView<'a,T>{
    pub fn new<S: Deref<Target=[T]>>(st: &'a S) -> Self{
        Self::from(st.deref())
    } 

    pub fn as_slice(&self) -> &[T]{
        // SAFETY:
        // Ignoring FFI Considerations,
        // [self._m_begin,self._m_end) is a valid range that is not modified
        // The inferred lifetime is bounded by 'a because self is.
        // The slice noted above has been borrowed immutably for 'a.
        unsafe{
            std::slice::from_raw_parts(self._m_begin,self._m_end.offset_from(self._m_begin) as usize)
        }
    }

    /// 
    /// Produces a new StringView from a pointer and a len.
    /// Neither ptr nor len are checked, either may be invalid
    /// ## Safety
    /// The behaviour is undefined if any of the following are violated
    /// * ptr shall not be null and shall be well aligned (even if len==0)
    /// * The range [ptr,ptr+len) shall be a valid range for the lifetime of the BasicStringView, and ptr shall have read-only provenance for that range.
    /// * The above noted range shall not be modified for the lifetime of the BasicStringView
    /// Note that this is the case even if the resulting BasicStringView is unused.
    pub unsafe fn from_raw_begin_len(ptr: *const T,len: usize) -> Self{
        Self{
            _m_begin: ptr,
            _m_end: if len==0{ptr}else{ptr.offset(len as isize)},
            _m_phantom: PhantomData
        }
    }

    /// 
    /// Produces a BasicStringView from raw parts (a begin and end pointer)
    /// Neither begin nor end are checked
    /// 
    /// ## Safety
    /// The behaviour is undefined if any of the following are violated
    /// * Both begin and end shall not be null and shall be well-aligned (even if begin==end)
    /// * The range [begin,end) shall be valid for reads for the lifetime of the BasicStringView
    /// * The above range shall not be modified for the lifetime of the BasicStringView
    /// Note that this is the case even if the resulting BasicStringView is unused.
    pub unsafe fn from_raw_parts(begin: *const T,end: *const T) -> Self{
        Self{
            _m_begin: begin,
            _m_end: end,
            _m_phantom: PhantomData
        }
    }

    pub fn into_raw_parts(self) -> (*const T,*const T){
        (self._m_begin,self._m_end)
    }

}

impl<'a,T: PartialEq> PartialEq for BasicStringView<'a,T>{
    fn eq(&self,other: &Self) -> bool{
        self.as_slice()==other.as_slice()
    }
}

impl<'a,T: Eq> Eq for BasicStringView<'a,T>{}

impl<'a> BasicStringView<'a,u8>{
    pub fn from_str<S: Deref<Target=str>>(st: &'a S) -> Self{
        Self::from(st.deref())
    }
    
    pub fn from_cstr<S: Deref<Target=CStr>>(st: &'a S) -> Self{
        Self::from(st.deref().to_bytes())
    }

    // Note: as_cstr not provided because a weaker guarantee is provided wrt. NUL termination vs. UTF-8 correctness

    pub fn as_utf8(&self) -> Result<&str,std::str::Utf8Error>{
        std::str::from_utf8(self.as_slice())
    }
}

impl<'a> From<&'a [char]> for BasicStringView<'a,u32>{
    fn from(slice: &'a [char]) -> Self{
        unsafe{Self::from_raw_begin_len(slice.as_ptr() as *const u32,slice.len())}
    }
}

impl<'a> BasicStringView<'a,u32>{
    pub fn from_char_slice<S: Deref<Target=[char]>>(st: &'a S) -> Self{
        Self::from(st.deref())
    }

    pub fn as_char_slice(&self) -> Option<&[char]>{
        // SAFETY:
        // BasicStringView asserts validity of [_m_begin,_m_end)
        let (ptr,len) = (self._m_begin,unsafe{self._m_end.offset_from(self._m_end) as usize});
        for i in 0..len{
            // SAFETY:
            // len is returned from offset_from, which implies validity of [ptr,ptr+len),
            // Therefore, as i<len, ptr+i is valid. 
            drop(std::char::from_u32(unsafe{*ptr.offset(i as isize)})?);
        }
        
        // SAFETY:
        // For reasons above, [ptr,ptr+len) is valid.
        // Loop above ensures that 
        Some(unsafe{std::slice::from_raw_parts(ptr as *const char,len)})
    }
}

pub type StringView<'a> = BasicStringView<'a,u8>;
pub type U8StringView<'a> = BasicStringView<'a,u8>;
pub type U16StringView<'a> = BasicStringView<'a,u16>;
pub type U32StringView<'a> = BasicStringView<'a,u32>;
