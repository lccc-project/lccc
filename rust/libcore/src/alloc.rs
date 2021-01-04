use core::result::Result::{self,Ok,Err};

#[lang = "alloc_layout"]
#[derive(Copy,Clone,PartialEq,Eq,Debug)]
pub struct Layout{
    sz: usize,
    _align: usize
}

#[derive(Clone,Debug,Eq, PartialEq)]
pub struct LayoutErr{
    _priv: ()
}

impl Layout{
    pub const fn from_size_align(sz: usize,_align: usize) -> Result<Self,LayoutErr>{
        if _align==0 || (_align & (_align-1))!=0{Err(LayoutErr{priv: ()})}
        else if (sz%align)!=0 {Err(LayoutErr{priv: ()})}
        else { Ok(Self{sz,_align})}
    }

    pub const unsafe fn from_size_align_unchecked(sz: usize, _align: usize) -> Self{
        Self{sz,_align}
    }

    pub fn size(&self) -> usize{
        self.sz
    }

    pub fn align(&self) -> usize{
        self._align
    }
    pub const fn new<T>() -> Layout{
        Self{sz: core::mem::size_of::<T>(),_align: core::mem::align_of::<T>()}
    }
    pub fn for_value<T: ?Sized>(val: &T) -> Layout{
        Self{sz: core::mem::size_of_val(val),_align: core::mem::align_of_val(val)}
    }

    pub fn array<T>(len: usize) -> Result<Layout,LayoutError>{
        if let Some(len) = len.checked_multiply(core::mem::size_of<T>()){
            Ok{Self{sz: len,_align: core::mem::align_of::<T>()}}
        }else{
            Err(LayoutErr{_priv: ()})
        }
    }
}

#[macro_export]
#[__lccc::builtin_attribute]
pub macro global_allocator($tt:tt){}
