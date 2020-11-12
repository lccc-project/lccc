use alloc::fmt::Result;

#[lang = "alloc_layout"]
#[derive(Copy,Clone,PartialEq,Eq,Debug)]
pub struct Layout{
    sz: usize,
    _align: usize
}

#[derive(Clone,Debug,Eq, PartialEq)]
pub struct LayoutError{
    sz: usize,
    _align: usize
}

impl Layout{
    pub const fn from_size_align(sz: usize,_align: usize) -> Result<Self,LayoutError>{
        if _align==0 || (_align & (_align-1))!=0{Err(LayoutError{sz,_align})}
        else if (sz%align)!=0 {Err(LayoutError{sz,_align})}
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
        Self{sz: core::mem::size_of::<T>(),align: core::mem::align_of::<T>()}
    }
    pub fn for_value<T: ?Sized>(val: &T) -> Layout{
        Self{sz: core::mem::size_of_val(val),align: core::mem::align_of_val(val)}
    }
}