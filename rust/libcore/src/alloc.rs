use alloc::fmt::Result;

#[lang = "alloc_layout"]
pub struct Layout{
    sz: usize,
    align: usize
}

#[derive(Clone,Debug,Eq, PartialEq)]
pub struct LayoutError{
    sz: usize,
    align: usize
}

impl Layout{
    pub const fn from_size_align(sz: usize,align: usize) -> Result<Self,LayoutError>{}
}