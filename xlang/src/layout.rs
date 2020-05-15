use crate::codegen::{Codegen, CodegenState, CodegenFunction, ParamAttribute};
use std::ops::RangeBounds;
use crate::span::Value;

pub trait Layout{
    fn size(&self)->Option<usize>;
    fn align(&self)->Option<usize>;
    fn duplicate_boxed(&self)->Box<dyn Layout>;
}


#[derive(Copy,Clone)]
pub struct IntegerLayout{
    size: usize,
    align: usize,
    nonzero: bool,
    signed: bool,
    max_valid_bit: usize
}

impl IntegerLayout{
    ///
    /// Constructs a new IntegerLayout, with a given size and alignment in bytes, and a given signedness.
    /// align shall be a power of 2 and shall divide size
    ///
    /// Panics if size or align are invalid inputs
    pub fn new(size: usize,align: usize,signed: bool)->IntegerLayout{
        assert!(size%align==0&&align.count_ones()==1,"Size must be a multiple of alignment, and alignment must be a power of 2");
        Self{
            size,
            align,
            nonzero: false,
            signed,
            max_valid_bit: 0 // Max valid bit of 0 means all bits are valid to be 1
        }
    }
    ///
    /// Adds a validity invariant to the integer type, restricting all bits beyond bit.
    /// This has (slightly) different meanings for signed/unsigned types.
    /// For unsigned types, all bits from (bit+1)..(end) must be 0.
    /// For signed types, all bits from (bit+1)..(end) must be equal to bit in the value representation.
    ///
    /// Setting this limit to 0, or self.size()*8-1 disables this limitation.
    ///
    /// Panics if bit is beyond the end of the layout
    pub fn with_valid_limit(self,bit: usize) -> IntegerLayout{
        assert!(bit/8<size,"bit shall be less than the length of the type in bits");
        Self{
            max_valid_bit: bit,
            ..self
        }
    }
}


impl Layout for IntegerLayout{
    fn size(&self) -> Option<usize> {
        Some(self.size)
    }

    fn align(&self) -> Option<usize> {
        Some(self.align)
    }

    fn duplicate_boxed(&self) -> Box<dyn Layout> {
        Box::new(*self)
    }
}

pub struct UnitLayout;

impl Layout for UnitLayout{
    fn size(&self) -> Option<usize> {
        Some(0)
    }

    fn align(&self) -> Option<usize> {
        Some(1)
    }

    fn duplicate_boxed(&self) -> Box<dyn Layout> {
        Box::new(UnitLayout)
    }
}

pub struct EmptyLayout;

impl Layout for EmptyLayout{
    fn size(&self) -> Option<usize> {
        Some(0)
    }

    fn align(&self) -> Option<usize> {
        Some(1)
    }

    fn duplicate_boxed(&self) -> Box<dyn Layout> {
        Box::new(UnitLayout)
    }
}



pub struct PointerLayout{
    pointed_to: Box<dyn Layout>,

}



