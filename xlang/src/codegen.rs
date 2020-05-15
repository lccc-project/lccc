use crate::layout::Layout;
use std::ops::RangeBounds;

pub trait Codegen{
    fn backend_name(&self)->&str;
    fn arch(&self)->&str;
    fn required_int_align(&self,bits: usize) -> usize;
    fn required_float_align(&self,bits: usize) -> usize;
    fn preferred_argument_size(&self)->usize;
}

pub trait CodegenItem{
    fn codegen(&self)->&dyn Codegen;
}

pub enum ParamAttribute{
    Nonzero,
    Dereference(usize),
    VolatileDereference(usize),
    Write,
    ReadOnly,
    NonAliased,
}

pub trait Param: CodegenItem{
    fn set_type_from_layout(&mut self,layout: &dyn Layout);
    fn push_attribute(&mut self,attr: ParamAttribute);
}


pub trait CodegenFunction: CodegenItem{
    fn write_param(&mut self,param: Box<dyn Param>);
    fn create_param(&self)->Box<dyn Param>;
}

pub trait CodegenState: CodegenItem{

}