use std::sync::Arc;

pub struct NameAndType<'a>{
    name: &'a str,
    descriptor: &'a str
}


pub enum ConstantPoolEntry<'a>{
    Utf8(std::string::String),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(&'a str),
    String(&'a str),
    FieldRef(&'a str,&'a NameAndType<'a>),
    MethodRef(&'a str,&'a NameAndType<'a>),
    InterfaceMethodRef(&'a str,&'a NameAndType<'a>),
    NameAndType(NameAndType<'a>)
}


#[derive(Clone,PartialEq,Eq,Debug)]
pub struct LoadedClass{
    name: std::string::String,
    cl: Arc<()>
}

impl LoadedClass{
    pub fn is_assignable_from(&self,other: &LoadedClass) -> bool{
        unimplemented!()
    }
    pub fn is_array(&self) -> bool{
        unimplemented!()
    }
    pub fn get_name(&self) -> &str{
        &self.name
    }
}