



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