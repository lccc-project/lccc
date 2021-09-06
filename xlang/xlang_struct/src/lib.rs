pub use xlang_abi::prelude::v1::*;

#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PathComponent {
    Root,
    Text(String),
    SpecialComponent(String),
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct Path {
    pub components: Vec<PathComponent>,
}

#[repr(u8)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AnnotationItem {
    Meta(Box<Annotation>),
    Identifier(Path),
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct Annotation {
    pub items: Vec<AnnotationItem>,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct AnnotatedElement {
    pub annotations: Vec<Annotation>,
}
