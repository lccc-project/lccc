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

#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Visibility{
    Public = 0,
    Origin = 1,
    Module = 2,
    Private = 3,
    None = 4,
}

impl Default for Visibility{
    fn default() -> Self{
        Self::Public
    }
}


#[repr(C)]
#[derive(Clone, Debug, Default)]
pub struct ScopeMember{
    pub annotations: AnnotatedElement,
    pub vis: Visibility,
    pub member_decl: MemberDeclaration
}

#[repr(C)]
#[derive(Clone, Debug, Default)]
pub struct Scope{
    pub annotations: AnnotatedElement,
    pub members: HashMap<Path,ScopeMember>
}

#[repr(u32)]
#[derive(Clone, Debug, )]
pub enum MemberDeclaration{
    Empty,
    Scope(Scope),
}

impl Default for MemberDeclaration{
    fn default() -> Self{
        Self::Empty
    }
}