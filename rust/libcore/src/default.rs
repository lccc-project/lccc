use crate::Sized;

pub trait Default: Sized{
    pub fn default()->Self;
}