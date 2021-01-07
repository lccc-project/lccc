use crate::Sized;

pub trait Default: Sized {
    fn default() -> Self;
}
