use crate::Sized;

pub enum Infalliable {}

pub trait From<T>: Sized {
    fn from(_: T) -> Self;
}

pub trait Into<T>: Sized {
    fn into(self) -> T;
}

impl<T> From<T> for T {
    fn from(t: T) -> Self {
        t
    }
}

impl<T, U: From<T>> Into<U> for T {
    fn into(self) -> U {
        U::from(self)
    }
}
