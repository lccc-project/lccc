#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Option<T> {
    None,
    Some(T),
}

pub use self::Option::{None, Some};

impl<T> From<T> for Option<T> {
    fn from(val: T) -> Self {
        Some(val)
    }
}

impl<T> From<core::option::Option<T>> for Option<T> {
    fn from(val: core::option::Option<T>) -> Self {
        match val {
            core::option::Option::Some(v) => Some(v),
            core::option::Option::None => None,
        }
    }
}

impl<T> Into<core::option::Option<T>> for Option<T> {
    fn into(self) -> core::option::Option<T> {
        match self {
            Some(v) => core::option::Option::Some(v),
            None => core::option::Option::None,
        }
    }
}

impl<T> Option<T> {}