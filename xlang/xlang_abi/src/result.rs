#[repr(u8)]
pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

use crate::option::{None, Option, Some};

pub use self::Result::{Err, Ok};

impl<T, E> Result<T, E> {
    #[must_use]
    pub const fn is_ok(&self) -> bool {
        matches!(self, Ok(_))
    }
    #[must_use]
    pub const fn is_err(&self) -> bool {
        matches!(self, Err(_))
    }

    pub fn contains<U>(&self, x: &U) -> bool
    where
        U: PartialEq<T>,
    {
        if let Ok(val) = self {
            x.eq(val)
        } else {
            false
        }
    }

    pub fn contains_err<F>(&self, f: &F) -> bool
    where
        F: PartialEq<E>,
    {
        if let Err(val) = self {
            f.eq(val)
        } else {
            false
        }
    }

    pub fn ok(self) -> Option<T> {
        if let Ok(val) = self {
            Some(val)
        } else {
            None
        }
    }

    pub fn err(self) -> Option<E> {
        if let Err(e) = self {
            Some(e)
        } else {
            None
        }
    }
}
