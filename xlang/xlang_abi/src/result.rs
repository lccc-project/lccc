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

    #[allow(clippy::missing_const_for_fn)] // Objectively incorrect lint, missing ability to drop
    pub fn ok(self) -> Option<T> {
        if let Ok(val) = self {
            Some(val)
        } else {
            None
        }
    }

    #[allow(clippy::missing_const_for_fn)] // Objectively incorrect lint, missing ability to drop
    pub fn err(self) -> Option<E> {
        if let Err(e) = self {
            Some(e)
        } else {
            None
        }
    }

    pub const fn as_ref(&self) -> Result<&T, &E> {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(e),
        }
    }

    pub fn as_mut(&mut self) -> Result<&mut T, &mut E> {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(e),
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> Result<U, E> {
        match self {
            Ok(x) => Ok(op(x)),
            Err(e) => Err(e),
        }
    }

    pub fn map_or<U, F: FnOnce(T) -> U>(self, default: U, op: F) -> U {
        match self {
            Ok(x) => op(x),
            Err(_) => default,
        }
    }

    pub fn map_or_else<U, F: FnOnce(T) -> U, D: FnOnce(E) -> U>(self, default_op: D, op: F) -> U {
        match self {
            Ok(x) => op(x),
            Err(e) => default_op(e),
        }
    }

    pub fn map_err<D, F: FnOnce(E) -> D>(self, op: F) -> Result<T, D> {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(op(e)),
        }
    }
}

impl<T, E> From<std::result::Result<T, E>> for Result<T, E> {
    fn from(f: std::result::Result<T, E>) -> Result<T, E> {
        match f {
            std::result::Result::Ok(x) => Ok(x),
            std::result::Result::Err(e) => Err(e),
        }
    }
}

impl<T, E> From<Result<T, E>> for std::result::Result<T, E> {
    fn from(f: Result<T, E>) -> std::result::Result<T, E> {
        match f {
            Ok(x) => std::result::Result::Ok(x),
            Err(e) => std::result::Result::Err(e),
        }
    }
}
