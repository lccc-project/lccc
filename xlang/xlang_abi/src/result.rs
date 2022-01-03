/// An ABI Safe version of [`std::result::Result`]
#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Result<T, E> {
    /// A result that contains a value
    Ok(T),
    /// A result that contains an error
    Err(E),
}

use std::fmt::Debug;

use crate::option::{None, Option, Some};

pub use self::Result::{Err, Ok};

impl<T, E> Result<T, E> {
    ///
    /// Checks if self contains a value
    #[must_use]
    pub const fn is_ok(&self) -> bool {
        matches!(self, Ok(_))
    }

    /// Checks if self contains an error
    #[must_use]
    pub const fn is_err(&self) -> bool {
        matches!(self, Err(_))
    }

    /// Checks if self contains a value that compares equal to `x`
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
    /// Checks if self contains an error that compares equal to `f`
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

    /// If self contains a value, returns that value inside an Option, otherwise returns `None`
    #[allow(clippy::missing_const_for_fn)] // Objectively incorrect lint, missing ability to drop
    pub fn ok(self) -> Option<T> {
        if let Ok(val) = self {
            Some(val)
        } else {
            None
        }
    }

    /// If self contains an error, returns that error inside an Option, otherwise returns `None`
    #[allow(clippy::missing_const_for_fn)] // Objectively incorrect lint, missing ability to drop
    pub fn err(self) -> Option<E> {
        if let Err(e) = self {
            Some(e)
        } else {
            None
        }
    }

    /// Maps self by returning a reference to the contained value, if any, or a reference to the contained error otherwise
    pub const fn as_ref(&self) -> Result<&T, &E> {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(e),
        }
    }

    /// Maps self by returning a mutable reference to the contained value, if any, or a mutable reference to the contained error otherwise
    pub fn as_mut(&mut self) -> Result<&mut T, &mut E> {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(e),
        }
    }

    /// Maps the value contained within self, if any, using `op`
    pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> Result<U, E> {
        match self {
            Ok(x) => Ok(op(x)),
            Err(e) => Err(e),
        }
    }

    /// Maps the value contained within self, if any, using `op`, and unwraps it, or returns the default value `default`.
    pub fn map_or<U, F: FnOnce(T) -> U>(self, default: U, op: F) -> U {
        match self {
            Ok(x) => op(x),
            Err(_) => default,
        }
    }

    /// Maps the value contained within self, if any, using `op`, and unwraps it, or returns a default value produced using `default_op`
    pub fn map_or_else<U, F: FnOnce(T) -> U, D: FnOnce(E) -> U>(self, default_op: D, op: F) -> U {
        match self {
            Ok(x) => op(x),
            Err(e) => default_op(e),
        }
    }

    /// Maps the error contained within self, if any
    pub fn map_err<D, F: FnOnce(E) -> D>(self, op: F) -> Result<T, D> {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(op(e)),
        }
    }

    /// Unwraps the contained value within self, if any
    ///
    /// # Panics
    /// Panics if self contains an error
    #[allow(clippy::missing_const_for_fn)] // Objectively incorrect lint, missing ability to drop
    pub fn unwrap(self) -> T
    where
        E: Debug,
    {
        match self {
            Ok(x) => x,
            Err(e) => panic!("Called unwrap with Err({:?})", e),
        }
    }

    /// Unwraps the contained value within self, if any
    ///
    /// # Panics
    /// Panics if self contains an error, with a message that contains `diag`
    #[allow(clippy::missing_const_for_fn)] // Objectively incorrect lint, missing ability to drop
    pub fn expect(self, diag: &str) -> T {
        match self {
            Ok(x) => x,
            Err(_) => panic!("{}", diag),
        }
    }

    /// Unwraps the contained value within self, if any, or returns `val`
    #[allow(clippy::missing_const_for_fn)] // Objectively incorrect lint, missing ability to drop
    pub fn unwrap_or(self, val: T) -> T {
        match self {
            Ok(x) => x,
            Err(_) => val,
        }
    }

    /// Unwraps the contained value within self, if any, or returns a default value produced by calling `op`
    pub fn unwrap_or_else<F: FnOnce(E) -> T>(self, op: F) -> T {
        match self {
            Ok(x) => x,
            Err(e) => op(e),
        }
    }

    /// Unwraps the contained value within self
    ///
    /// # Safety
    /// self shall contain a value
    #[allow(clippy::missing_const_for_fn)] // Objectively incorrect lint, missing ability to drop
    #[inline]
    pub unsafe fn unwrap_unchecked(self) -> T {
        match self {
            Ok(x) => x,
            Err(_) => core::hint::unreachable_unchecked(),
        }
    }
}

impl<T, E> From<std::result::Result<T, E>> for Result<T, E> {
    fn from(f: std::result::Result<T, E>) -> Self {
        match f {
            std::result::Result::Ok(x) => Ok(x),
            std::result::Result::Err(e) => Err(e),
        }
    }
}

impl<T, E> From<Result<T, E>> for std::result::Result<T, E> {
    fn from(f: Result<T, E>) -> Self {
        match f {
            Ok(x) => Self::Ok(x),
            Err(e) => Self::Err(e),
        }
    }
}
