/// An abi safe version of [`std::option::Option`].
///
/// ## FFI Behaviour
///
/// This type has fixed ABI for all `T` that has fixed ABI. This means that [`self::Option`] is always FFI safe (provided `T` is). However, this costs a number of things.
/// In particular, niche optimization does not occur for this type. If `T` is known to be a type guaranteed to have niche optimization (such as a reference, function pointer, or `std::boxed::Box`),
/// then [`std::option::Option`] should be used instead.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Option<T> {
    /// None
    None,
    /// Some(T)
    Some(T),
}

use std::ops::{Deref, DerefMut};

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

impl<T> From<Option<T>> for core::option::Option<T> {
    fn from(val: Option<T>) -> Self {
        match val {
            Some(v) => Self::Some(v),
            None => Self::None,
        }
    }
}

impl<T> Option<T> {
    /// Checks if `self` is `Some`.
    #[must_use]
    pub const fn is_some(&self) -> bool {
        matches!(self, Some(_))
    }

    /// Checks if `self` is `None`.
    #[must_use]
    pub const fn is_none(&self) -> bool {
        matches!(self, None)
    }

    /// Checks if `self` contains a value that compares equal to `val`
    pub fn contains<U>(&self, val: &U) -> bool
    where
        U: PartialEq<T>,
    {
        match self {
            Some(x) => val.eq(x),
            None => false,
        }
    }

    /// Unwraps self into a value of type `T` if it is `Some`.
    ///
    /// # Panics
    /// panics if self is `None`
    #[allow(clippy::missing_const_for_fn)]
    #[track_caller]
    pub fn unwrap(self) -> T {
        match self {
            Some(val) => val,
            None => panic!("Called unwrap on a None value"),
        }
    }

    /// Unwraps self if it is `None`.
    ///
    /// # Panics
    /// panics if self is `Some(val)`

    #[track_caller]
    pub fn unwrap_none(self)
    where
        T: core::fmt::Debug,
    {
        match self {
            Some(val) => panic!("Called unwrap_none on Some value: {:?}", val),
            None => (),
        }
    }

    /// Unwraps self into a value of type `T` if it is `Some`.
    ///
    /// # Panics
    /// panics  with a message containing `diag` if self is `None`.
    #[track_caller]
    pub fn expect(self, diag: &str) -> T {
        match self {
            Some(val) => val,
            None => panic!("{}", diag),
        }
    }

    /// Unwraps self if it is `None`.
    ///
    /// # Panics
    /// panics with a message containing `diag` self is `Some(val)`

    #[track_caller]
    pub fn expect_none(self, diag: &str)
    where
        T: core::fmt::Debug,
    {
        match self {
            Some(val) => panic!("{}: {:?}", diag, val),
            None => (),
        }
    }

    #[allow(clippy::missing_const_for_fn)]
    /// Unwraps self into a value of type `T` if it is `Some`, or returns the result of calling `op` otherwise.
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Some(val) => val,
            None => default,
        }
    }

    /// Unwraps self into a value of type `T` if it is `Some`, or returns the result of calling `op` otherwise.
    #[allow(clippy::missing_const_for_fn)]
    pub fn unwrap_or_else<F: FnOnce() -> T>(self, op: F) -> T {
        match self {
            Some(val) => val,
            None => op(),
        }
    }

    /// Unwraps self into a value of type `T`
    ///
    /// # Safety
    /// self must not be `None`.
    #[allow(clippy::missing_const_for_fn)]
    #[inline]
    pub unsafe fn unwrap_unchecked(self) -> T {
        match self {
            Some(val) => val,
            None => core::hint::unreachable_unchecked(),
        }
    }

    /// Unwraps self into a value of type `T`
    ///
    /// # Safety
    /// self must not be `None`.
    #[allow(clippy::missing_const_for_fn)]
    #[inline]
    pub unsafe fn unwrap_none_unchecked(self) {
        match self {
            Some(_) => core::hint::unreachable_unchecked(),
            None => (),
        }
    }

    /// Maps `self` into an Option containing a reference to the inner value if `self` is `Some` or `None` otherwise
    #[allow(clippy::needless_match)] // False positive
    pub const fn as_ref(&self) -> Option<&T> {
        match self {
            Some(x) => Some(x),
            None => None,
        }
    }

    /// Maps `self` into an Option containing a mutable reference to the inner value if `self` is `Some` or `None` otherwise
    #[allow(clippy::needless_match)] // False positive
    pub fn as_mut(&mut self) -> Option<&mut T> {
        match self {
            Some(x) => Some(x),
            None => None,
        }
    }

    /// Maps `self` into an Option dereferencing the inner value if `self` is `Some`, or `None` otherwise.
    #[allow(clippy::needless_match)] // False positive
    pub fn as_deref(&self) -> Option<&<T as Deref>::Target>
    where
        T: Deref,
    {
        match self {
            Some(x) => Some(x),
            None => None,
        }
    }

    /// Maps `self` into an Option mutably dereferencing the inner value if `self` is `Some`, or `None` otherwise.
    #[allow(clippy::needless_match)] // False positive
    pub fn as_deref_mut(&mut self) -> Option<&<T as Deref>::Target>
    where
        T: DerefMut,
    {
        match self {
            Some(x) => Some(x),
            None => None,
        }
    }

    /// Maps self using `f`.
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Option<U> {
        match self {
            Some(x) => Some(f(x)),
            None => None,
        }
    }

    /// Maps self using `f`, and flattens the result
    pub fn flat_map<U, F: FnOnce(T) -> Option<U>>(self, f: F) -> Option<U> {
        match self {
            Some(x) => f(x),
            None => None,
        }
    }

    /// Takes the contained value outside of this [`Option`], leaving it as [`None`]
    #[must_use]
    pub fn take(&mut self) -> Self {
        core::mem::take(self)
    }

    /// Assigns `*self` to `Some(val)`, and returns a mutable reference to the inner value of `self`
    pub fn insert(&mut self, val: T) -> &mut T {
        *self = Some(val);

        match self {
            Some(val) => val,
            None => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    /// Assigns `*self` to `Some(val)` if `*self` is `None`, and returns a mutable reference to the inner value of `self`
    ///
    /// `val` is eagerly evaluated, even if `self` is `None`. If constructing or dropping `T` is an expensive operation, consider using [`Self::get_or_insert_with`] instead
    pub fn get_or_insert(&mut self, val: T) -> &mut T {
        match self {
            Some(val) => val,
            None => {
                *self = Some(val);

                match self {
                    Some(val) => val,
                    None => unsafe { core::hint::unreachable_unchecked() },
                }
            }
        }
    }

    /// Assigns `*self` to `Some(ctor())` if `*self` is `None`, and returns a mutable reference to the inner value of `self`
    pub fn get_or_insert_with<F: FnOnce() -> T>(&mut self, ctor: F) -> &mut T {
        match self {
            Some(val) => val,
            None => {
                *self = Some(ctor());

                match self {
                    Some(val) => val,
                    None => unsafe { core::hint::unreachable_unchecked() },
                }
            }
        }
    }
}

impl<T: Default> Option<T> {
    /// Unwraps self into a value of type `T` if it is `Some`, or returns the default value associated with the type otherwise.
    pub fn unwrap_or_default(self) -> T {
        self.unwrap_or_else(T::default)
    }
}

impl<T> Option<Option<T>> {
    /// Flattens self by removing a single level of [`Option`]
    #[allow(clippy::missing_const_for_fn)]
    pub fn flatten(self) -> Option<T> {
        match self {
            Some(x) => x,
            None => None,
        }
    }
}

impl<T> Default for Option<T> {
    fn default() -> Self {
        Self::None
    }
}
