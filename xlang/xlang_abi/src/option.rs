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

impl<T> From<Option<T>> for core::option::Option<T> {
    fn from(val: Option<T>) -> Self {
        match val {
            Some(v) => Self::Some(v),
            None => Self::None,
        }
    }
}

impl<T> Option<T> {
    #[must_use]
    pub const fn is_some(&self) -> bool {
        matches!(self, Some(_))
    }

    #[must_use]
    pub const fn is_none(&self) -> bool {
        matches!(self, None)
    }

    pub fn contains<U>(&self, val: &U) -> bool
    where
        U: PartialEq<T>,
    {
        match self {
            Some(x) => val.eq(x),
            None => false,
        }
    }

    #[allow(clippy::missing_const_for_fn)]
    pub fn unwrap(self) -> T {
        match self {
            Some(val) => val,
            None => panic!("Called unwrap on a None value"),
        }
    }

    #[allow(clippy::missing_const_for_fn)]
    pub fn expect(self, diag: &str) -> T {
        match self {
            Some(val) => val,
            None => panic!("{}", diag),
        }
    }

    #[allow(clippy::missing_const_for_fn)]
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Some(val) => val,
            None => default,
        }
    }

    #[allow(clippy::missing_const_for_fn)]
    pub fn unwrap_or_else<F: FnOnce() -> T>(self, op: F) -> T {
        match self {
            Some(val) => val,
            None => op(),
        }
    }

    #[allow(clippy::missing_const_for_fn)]
    #[inline]
    pub unsafe fn unwrap_unchecked(self) -> T {
        match self {
            Some(val) => val,
            None => core::hint::unreachable_unchecked(),
        }
    }
}
