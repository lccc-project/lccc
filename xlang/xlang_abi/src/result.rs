pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

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
}
