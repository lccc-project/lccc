/// Enum Representing Control Flow for the [`try!`] macro
pub enum ControlFlow<B, C> {
    /// Continue execution
    Continue(C),
    /// Break execution
    Break(B),
}

/// A trait that allows types of residuals
pub trait Residual<Output>: Sized {
    /// The canonical `Try` type for the given output
    type Try: Try<Output = Output> + FromResidual<Self>;
}

/// Allows construction of types from Residuals
pub trait FromResidual<Residual = <Self as Try>::Residual> {
    /// Constructs from a residual value
    fn from_residual(res: Residual) -> Self;
}

/// Reimplementation of the standard [`Try`] trait.
pub trait Try: FromResidual {
    /// The residual of the type
    type Residual;
    /// The Output of the type
    type Output;

    /// Converts the `Output` type into type.
    fn from_output(output: Self::Output) -> Self;

    /// Branches into a Residual or Output value
    fn branch(self) -> ControlFlow<Self::Residual, Self::Output>;
}

/// An Empty enum, used for residuals
pub enum Empty {}

impl From<Empty> for core::convert::Infallible {
    fn from(value: Empty) -> Self {
        match value {}
    }
}

impl From<core::convert::Infallible> for Empty {
    fn from(value: core::convert::Infallible) -> Self {
        match value {}
    }
}

impl<T, E> Residual<T> for crate::result::Result<Empty, E> {
    type Try = crate::result::Result<T, E>;
}

impl<T, E> FromResidual for crate::result::Result<T, E> {
    fn from_residual(res: crate::result::Result<Empty, E>) -> Self {
        match res {
            crate::result::Result::Ok(v) => match v {},
            crate::result::Result::Err(e) => Self::Err(e),
        }
    }
}

impl<T, E> FromResidual<Result<core::convert::Infallible, E>> for crate::result::Result<T, E> {
    fn from_residual(res: Result<core::convert::Infallible, E>) -> Self {
        match res {
            Ok(v) => match v {},
            Err(e) => Self::Err(e),
        }
    }
}

impl<T, E> Try for crate::result::Result<T, E> {
    type Residual = crate::result::Result<Empty, E>;
    type Output = T;

    fn from_output(output: T) -> Self {
        Self::Ok(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Self::Ok(c) => ControlFlow::Continue(c),
            Self::Err(e) => ControlFlow::Break(crate::result::Result::Err(e)),
        }
    }
}

impl<T, E> Residual<T> for Result<core::convert::Infallible, E> {
    type Try = Result<T, E>;
}

impl<T, E> FromResidual for Result<T, E> {
    // use `core::convert::Infallible` instead of `Empty`
    fn from_residual(x: Result<core::convert::Infallible, E>) -> Self {
        match x {
            Ok(v) => match v {},
            Err(e) => Err(e),
        }
    }
}

impl<T, E> FromResidual<crate::result::Result<Empty, E>> for Result<T, E> {
    // use `core::convert::Infallible` instead of `Empty`
    fn from_residual(x: crate::result::Result<Empty, E>) -> Self {
        match x {
            crate::result::Ok(v) => match v {},
            crate::result::Err(e) => Err(e),
        }
    }
}

impl<T, E> Try for Result<T, E> {
    type Residual = Result<core::convert::Infallible, E>;
    type Output = T;

    fn from_output(output: Self::Output) -> Self {
        Ok(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Ok(val) => ControlFlow::Continue(val),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }
}

impl<T> Try for crate::option::Option<T> {
    type Output = T;
    type Residual = crate::option::Option<Empty>;

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Self::Some(val) => ControlFlow::Continue(val),
            Self::None => ControlFlow::Break(crate::option::None),
        }
    }

    fn from_output(output: Self::Output) -> Self {
        Self::Some(output)
    }
}

impl<T> Residual<T> for crate::option::Option<Empty> {
    type Try = crate::option::Option<T>;
}

impl<T> FromResidual for crate::option::Option<T> {
    fn from_residual(_: <Self as Try>::Residual) -> Self {
        Self::None
    }
}

impl<T> FromResidual<Option<core::convert::Infallible>> for crate::option::Option<T> {
    fn from_residual(_: Option<core::convert::Infallible>) -> Self {
        Self::None
    }
}

impl<T> Residual<T> for Option<core::convert::Infallible> {
    type Try = Option<T>;
}

impl<T> Try for Option<T> {
    type Output = T;
    type Residual = Option<core::convert::Infallible>;

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Some(val) => ControlFlow::Continue(val),
            None => ControlFlow::Break(None),
        }
    }

    fn from_output(output: Self::Output) -> Self {
        Some(output)
    }
}

impl<T> FromResidual for Option<T> {
    fn from_residual(_: <Self as Try>::Residual) -> Self {
        None
    }
}

impl<T> FromResidual<crate::option::Option<Empty>> for Option<T> {
    fn from_residual(_: crate::option::Option<Empty>) -> Self {
        None
    }
}

impl<T, E> FromResidual<Result<core::convert::Infallible, E>> for Option<Result<T, E>> {
    fn from_residual(res: Result<core::convert::Infallible, E>) -> Self {
        match res {
            Err(e) => Some(Err(e)),
            Ok(val) => match val {},
        }
    }
}

impl<T, E> FromResidual<crate::result::Result<Empty, E>> for Option<Result<T, E>> {
    fn from_residual(res: crate::result::Result<Empty, E>) -> Self {
        match res {
            crate::result::Err(e) => Some(Err(e)),
            crate::result::Ok(val) => match val {},
        }
    }
}

impl<T, E> FromResidual<Option<core::convert::Infallible>> for Result<Option<T>, E> {
    fn from_residual(_: Option<core::convert::Infallible>) -> Self {
        Ok(None)
    }
}

impl<T, E> FromResidual<crate::option::Option<Empty>> for Result<Option<T>, E> {
    fn from_residual(_: crate::option::Option<Empty>) -> Self {
        Ok(None)
    }
}

/// `?` operator for `xlang_abi` types
#[macro_export]
macro_rules! try_ {
    ($expr:expr) => {{
        match $crate::ops::Try::branch($expr) {
            $crate::ops::ControlFlow::Continue(val) => val,
            $crate::ops::ControlFlow::Break(residual) => {
                return $crate::ops::FromResidual::from_residual(residual)
            }
        }
    }};
}

/// A type alias that changes the `Output` type of `R` to `T`
pub type ChangeOutputType<R, T> = <<R as Try>::Residual as Residual<T>>::Try;

/// Implement [`FromResidual`] of [`Yeet`] for this type, to support the [`yeet`] macro
pub struct Yeet<T>(pub T);

impl<T> FromResidual<Yeet<()>> for core::option::Option<T> {
    fn from_residual(_: Yeet<()>) -> Self {
        Self::None
    }
}

impl<T> FromResidual<Yeet<()>> for crate::option::Option<T> {
    fn from_residual(_: Yeet<()>) -> Self {
        Self::None
    }
}

impl<T, E> FromResidual<Yeet<E>> for core::result::Result<T, E> {
    fn from_residual(res: Yeet<E>) -> Self {
        Self::Err(res.0)
    }
}

impl<T, E> FromResidual<Yeet<E>> for crate::result::Result<T, E> {
    fn from_residual(res: Yeet<E>) -> Self {
        Self::Err(res.0)
    }
}

/// Yeets an expression, breaking out of a [`Try`] context with an explicit `None`/`Err`
#[macro_export]
macro_rules! yeet {
    () => {
        return $crate::ops::FromResidual::from_residual($crate::ops::Yeet(()))
    };
    ($e:expr) => {
        return $crate::ops::FromResidual::from_residual($crate::ops::Yeet($e))
    };
}

pub use yeet;

#[cfg(test)]
mod test {
    use crate::prelude::v1::*;

    #[test]
    fn test_control_flow() -> std::result::Result<(), ()> {
        let res = crate::result::Result::Ok(1);
        let val = try_!(res);
        assert_eq!(val, 1);
        Ok(())
    }
}
