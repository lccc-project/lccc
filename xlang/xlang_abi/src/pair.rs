use std::cmp::Ordering;

/// An abi safe type that contains an ordered pair (2-tuple) of `T` and `U`
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, Eq, Ord, Default)]
pub struct Pair<T, U>(pub T, pub U);

impl<T, R, U, S> PartialEq<Pair<R, S>> for Pair<T, U>
where
    T: PartialEq<R>,
    U: PartialEq<S>,
{
    fn eq(&self, rhs: &Pair<R, S>) -> bool {
        (self.0 == rhs.0) && (self.1 == rhs.1)
    }
}

impl<T, R, U, S> PartialOrd<Pair<R, S>> for Pair<T, U>
where
    T: PartialOrd<R>,
    U: PartialOrd<S>,
{
    fn partial_cmp(&self, rhs: &Pair<R, S>) -> Option<Ordering> {
        match self.0.partial_cmp(&rhs.0)? {
            Ordering::Equal => self.1.partial_cmp(&rhs.1),
            ord => Some(ord),
        }
    }
}

impl<T, U> From<(T, U)> for Pair<T, U> {
    fn from((a, b): (T, U)) -> Self {
        Self(a, b)
    }
}

impl<T, U> From<Pair<T, U>> for (T, U) {
    fn from(pair: Pair<T, U>) -> (T, U) {
        let Pair(a, b) = pair;

        (a, b)
    }
}
