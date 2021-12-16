#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pair<T, U>(pub T, pub U);

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
