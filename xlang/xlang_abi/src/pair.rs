#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pair<T, U>(pub T, pub U);

impl<T, U> From<(T, U)> for Pair<T, U> {
    fn from((a, b): (T, U)) -> Self {
        Self(a, b)
    }
}

impl<T, U> Into<(T, U)> for Pair<T, U> {
    fn into(self) -> (T, U) {
        let Self(a, b) = self;

        (a, b)
    }
}
