use core::mem::MaybeUninit;

pub struct IntoIter<T, const N: usize> {
    inner: [MaybeUninit<T>; N],
}
