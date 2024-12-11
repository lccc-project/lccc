use core::marker::PhantomData;
use std::mem::ManuallyDrop;

#[inline(always)]
pub(crate) fn is_copy<T>() -> bool {
    struct TestIsCopy<T>(bool, PhantomData<T>);
    impl<T> Clone for TestIsCopy<T> {
        #[inline(always)]
        fn clone(&self) -> Self {
            Self(false, self.1)
        }
    }

    impl<T: Copy> Copy for TestIsCopy<T> {}

    [TestIsCopy(true, PhantomData::<T>)].clone()[0].0
}

pub(crate) const unsafe fn transmute_unchecked<T, U>(x: T) -> U {
    union Transmuter<T, U> {
        a: ManuallyDrop<T>,
        b: ManuallyDrop<U>,
    }

    unsafe {
        ManuallyDrop::into_inner(
            Transmuter {
                a: ManuallyDrop::new(x),
            }
            .b,
        )
    }
}
