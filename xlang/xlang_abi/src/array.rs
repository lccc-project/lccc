use std::{marker::PhantomData, mem::MaybeUninit, ptr::NonNull};

use sealed::Rebind;

use crate::{
    ops::{ChangeOutputType, Residual, Try},
    try_,
};

mod sealed {
    pub trait Sealed: Sized {
        type Elem: Sized;
    }

    pub trait Rebind<E>: Sealed {
        type Output: Sized + Sealed<Elem = E>;
    }
}

impl<T, const N: usize> sealed::Sealed for [T; N] {
    type Elem = T;
}

impl<T, const N: usize, U> sealed::Rebind<U> for [T; N] {
    type Output = [U; N];
}

/// Extension Trait for arrays
pub trait ArrayExt: sealed::Sealed {
    /// Tries to map from `self` to an array of another type, erroring if any other element appears
    fn try_map<E, R, F: FnMut(Self::Elem) -> R>(
        self,
        f: F,
    ) -> ChangeOutputType<R, <Self as Rebind<E>>::Output>
    where
        Self: Rebind<E>,
        R: Try<Output = E>,
        R::Residual: Residual<<Self as Rebind<E>>::Output>;
}

impl<T, const N: usize> ArrayExt for [T; N]
where
    Self: sealed::Sealed<Elem = T>,
{
    fn try_map<E, R, F: FnMut(Self::Elem) -> R>(
        self,
        mut f: F,
    ) -> ChangeOutputType<R, <Self as Rebind<E>>::Output>
    where
        Self: Rebind<E>,
        R: Try<Output = E>,
        R::Residual: Residual<<Self as Rebind<E>>::Output>,
    {
        #[allow(unused_braces)] // This should become const block if we ever reach 1.79.
        if {
            (core::mem::size_of::<T>() == core::mem::size_of::<E>())
                && (core::mem::size_of::<T>() != 0)
        } {
            struct Guard<'a, T, E> {
                begin: NonNull<E>,
                cursor: *mut T,
                end: NonNull<T>,
                _phantom: PhantomData<(&'a mut [T], &'a mut [E])>,
            }

            impl<'a, T, E> Drop for Guard<'a, T, E> {
                fn drop(&mut self) {
                    if core::mem::needs_drop::<E>() {
                        let mut ptr = self.begin.as_ptr();
                        while ptr < self.cursor.wrapping_sub(1).cast() {
                            unsafe {
                                core::ptr::drop_in_place(ptr);
                            }
                            ptr = unsafe { ptr.add(1) };
                        }
                    }

                    if core::mem::needs_drop::<T>() {
                        let mut ptr = self.cursor;
                        while ptr < self.end.as_ptr() {
                            unsafe {
                                core::ptr::drop_in_place(ptr);
                            }
                            ptr = unsafe { ptr.add(1) };
                        }
                    }
                }
            }

            impl<'a, T, E> Iterator for &'_ mut Guard<'a, T, E> {
                type Item = NonNull<T>;

                fn next(&mut self) -> Option<Self::Item> {
                    if core::ptr::eq(self.cursor, self.end.as_ptr()) {
                        None
                    } else {
                        let val = self.cursor;
                        self.cursor = unsafe { self.cursor.add(1) };

                        Some(unsafe { NonNull::new_unchecked(val) })
                    }
                }
            }
            let mut mem =
                unsafe { crate::util::transmute_unchecked::<_, [MaybeUninit<T>; N]>(self) };

            let range = mem.as_mut_ptr_range();

            let begin = unsafe { NonNull::new_unchecked(range.start).cast() };
            let end = unsafe { NonNull::new_unchecked(range.end).cast() };
            let cursor = range.start.cast();

            let mut guard: Guard<T, E> = Guard {
                begin,
                cursor,
                end,
                _phantom: PhantomData,
            };

            for ptr in &mut guard {
                let val = unsafe { ptr.as_ptr().read() };
                let rval = try_!(f(val));
                unsafe { ptr.cast::<E>().as_ptr().write(rval) }
            }
            core::mem::forget(guard);

            <_>::from_output(unsafe { crate::util::transmute_unchecked(mem) })
        } else {
            let mut mem = unsafe { MaybeUninit::<[MaybeUninit<E>; N]>::uninit().assume_init() };
            struct Guard<'a, T, E> {
                begin: NonNull<E>,
                cursor: *mut T,
                end: NonNull<T>,
                _phantom: PhantomData<(&'a mut [T], &'a mut [E])>,
            }

            impl<'a, T, E> Drop for Guard<'a, T, E> {
                fn drop(&mut self) {
                    if core::mem::needs_drop::<E>() {
                        let mut ptr = self.begin.as_ptr();
                        while ptr < self.cursor.wrapping_sub(1).cast() {
                            unsafe {
                                core::ptr::drop_in_place(ptr);
                            }
                            ptr = unsafe { ptr.add(1) };
                        }
                    }
                }
            }

            impl<'a, T, E> Iterator for &'_ mut Guard<'a, T, E> {
                type Item = NonNull<T>;

                fn next(&mut self) -> Option<Self::Item> {
                    if core::ptr::eq(self.cursor, self.end.as_ptr()) {
                        None
                    } else {
                        let val = self.cursor;
                        self.cursor = unsafe { self.cursor.add(1) };

                        Some(unsafe { NonNull::new_unchecked(val) })
                    }
                }
            }
            let range = mem.as_mut_ptr_range();

            let begin = unsafe { NonNull::new_unchecked(range.start).cast() };
            let end = unsafe { NonNull::new_unchecked(range.end).cast() };
            let cursor = range.start.cast();

            let mut guard: Guard<E, E> = Guard {
                begin,
                cursor,
                end,
                _phantom: PhantomData,
            };

            for (ptr, val) in (&mut guard).zip(self) {
                unsafe {
                    ptr.as_ptr().write(try_!(f(val)));
                }
            }
            <_>::from_output(unsafe { crate::util::transmute_unchecked(mem) })
        }
    }
}

#[cfg(test)]
#[allow(unstable_name_collisions)]
mod test {
    use super::ArrayExt;

    #[test]
    fn test_try_map_diff_size() {
        let x = [0u8; 32];

        let y: [u32; 32] = x.try_map(|x| x.try_into()).unwrap();

        assert_eq!(y, [0u32; 32]);
    }

    #[test]
    fn test_try_map_same_size() {
        let x = [0u8; 32];

        let y: [i8; 32] = x.try_map(|x| x.try_into()).unwrap();

        assert_eq!(y, [0i8; 32]);
    }
}
