use crate::marker::Sized;
use crate::slice;

pub trait Borrow<Borrowed: ?Sized>{
    fn borrow(&self)->&Borrowed;
}

pub trait BorrowMut<Borrowed: ?Sized>: Borrow<Borrowed>{
    fn borrow_mut(&mut self) -> &mut Borrowed;
}

impl<T: ?Sized> Borrow<T> for T{
    fn borrow(&self) -> &T {
        self
    }
}
impl<T: ?Sized> BorrowMut<T> for T{
    fn borrow_mut(&mut self) -> &mut T {
        self
    }
}
impl<T: ?Sized> Borrow<T> for &'_ T{
    fn borrow(&self) -> &T {
        *self
    }
}
impl<T: ?Sized> Borrow<T> for &'_ mut T{
    fn borrow(&self) -> &T {
        *self
    }
}
impl<T: ?Sized> BorrowMut<T> for &'_ mut T{
    fn borrow_mut(&mut self) -> &mut T {
        *self
    }
}

macro_rules! __borrow_array_impl{
    [$($n:literal)+] => {
        $(impl<T> Borrow<[T]> for [T;$n]{
            fn borrow(&self) -> &[T]{
                unsafe{slice::from_raw_parts(self as *const [T;$n] as *const T, $n)}
            }
        }
        impl<T> BorrowMut<[T]> for [T;$n]{
            fn borrow_mut(&mut self) -> &mut [T]{
                unsafe{slice::from_raw_parts_mut(self as *mut [T;$n] as *mut T, $n)}
            }
        })+
    }
}

__borrow_array_impl![0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32];


