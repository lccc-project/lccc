/**
 * rust/libcore/borrow.rs
 * This file is part of lcrust standard libraries, a part of the lccc project
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * Like all libraries as part of the lccc project,
 *  the lcrust standard libraries are additionally dual licensed under the terms of the MIT and Apache v2 license. 
 * When dealing in this software, you may, at your option, do so under only those terms,
 *  or only under the terms of the GNU Lesser General Public License, or under both sets of terms. 
 */
use crate::marker::Sized;
use crate::slice;

pub trait Borrow<Borrowed: ?Sized> {
    fn borrow(&self) -> &Borrowed;
}

pub trait BorrowMut<Borrowed: ?Sized>: Borrow<Borrowed> {
    fn borrow_mut(&mut self) -> &mut Borrowed;
}

impl<T: ?Sized> Borrow<T> for T {
    fn borrow(&self) -> &T {
        self
    }
}
impl<T: ?Sized> BorrowMut<T> for T {
    fn borrow_mut(&mut self) -> &mut T {
        self
    }
}
impl<T: ?Sized> Borrow<T> for &'_ T {
    fn borrow(&self) -> &T {
        *self
    }
}
impl<T: ?Sized> Borrow<T> for &'_ mut T {
    fn borrow(&self) -> &T {
        *self
    }
}
impl<T: ?Sized> BorrowMut<T> for &'_ mut T {
    fn borrow_mut(&mut self) -> &mut T {
        *self
    }
}

impl<T, const N: usize> Borrow<[T]> for [T; N] {
    fn borrow(&self) -> &[T] {
        self
    }
}

impl<T, const N: usize> BorrowMut<[T]> for [T; N] {
    fn borrow(&mut self) -> &mut [T] {
        self
    }
}
