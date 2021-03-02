/**
 * rust/libcore/pin.rs
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

use core::marker::Unpin;
use core::ops::{Deref, DerefMut};

#[lang = "pin"]
#[repr(transparent)]
#[derive(Copy, Clone, Debug)]
pub struct Pin<P> {
    ptr: P,
}

impl<P: Deref> Pin<P>
where
    <P as Deref>::Target: Unpin,
{
    pub fn new(ptr: P) -> Self {
        Self { ptr }
    }

    pub fn into_inner(self) -> P {
        self.ptr
    }
}

impl<P: Deref> Pin<P> {
    pub unsafe fn new_unchecked(ptr: P) -> Self {
        Self { ptr }
    }

    pub fn as_ref(&self) -> Pin<&<P as Deref>::Target> {
        // SAFETY:
        // The Contract of Pin::new_unchecked here is upheld by the invariant of the previous Pin::new_unchecked
        // This simply converts it to a &T
        unsafe { Pin::new_unchecked(&*(self.ptr)) }
    }

    pub unsafe fn into_inner_unchecked(self) -> P {
        self.ptr
    }
}

impl<P: DerefMut> Pin<P> {
    pub fn as_mut(&mut self) -> Pin<&mut <P as Deref>::Target> {
        // SAFETY:
        // The Contract of Pin::new_unchecked here is upheld by the invariant of the previous Pin::new_unchecked
        // This simply converts it to a &mut T
        unsafe { Pin::new_unchecked(&mut *(self.ptr)) }
    }

    pub fn set(&mut self, value: <P as Deref>::Target)
    where
        <P as Deref>::Target: Sized,
    {
        *self.ptr = value;
    }
}

impl<'a, T: ?Sized> Pin<&'a T> {
    pub fn get_ref(self) -> &'a T {
        self.ptr
    }

    pub unsafe fn map_unchecked<U: ?Sized, F: FnOnce(&T) -> &U>(self, func: F) -> Pin<&'a U> {
        Pin::new_unchecked(func(self.ptr))
    }
}

impl<'a, T: ?Sized> Pin<&'a mut T> {
    pub fn into_ref(self) -> Pin<&'a T> {
        // SAFETY:
        // Like other "Projections" defined by the Pin API,
        // This is sound because the previous Pin::new_unchecked upholds the Pinning Invariant
        unsafe { Pin::new_unchecked(self.ptr) }
    }

    pub fn get_mut(self) -> &'a mut T
    where
        T: Unpin,
    {
        self.ptr
    }

    pub unsafe fn get_unchecked_mut(self) -> &'a mut T {
        self.ptr
    }

    pub unsafe fn map_unchecked_mut<U: ?Sized, F: FnOnce(&mut T) -> &mut U>(
        self,
        f: F,
    ) -> Pin<&'a mut U> {
        Pin::new_unchecked(f(self.ptr))
    }
}

impl<T: ?Sized> Pin<&'static T> {
    #[unstable(feature = "pin_static_ref", issue = "78168")]
    pub fn static_ref(ptr: &'static T) -> Self {
        // SAFETY:
        // This is safe because ptr is borrowed forever
        unsafe { Pin::new_unchecked(ptr) }
    }
}

impl<T: ?Sized> Pin<&'static mut T> {
    #[unstable(feature = "pin_static_ref", issue = "78168")]
    pub fn static_mut(ptr: &'static mut T) -> Self {
        // SAFETY:
        // This is safe because ptr is borrowed forever
        unsafe { Pin::new_unchecked(ptr) }
    }
}

impl<P: Deref> Deref for Pin<P> {
    type Target = <P as Deref>::Target;
    fn deref(&self) -> &Self::Target {
        self.ptr.deref()
    }
}

impl<P: DerefMut> DerefMut for Pin<P>
where
    <P as Deref>::Target: Unpin,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.pin.deref_mut()
    }
}

unsafe impl<P: DerefPure> DerefPure for Pin<P>{}
