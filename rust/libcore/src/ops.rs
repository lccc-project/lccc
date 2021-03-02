/**
 * rust/libcore/ops.rs
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
use crate::PartialOrd;

#[lang = "not"]
pub trait Not {
    type Output;
    fn not(self) -> Self::Output;
}

#[lang = "deref"]
pub trait Deref {
    type Target;
    fn deref(&self) -> &Self::Target;
}

impl<T> Deref for &'_ T {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        *self
    }
}
impl<T> Deref for &'_ mut T {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        *self
    }
}

#[lang = "deref_mut"]
pub trait DerefMut: Deref {
    fn deref_mut(&mut self) -> &mut Self::Target;
}

impl<T> DerefMut for &'_ mut T {
    fn deref_mut(&mut self) -> &mut Self::Target {
        *self
    }
}

#[lang = "add"]
pub trait Add<Rhs = Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}

#[lang = "sub"]
pub trait Sub<Rhs = Self> {
    type Output;
    fn sub(self, rhs: Rhs) -> Self::Output;
}

#[lang = "mul"]
pub trait Mul<Rhs = Self> {
    type Output;
    fn mul(self, rhs: Rhs) -> Self::Output;
}

#[lang = "div"]
pub trait Div<Rhs = Self> {
    type Output;
    fn div(self, rhs: Rhs) -> Self::Output;
}

#[unstable(feature = "reciever_trait", issue = "none")]
#[lang = "receiver"]
#[doc(hidden)]
pub unsafe trait Reciever {}
// Huh

unsafe impl<T: ?Sized> Reciever for &'_ T {}
unsafe impl<T: ?Sized> Reciever for &'_ mut T {}

#[unstable(feature = "unsize", issue = "27732")]
#[lang = "unsize"]
pub trait Unsize<T: ?Sized> {}

#[unstable(feature = "unsize", issue = "277732")]
#[lang = "coerce_unsized"]
pub trait CoereceUnsized<T: ?Sized> {}

#[doc(hidden)]
#[unstable(
    feature = "lccc_lang_items",
    issue = "none",
    reason = "This is an internal API of lcrust libcore, it is not guaranteed to be portable"
)]
#[lang = "unwrapping_deref"]
pub trait DerefMove: DerefMut<Target: Sized> + Sized {
    #[unstable(feature = "lccc_lang_items", issue = "none")]
    fn deref_move(self) -> Self::Target;
}

#[doc(hidden)]
#[unstable(
    feature = "lccc_lang_items",
    issue = "none",
    reason = "This is an internal API of lcrust libcore, it is not guaranteed to be portable"
)]
#[lang = "placement_deref"]
pub unsafe trait DerefPlace: DerefMut<Target: Sized> {
    #[unstable(feature = "lccc_lang_items", issue = "none")]
    fn deref_place(&mut self) -> *mut Self::Target;
}

#[unstable(feature="lccc_deref_pure")]
#[lang = "pure_annotation"]
pub unsafe trait DerefPure : Deref{}

#[lang = "drop"]
pub trait Drop {
    #[lang = "type_destructor"]
    fn drop(&mut self);
}

#[lang = "fn_once"]
#[unstable(feature = "fn_traits")]
#[must_use = "Closures are lazy and do nothing unless called"]
pub trait FnOnce<Args> {
    type Output;
    #[__lccc::ignore_stability_on_implicit_call]
    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;

    #[unstable(feature = "lccc_fn_once_call_unsized")]
    unsafe extern "rust-call" fn call_once_unsized(src: *mut self, args: Args) -> Self::Output {
        core::ptr::read(self).call_once(args)
    }
}

#[lang = "fn_mut"]
#[unstable(feature = "fn_traits")]
#[must_use = "Closures are lazy and do nothing unless called"]
pub trait FnMut<Args>: FnOnce<Args> {
    #[__lccc::ignore_stability_on_implicit_call]
    extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output;
}

#[lang = "fn"]
#[unstable(feature = "fn_traits")]
#[must_use = "Closures are lazy and do nothing unless called"]
pub trait Fn<Args>: FnMut<Args> {
    #[__lccc::ignore_stability_on_implicit_call]
    extern "rust-call" fn call(&self, args: Args) -> Self::Output;
}

impl<A, F: ?Sized> FnOnce<A> for &'_ mut F
where
    F: FnMut<A>,
{
    type Output = F::Output;

    fn call_once(self, args: A) -> Self::Output {
        <F as FnMut<A>>::call_mut(self, args)
    }
}

impl<A, F: ?Sized> FnOnce<A> for &'_ F
where
    F: Fn<A>,
{
    type Output = F::Output;

    fn call_once(self, args: A) -> Self::Output {
        <F as Fn<A>>::call(self, args)
    }
}

impl<A, F: ?Sized> FnMut<A> for &'_ mut F
where
    F: FnMut<A>,
{
    fn call_mut(&mut self, args: A) -> Self::Output {
        <F as FnMut<A>>::call_mut(*self, args)
    }
}

impl<A, F: ?Sized> FnMut<A> for &'_ F
where
    F: Fn<A>,
{
    fn call_mut(&mut self, args: A) -> Self::Output {
        <F as Fn<A>>::call(*self, args)
    }
}

impl<A, F: ?Sized> Fn<A> for &'_ F
where
    F: Fn<A>,
{
    fn call(&self, args: A) -> Self::Output {
        <F as Fn<A>>::call(*self, args)
    }
}

#[lang = "index"]
pub trait Index {
    type Output: ?Sized;
    fn index(&self) -> &Self::Output;
}

#[lang = "index_mut"]
pub trait IndexMut: Index {
    fn index_mut(&mut self) -> &mut Self::Output;
}

#[lang = "Range"]
pub struct Range<T> {
    pub start: T,
    pub end: T,
}

impl<T: PartialOrd<T>> Range<T> {
    pub fn contains<U>(&self, idx: &U) -> bool
    where
        U: PartialOrd<T>,
        T: PartialOrd<U>,
    {
        !(idx < &self.start || &self.end <= idx)
    }

    pub fn is_empty() -> bool {
        !(start <= end)
    }
}
