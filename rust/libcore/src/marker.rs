/**
 * rust/libcore/marker.rs
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

use crate::cell::UnsafeCell;
use crate::clone::Clone;
use crate::mem::ManuallyDrop;
use crate::ops::Drop;

#[lang = "sized"]
pub trait Sized {}

#[lang = "phantom_data"] // Still needed for variance
pub struct PhantomData<#[__lccc::no_unused_type_param] T: ?Sized>;

impl<T: ?Sized> Clone for PhantomData<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for PhantomData<T> {}

impl<T: ?Sized + Send> Send for PhantomData<T> {}
impl<T: ?Sized + Sync> Sync for PhantomData<T> {}
impl<T: ?Sized + Unpin> Unpin for PhantomData<T> {}

#[lang = "unpin"]
pub auto trait Unpin {}

pub struct PhantomPinned;

impl !Unpin for PhantomPinned {}

pub unsafe auto trait Send {}
impl<T: ?Sized> !Send for *const T {}
impl<T: ?Sized> !Send for *mut T {}
unsafe impl<T: ?Sized + Send> Send for &'_ mut T {}
unsafe impl<T: ?Sized + Sync> Send for &'_ T {}

pub unsafe auto trait Sync {}
impl<T: ?Sized> !Sync for *const T {}
impl<T: ?Sized> !Sync for *mut T {}
unsafe impl<T: ?Sized + Sync> Sync for &'_ mut T {}
unsafe impl<T: ?Sized + Sync> Sync for &'_ T {}

#[lang = "copy"]
pub trait Copy: Clone {}

#[unstable(feature = "structural_match", issue = "31434")]
#[rustc_on_unimplemented(message = "the type `{Self}` does not `#[derive(Eq)]`")]
#[lang = "structural_teq"]
pub trait StructuralEq {}

#[unstable(feature = "structural_match", issue = "31434")]
#[rustc_on_unimplemented(message = "the type `{Self}` does not `#[derive(PartialEq)]`")]
#[lang = "structural_peq"]
pub trait StructuralPartialEq {}

#[lang = "freeze"]
pub(crate) unsafe auto trait Freeze {}

impl<T: ?Sized> !Freeze for UnsafeCell<T> {}
unsafe impl<T: ?Sized> Freeze for PhantomData<T> {}
unsafe impl<T: ?Sized> Freeze for *const T {}
unsafe impl<T: ?Sized> Freeze for *mut T {}
unsafe impl<T: ?Sized> Freeze for &T {}
unsafe impl<T: ?Sized> Freeze for &mut T {}

#[lang = "trivial_destroy"]
#[doc(hidden)]
#[unstable(feature = "lccc_borrowck_helpers")]
#[__lccc::builtin_trait]
pub unsafe auto trait TrivialDestruction {}

impl<T: ?Sized + Drop> !TrivialDestruction for T {}

unsafe impl<'a, T: ?Sized> TrivialDestruction for &'a T {}
unsafe impl<'a, T: ?Sized> TrivialDestruction for &'a mut T {}
unsafe impl<T: ?Sized> TrivialDestruction for *const T {}
unsafe impl<T: ?Sized> TrivialDestruction for *mut T {}

unsafe impl<T: ?Sized + TrivialDestruction> TrivialDestruction for PhantomData<T> {}

#[lang = "non_owning"]
#[doc(hidden)]
#[unstable(feature = "lccc_borrowck_helpers")]
#[__lccc::builtin_trait]
pub unsafe auto trait NonOwning<T: ?Sized> {}

impl<T: ?Sized> !NonOwning<T> for T {}

impl<T: ?Sized> !NonOwning<T> for PhantomData<T> {}

impl<T: ?Sized> Same<T> for T {}

unsafe impl<'a, T: ?Sized> NonOwning<T> for &'a T {}
unsafe impl<'a, T: ?Sized> NonOwning<T> for &'a mut T {}
unsafe impl<T: ?Sized> NonOwning<T> for *mut T {}
unsafe impl<T: ?Sized> NonOwning<T> for *const T {}

#[lang = "trait_object_marker"]
#[doc(hidden)]
#[unstable(feature = "lccc_trait_object")]
#[__lccc::builtin_trait]
pub unsafe trait TraitObject {}
