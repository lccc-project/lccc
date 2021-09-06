/**
 * rust/libcore/option.rs
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

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Option<T> {
    #[lang = "none"]
    None,
    #[lang = "some"]
    Some(T),
}
use crate::clone::Clone;
use crate::convert::From;
use crate::default::Default;
use crate::iter::{IntoIterator, Iterator};
use crate::ops::{Deref, DerefMut, FnOnce};
use crate::result::Result;
use crate::result::Result::Ok;
use crate::{Copy, PartialEq};
use Option::*;

impl<T> Default for Option<T> {
    fn default() -> Self {
        None
    }
}

impl<T> Option<T> {
    #[must_use = "if you intended to assert that this has a value, consider `.unwrap()` instead"]
    pub fn is_some(&self) -> bool {
        if let Some(_) = self {
            true
        } else {
            false
        }
    }

    #[must_use = "if you intended to assert that this doesn't have a value, consider `.and_then(|| panic!(\"`Option` had a value when expected `None`\"))` instead"]
    pub fn is_none(&self) -> bool {
        if let None = self {
            true
        } else {
            false
        }
    }

    #[must_use]
    #[unstable(feature = "option_result_contains")]
    pub fn contains<U>(&self, val: &U) -> bool
    where
        U: PartialEq<T>,
    {
        if let Some(t) = self {
            val == t
        } else {
            false
        }
    }

    pub fn as_ref(&self) -> Option<&T> {
        if let Some(t) = self {
            Some(t)
        } else {
            None
        }
    }

    pub fn as_mut(&mut self) -> Option<&mut T> {
        if let Some(t) = self {
            Some(t)
        } else {
            None
        }
    }

    pub fn as_pin_ref<'a>(self: Pin<&'a Self>) -> Option<Pin<&'a T>> {
        match self.get_ref() {
            /// SAFETY:
            /// This is a structural pin, and Option<T>: Unpin iff T: Unpin
            Some(r) => Some(unsafe { Pin::new_unchecked(r) }),
            None => None,
        }
    }

    pub fn as_pin_mut<'a>(self: Pin<&'a mut Self>) -> Option<Pin<&'a mut T>> {
        /// SAFETY:
        /// the match is not used to move from
        match unsafe { self.get_unchecked_mut() } {
            /// SAFETY:
            /// This is a structural pin, and Option<T>: Unpin iff T: Unpin
            Some(r) => Some(unsafe { Pin::new_unchecked(r) }),
            None => None,
        }
    }

    pub fn expect(self, msg: &str) -> T {
        if let Some(t) = self {
            t
        } else {
            panic!(msg)
        }
    }

    pub fn unwrap(self) -> T {
        if let Some(t) = self {
            t
        } else {
            panic!()
        }
    }

    pub fn unwrap_or(self, default: T) -> T {
        if let Some(t) = self {
            t
        } else {
            default
        }
    }

    pub fn unwrap_or_else<F: FnOnce() -> T>(self, op: F) -> T {
        if let Some(t) = self {
            t
        } else {
            op()
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> Option<U> {
        if let Some(t) = self {
            Some(op(t))
        } else {
            None
        }
    }

    pub fn map_or<U, F: FnOnce(T) -> U>(self, default: U, op: F) -> U {
        if let Some(t) = self {
            op(t)
        } else {
            default
        }
    }
    pub fn map_or_else<U, D: FnOnce() -> U, F: FnOnce(T) -> U>(self, default: D, op: F) -> U {
        if let Some(t) = self {
            op(t)
        } else {
            default()
        }
    }

    pub fn ok_or<E>(self, err: E) -> Result<T, E> {
        if let Some(t) = self {
            Result::Ok(t)
        } else {
            Result::Err(err)
        }
    }

    pub fn ok_or_else<E, F: FnOnce() -> E>(self, err: F) -> Result<T, E> {
        if let Some(t) = self {
            Result::Ok(t)
        } else {
            Result::Err(err())
        }
    }

    pub fn iter(&self) -> Iter<T> {
        Iter(IntoIter { opt: self.as_ref() })
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut(IntoIter { opt: self.as_mut() })
    }

    pub fn and<U>(self, other: Option<U>) -> Option<U> {
        if let None = self {
            None
        } else {
            other
        }
    }

    pub fn and_then<U, F: FnOnce(T) -> Option<U>>(self, op: F) -> Option<U> {
        if let Some(t) = self {
            op(t)
        } else {
            None
        }
    }

    pub fn filter<F: FnOnce(&T) -> bool>(self, pred: F) -> Option<T> {
        match self {
            Some(t) if pred(&t) => Some(t),
            _ => None,
        }
    }

    pub fn or(self, optb: Option<T>) -> Option<T> {
        match self {
            None => optb,
            _ => self,
        }
    }

    pub fn or_else<F: FnOnce() -> Option<T>>(self, op: F) -> Option<T> {
        match self {
            None => op(),
            _ => self,
        }
    }

    pub fn xor(self, optb: Option<T>) -> Option<T> {
        if let Some(t) = self {
            if let None = optb {
                Some(t)
            } else {
                None
            }
        } else {
            optb
        }
    }

    pub fn get_or_insert(&mut self, v: T) -> &mut T {
        if let None = self {
            *self = Some(v);
        }
        self.as_mut()
            .unwrap_or_else(|| unsafe { crate::hint::unreachable_unchecked() })
    }

    pub fn get_or_insert_with<F: FnOnce() -> T>(&mut self, op: F) -> &mut T {
        if let None = self {
            *self = Some(op());
        }
        self.as_mut()
            .unwrap_or_else(|| unsafe { crate::hint::unreachable_unchecked() })
    }

    pub fn take(&mut self) -> Option<T> {
        crate::mem::take(self)
    }

    pub fn replace(&mut self, val: T) -> Option<T> {
        crate::mem::replace(self, Some(val))
    }

    #[unstable(feature = "option_zip", issue = "70086")]
    pub fn zip<U>(self, other: Option<U>) -> Option<(T, U)> {
        match (self, other) {
            (Some(t), Some(u)) => Some((t, u)),
            _ => None,
        }
    }

    #[unstable(feature = "option_zip", issue = "70086")]
    pub fn zip_with<U, R, F: FnOnce(T, U) -> R>(self, other: Option<U>, op: F) -> Option<R> {
        match (self, other) {
            (Some(t), Some(u)) => Some(op(t, u)),
            _ => None,
        }
    }
}

impl<T: Copy> Option<&'_ T> {
    pub fn copied(self) -> Option<T> {
        if let Some(t) = self {
            Some(*t)
        } else {
            None
        }
    }
}

impl<T: Copy> Option<&'_ mut T> {
    pub fn copied(self) -> Option<T> {
        if let Some(t) = self {
            Some(*t)
        } else {
            None
        }
    }
}

impl<T: Clone> Option<&'_ T> {
    pub fn cloned(self) -> Option<T> {
        if let Some(t) = self {
            Some(t.clone())
        } else {
            None
        }
    }
}

impl<T: Clone> Option<&'_ mut T> {
    pub fn cloned(self) -> Option<T> {
        if let Some(t) = self {
            Some(t.clone())
        } else {
            None
        }
    }
}

impl<T: Default> Option<T> {
    pub fn unwrap_or_default(self) -> T {
        if let Some(t) = self {
            t
        } else {
            Default::default()
        }
    }
}

impl<T: Deref> Option<T> {
    pub fn as_deref(&self) -> Option<&T::Target> {
        if let Some(t) = self {
            Some(&*t)
        } else {
            None
        }
    }
}

impl<T: DerefMut> Option<T> {
    pub fn as_deref_mut(&mut self) -> Option<&mut T::Target> {
        if let Some(t) = self {
            Some(&mut *t)
        } else {
            None
        }
    }
}

impl<T, E> Option<Result<T, E>> {
    pub fn transpose(self) -> Result<Option<T>, E> {
        match self {
            Some(Result::Ok(t)) => Result::Ok(Some(t)),
            Some(Result::Err(e)) => Result::Err(e),
            None => Ok(None),
        }
    }
}

impl<T> Option<Option<T>> {
    pub fn flatten(self) -> Option<T> {
        if let Some(opt) = self {
            opt
        } else {
            None
        }
    }
}

impl<'a, T> From<&'a Option<T>> for Option<&'a T> {
    fn from(r: &'a Option<T>) -> Self {
        r.as_ref()
    }
}

impl<'a, T> From<&'a mut Option<T>> for Option<&'a mut T> {
    fn from(r: &'a mut Option<T>) -> Self {
        r.as_mut()
    }
}

impl<T> From<T> for Option<T> {
    fn from(val: T) -> Self {
        Some(val)
    }
}

#[derive(Clone, Debug)]
pub struct IntoIter<T> {
    opt: Option<T>,
}

pub struct Iter<'a, A: 'a>(IntoIter<&'a A>);
pub struct IterMut<'a, A: 'a>(IntoIter<&'a mut A>);

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.opt.take()
    }
}

impl<'a, A> Iterator for Iter<'a, A> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a, A> Iterator for IterMut<'a, A> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<T> IntoIterator for Option<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { opt: self }
    }
}

impl<'a, T: 'a> IntoIterator for &'a Option<T> {
    type Item = &'a T;
    type IntoIter = OptionIter<&'a T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T: 'a> IntoIterator for &'a mut Option<T> {
    type Item = &'a mut T;
    type IntoIter = OptionIter<&'a mut T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}
