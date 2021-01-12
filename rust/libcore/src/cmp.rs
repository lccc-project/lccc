/**
 * rust/libcore/cmp.rs
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
use crate::Option;
use crate::Option::Some;

#[lang = "eq"]
pub trait PartialEq<Rhs = Self> {
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn eq(&self, other: &Rhs) -> bool;
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn ne(&self, other: &Rhs) -> bool {
        !self.eq(other)
    }
}

pub enum Ordering {
    Less,
    Equal,
    Greater,
}

#[lang = "partial_ord"]
pub trait PartialOrd<Rhs = Self>: PartialEq<Rhs> {
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn partial_cmp(&self, other: &Rhs) -> Option<Ordering>;

    #[must_use = "Comparison Operations are generally free from side effects"]
    fn lt(&self, other: &Rhs) -> bool {
        if let Some(Ordering::Less) = self.partial_cmp(other) {
            true
        } else {
            false
        }
    }

    #[must_use = "Comparison Operations are generally free from side effects"]
    fn gt(&self, other: &Rhs) -> bool {
        if let Some(Ordering::Less) = self.partial_cmp(other) {
            true
        } else {
            false
        }
    }

    #[must_use = "Comparison Operations are generally free from side effects"]
    fn le(&self, other: &Rhs) -> bool {
        match self.partial_cmp(other) {
            Some(Ordering::Equal) | Some(Ordering::Less) => true,
            _ => false,
        }
    }
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn ge(&self, other: &Rhs) -> bool {
        match self.partial_cmp(other) {
            Some(Ordering::Equal) | Some(Ordering::Greater) => true,
            _ => false,
        }
    }
}

pub trait Eq: PartialEq<Self> {}

pub trait Ord: PartialOrd<Self> + Eq {
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn cmp(&self, other: &Rhs) -> Ordering;

    #[must_use = "Comparison Operations are generally free from side effects"]
    fn max(self, other: Self) -> Self
    where
        Self: Sized,
    {
        match self.cmp(&other) {
            Ordering::Less => other,
            _ => self,
        }
    }
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn min(self, other: Self) -> Self
    where
        Self: Sized,
    {
        match self.cmp(&other) {
            Ordering::Greater => other,
            _ => self,
        }
    }

    #[must_use = "Comparison Operations are generally free from side effects"]
    fn clamp(self, min: Self, max: Self) -> Self
    where
        Self: Sized,
    {
        if max < min {
            panic!();
        } else if self < min {
            min
        } else if max < self {
            max
        } else {
            self
        }
    }
}

#[rustc_builtin_macro]
#[allow_internal_unstable(core_intrinsics)]
pub macro PartialEq($item:item) {}

#[rustc_builtin_macro]
#[allow_internal_unstable(core_intrinsics)]
pub macro PartialOrd($item:item) {}

#[rustc_builtin_macro]
#[allow_internal_unstable(core_intrinsics)]
pub macro Ord($item:item) {}

#[rustc_builtin_macro]
#[allow_internal_unstable(core_intrinsics)]
pub macro Eq($item:item) {}
