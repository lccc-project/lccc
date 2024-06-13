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
use crate::option::Option;
use crate::option::Option::Some;

#[lang = "eq"]
pub trait PartialEq<Rhs = Self> {
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn eq(&self, other: &Rhs) -> bool;
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn ne(&self, other: &Rhs) -> bool {
        !self.eq(other)
    }
}

#[repr(i8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Ordering {
    Less = -1,
    Equal = 0,
    Greater = 1,
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

#[lcrust::builtin_macro_def]
#[lcrust::decl_macro_derive]
#[allow_internal_unstable(core_intrinsics)]
pub macro PartialEq($item:item) {}

#[lcrust::builtin_macro_def]
#[lcrust::decl_macro_derive]
#[allow_internal_unstable(core_intrinsics)]
pub macro PartialOrd($item:item) {}

#[lcrust::builtin_macro_def]
#[lcrust::decl_macro_derive]
#[allow_internal_unstable(core_intrinsics)]
pub macro Ord($item:item) {}

#[lcrust::builtin_macro_def]
#[lcrust::decl_macro_derive]
#[allow_internal_unstable(core_intrinsics)]
pub macro Eq($item:item) {}

macro_rules! cmp_int {
    ($($ty:ty),* $(,)?) => {
        $(
            impl PartialEq for $ty{
                #[inline(always)]
                fn eq(&self, other: &Self) -> bool{
                    *self == *other
                }
            }

            impl Eq for $ty{}

            impl Ord for $ty{
                #[inline(always)]
                fn cmp(&self, other: &Self) -> Ordering{
                    let ord = core::intrinsics::__builtin_cmp::<$ty, i32>(*self, *other);

                    // Safety:
                    // `__builtin_cmp` returns `-1`, `0`, or `1` ordering to comparison b/c this is an integer type
                    unsafe{core::mem::transmute_copy(&ord)}
                }

                #[inline]
                fn max(self, other: Self) -> Self{
                    core::intrinsics::__builtin_max_val(self, other)
                }

                #[inline]
                fn min(self, other: Self) -> Self{
                    core::intrinsics::__builtin_min_val(self, other)
                }

                #[inline]
                fn clamp(self, lower: Self, upper: Self) -> Self{

                    if lower > upper{
                        panic!("Cannot clamp between {lower} and {upper} (lower must be less than or equal to upper)")
                    }

                    core::intrinsics::__builtin_clamp_val(self, lower, upper)
                }
            }
            impl PartialOrd for $ty{
                #[inline(always)]
                fn partial_cmp(&self, other: &Self) -> Option<Ordering>{
                    Some(<$ty>::cmp(self, other))
                }
            }
        )*
    }
}

cmp_int!(i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);

macro_rules! cmp_float {
    ($($ty:ty),* $(,)?) => {
        $(
            impl PartialEq for $ty{
                #[inline(always)]
                fn eq(&self, other: &Self) -> bool{
                    *self == *other
                }
            }
            impl PartialOrd for $ty{
                #[inline]
                fn partial_cmp(&self, other: &Self) -> Option<Ordering>{
                    let ord = core::intrinsics::__builtin_cmp::<$ty, core::mem::MaybeUninit<i32>>(*self, *other);
                    match (self.is_nan(), other.is_nan()){
                        // SAFETY:
                        // `__builtin_cmp` returns `-1`, `0`, or `1` ordering to b/c the floating-point comparison is ordered (neither value is NaN)
                        (false, false) => Some(unsafe{core::mem::transmute_copy(&ord)}),
                        _ => None
                    }
                }
            }
        )*
    }
}

cmp_float!(f32, f64);
