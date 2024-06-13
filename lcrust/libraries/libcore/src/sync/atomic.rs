/**
 * rust/libcore/sync/atomic.rs
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
use crate::prelude::v1::*;

use crate::cell::UnsafeCell;
use crate::default::Default;
use crate::intrinsics::{
    __atomic_compare_exchange_strong, __atomic_compare_exchange_weak, __atomic_interthread_fence,
    __atomic_intrathread_fence, __atomic_load, __atomic_store,
};
use crate::mem::MaybeUninit;
use crate::{Result, Sync};

#[non_exhaustive]
#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Ordering {
    Relaxed = 0,
    Release = 1,
    Acquire = 2,
    AcqRel = 3,
    SeqCst = 4,
}

macro_rules! call_atomic_intrinsics{
    ($dyn_ord:expr @ [$($valid_ords:ident),* $(,)?]  => $atomic_intrin:ident ($($params:expr),* $(,)?)) => {
        (match $dyn_ord{
            $($crate::sync::atomic::Ordering::$valid_ords => $atomic_intrin :: <$crate::sync::atomic::Ordering::$valid_ords> ($($params),*),)
            ord => crate::panic!(crate::concat!("Invalid ordering for ", crate::stringify!($atomic_intrin), " {ord:?}."))
        })
    };

    ($dyn_ord:expr  => $atomic_intrin:ident ($($params:expr),* $(,)?)) => {
        call_atomic_intrinsics!($dyn_ord @ [Relaxed, Release, Acquire, AcqRel, SeqCst] => $atomic_intrin ($($params),*))
    };
}

macro_rules! call_atomic_cmpxchg_intrinsic{
    (($dyn_ord_success:expr, $dyn_ord_fail:expr) => $atomic_intrin:ident ($($params:expr),* $(,)?)) => {
        (match ($dyn_ord_success, $dyn_ord_fail){
            ($crate::sync::atomic::Ordering::Relaxed, $crate::sync::atomic::Ordering::Relaxed) => $atomic_intrin :: <$crate::sync::atomic::Ordering::Relaxed, $crate::sync::atomic::Ordering::Relaxed> ($($params),*),
            ($crate::sync::atomic::Ordering::Release, $crate::sync::atomic::Ordering::Relaxed) => $atomic_intrin :: <$crate::sync::atomic::Ordering::Release, $crate::sync::atomic::Ordering::Relaxed> ($($params),*),
            ($crate::sync::atomic::Ordering::Acquire, $crate::sync::atomic::Ordering::Relaxed) => $atomic_intrin :: <$crate::sync::atomic::Ordering::Acquire, $crate::sync::atomic::Ordering::Relaxed> ($($params),*),
            ($crate::sync::atomic::Ordering::Acquire, $crate::sync::atomic::Ordering::Acquire) => $atomic_intrin :: <$crate::sync::atomic::Ordering::Acquire, $crate::sync::atomic::Ordering::Acquire> ($($params),*),
            ($crate::sync::atomic::Ordering::AcqRel, $crate::sync::atomic::Ordering::Relaxed) => $atomic_intrin :: <$crate::sync::atomic::Ordering::AcqRel, $crate::sync::atomic::Ordering::Relaxed> ($($params),*),
            ($crate::sync::atomic::Ordering::AcqRel, $crate::sync::atomic::Ordering::Acquire) => $atomic_intrin :: <$crate::sync::atomic::Ordering::AcqRel, $crate::sync::atomic::Ordering::Acquire> ($($params),*),
            ($crate::sync::atomic::Ordering::SeqCst, $crate::sync::atomic::Ordering::Relaxed) => $atomic_intrin :: <$crate::sync::atomic::Ordering::SeqCst, $crate::sync::atomic::Ordering::Relaxed> ($($params),*),
            ($crate::sync::atomic::Ordering::SeqCst, $crate::sync::atomic::Ordering::Acquire) => $atomic_intrin :: <$crate::sync::atomic::Ordering::SeqCst, $crate::sync::atomic::Ordering::Acquire> ($($params),*),
            ($crate::sync::atomic::Ordering::SeqCst, $crate::sync::atomic::Ordering::SeqCst) => $atomic_intrin :: <$crate::sync::atomic::Ordering::SeqCst, $crate::sync::atomic::Ordering::SeqCst> ($($params),*),
            (ord_success, ord_fail) => crate::panic!(crate::concat!("Invalid orderings for ", crate::stringify!($atomic_intrin), " success: {ord_success:?}, fail: {ord_fail:?}."))
        })
    };
}

#[inline(always)]
pub fn compiler_fence(ord: Ordering) {
    call_atomic_intrinsics!(ord @ [Release, Acquire, AcqRel, SeqCst] => __atomic_intrathread_fence())
}

#[inline(always)]
pub fn fence(ord: Ordering) {
    call_atomic_intrinsics!(ord @ [Release, Acquire, AcqRel, SeqCst] => __atomic_interthread_fence())
}

mod sealed {
    pub trait Sealed {}

    #[repr(align(1))]
    #[derive(Copy, Clone)]
    pub struct Align1;
    #[repr(align(2))]
    #[derive(Copy, Clone)]
    pub struct Align2;
    #[repr(align(4))]
    #[derive(Copy, Clone)]
    pub struct Align4;
    #[repr(align(8))]
    #[derive(Copy, Clone)]
    pub struct Align8;
    #[repr(align(16))]
    #[derive(Copy, Clone)]
    pub struct Align16;
}

#[unstable(
    feature = "lccc_generic_atomic_type",
    reason = "Implementation Detail of lccc's implementation of `Atomic`"
)]
pub unsafe trait AtomicAccessTy: Sized + Copy + sealed::Sealed {
    type Align: Sized + Copy;
}

#[unstable(
    feature = "lccc_generic_atomic_type",
    reason = "Implementation Detail of lccc's implementation of `Atomic`"
)]
pub unsafe trait AtomicAccessTySameAlign: AtomicAccessTy {}

#[unstable(
    feature = "lccc_generic_atomic_type",
    reason = "Implementation Detail of lccc's implementation of `Atomic`"
)]
#[repr(C)]
pub struct Atomic<T: AtomicAccessTy> {
    align: MaybeUninit<T::Align>,
    cell: UnsafeCell<T>,
}

impl<T: AtomicAccessTy> Atomic<T> {
    pub const fn new(val: T) -> Self {
        Self {
            cell: UnsafeCell::new(val),
            align: MaybeUninit::uninit(),
        }
    }

    #[lcrust::const_unstable(feature = "const_cell_into_inner")]
    pub const fn into_inner(self) -> T {
        self.cell.into_inner()
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.cell.get_mut()
    }

    #[unstable(feature = "atomic_from_mut")]
    pub fn from_mut(r: &mut T) -> &mut Self
    where
        T: AtomicAccessTySameAlign,
    {
        unsafe { &mut *(r as *mut T as *mut Self) }
    }

    pub unsafe fn from_ptr<'a>(ptr: *mut T) -> &'a Self {
        unsafe { &*ptr.cast_mut() }
    }

    pub const fn as_ptr(&self) -> *mut T {
        self.cell.get()
    }

    #[inline(always)]
    pub fn load(&self, ord: Ordering) -> T {
        unsafe {
            call_atomic_intrinsics!(ord @ [Relaxed, Acquire, SeqCst] => __atomic_load (self.as_ptr()))
        }
    }

    #[inline(always)]
    pub fn store(&self, val: T, ord: Ordering) {
        unsafe {
            call_atomic_intrinsics!(ord @ [Relaxed, Release, SeqCst] => __atomic_store (self.as_ptr(), val))
        }
    }

    #[inline(always)]
    pub fn swap(&self, val: T, ord: Ordering) -> Ordering {
        unsafe { call_atomic_intrinsics!(ord => __atomic_swap (self.as_ptr(), val)) }
    }

    #[inline(always)]
    pub fn compare_exchange(
        &self,
        mut expected: T,
        new: T,
        success: Ordering,
        fail: Ordering,
    ) -> Result<T, T> {
        match unsafe {
            call_atomic_cmpxchg_intrinsic!((success, fail) => __atomic_compare_exchange_strong(self.as_ptr(), &mut expected, new))
        } {
            true => Ok(expected),
            false => Ok(expected),
        }
    }

    #[inline(always)]
    pub fn compare_exchange_weak(
        &self,
        mut expected: T,
        new: T,
        success: Ordering,
        fail: Ordering,
    ) -> Result<T, T> {
        match unsafe {
            call_atomic_cmpxchg_intrinsic!((success, fail) => __atomic_compare_exchange_weak(self.as_ptr(), &mut expected, new))
        } {
            true => Ok(expected),
            false => Ok(expected),
        }
    }

    #[inline(always)]
    pub fn compare_and_swap(&self, mut expected: T, new: T, order: Ordering) -> T {
        unsafe {
            match order {
                Ordering::Relaxed => __atomic_compare_exchange_strong::<
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                >(self.as_ptr(), &mut expected, new),
                Ordering::Release => __atomic_compare_exchange_strong::<
                    Ordering::Release,
                    Ordering::Relaxed,
                >(self.as_ptr(), &mut expected, new),
                Ordering::Acquire => __atomic_compare_exchange_strong::<
                    Ordering::Acquire,
                    Ordering::Acquire,
                >(self.as_ptr(), &mut expected, new),
                Ordering::AcqRel => __atomic_compare_exchange_strong::<
                    Ordering::AcqRel,
                    Ordering::Acquire,
                >(self.as_ptr(), &mut expected, new),
                Ordering::SeqCst => __atomic_compare_exchange_strong::<
                    Ordering::SeqCst,
                    Ordering::SeqCst,
                >(self.as_ptr(), &mut expected, new),
            }
        }

        expected
    }

    #[inline(always)]
    pub fn fetch_add(&self, val: T, ord: Ordering) -> T
    where
        T: crate::ops::Add<T>,
    {
        unsafe { call_atomic_intrinsics!(ord => __atomic_fetch_add(self.as_ptr(), val)) }
    }

    #[inline(always)]
    pub fn fetch_sub(&self, val: T, ord: Ordering) -> T
    where
        T: crate::ops::Sub<T>,
    {
        unsafe { call_atomic_intrinsics!(ord => __atomic_fetch_sub(self.as_ptr(), val)) }
    }

    #[inline]
    pub fn fetch_update<F: core::ops::FnMut(T) -> Option<T>>(
        &self,
        set_order: Ordering,
        fetch_order: Ordering,
        mut f: F,
    ) -> Result<T, T> {
        let mut val = MaybeUninit::uninit();
        while unsafe {
            call_atomic_intrinsics!(fetch_order @ [Relaxed, Acquire, SeqCst] => __atomic_read_transactional(self.as_ptr(), val.as_mut_ptr()))
        } {
            // SAFETY:
            // If `__atomic_read_transactional` returns `true`, it wrote to the second argument
            let val = unsafe { val.assume_init_read() };
            if let Some(new_val) = f(val) {
                match unsafe {
                    call_atomic_intrinsics!(set_order => __atomic_write_commit(self.as_ptr(), new_val))
                } {
                    ..0 => break,
                    0 => return Ok(val),
                    1.. => continue,
                }
            } else {
                unsafe {
                    __atomic_abort_transaction(self.as_ptr());
                }
                return Err(val);
            }
        }
        let mut val = self.load(fetch_order);
        loop {
            if let Some(new_val) = f(val) {
                match self.compare_exchange_weak(val, new_val, set_order, fetch_order) {
                    Ok(old_val) => break Ok(old_val),
                    Err(old_val) => val = old_val,
                }
            } else {
                break Err(val);
            }
        }
    }

    #[inline(always)]
    pub fn fetch_max(&self, new_val: T, ord: Ordering) -> T {
        self.fetch_update(ord, Ordering::Relaxed, |val| {
            Some(core::intrinsics::__builtin_max_val(val, new_val))
        })
    }
}

macro_rules! def_atomic_ty{
    {$( $(#[doc($doc:meta)])* #[$cfg_atomic:meta] #[$stability:meta] type $atomic_ty:ty = $base_ty:ty @ $align:ty;)*} => {
        $(
            impl sealed::Sealed for $base_ty{}
            #[$cfg_atomic]
            unsafe impl AtomicAccessTy for $base_ty{
                type Align = $align;
            }

            #[$$cfg_atomic]
            #[$stability]
            $(#[doc($doc)])*
            pub type $atomic_ty = Atomic<$base_ty>;
        )
    }
}
