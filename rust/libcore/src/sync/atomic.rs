use crate::prelude::v1::*;

use crate::cell::UnsafeCell;
use crate::default::Default;
use crate::{Sync, Result};
use crate::intrinsics::transmute;

#[non_exhaustive]
#[derive(Clone,Copy,Ord, PartialOrd, Eq, PartialEq,Hash)]
pub enum Ordering{
    Relaxed,
    Release,
    Acquire,
    AcqRel,
    SeqCst
}

#[inline]
#[__lccc::xlang_opt_hint(contains_sequence)]
pub fn compiler_fence(ord: Ordering){
    match ord{
        Ordering::Release => {__lccc::xir!("sequence atomic release")}
        Ordering::Acquire => {__lccc::xir!("sequence atomic acquire")}
        Ordering::AcqRel => {__lccc::xir!("sequence atomic acqrel")}
        Ordering::SeqCst => {__lccc::xir!("sequence atomic seq_cst")}
        ord => panic!("Invalid ordering {}",ord)
    }
}

#[inline]
#[__lccc::xlang_opt_hint(contains_fence)]
pub fn fence(ord: Ordering){
    match ord{
        Ordering::Release => {__lccc::xir!("fence atomic release")}
        Ordering::Acquire => {__lccc::xir!("fence atomic acquire")}
        Ordering::AcqRel => {__lccc::xir!("fence atomic acqrel")}
        Ordering::SeqCst => {__lccc::xir!("fence atomic seq_cst")}
        _ => panic!("Invalid ordering {}")
    }
}

#[cfg(target_has_atomic = "8")]
#[repr(C,align(1))]
pub struct AtomicBool{
    inner: UnsafeCell<u8>
}

#[cfg(target_has_atomic = "8")]
impl Default for AtomicBool{
    fn default() -> Self{
        AtomicBool::new(false)
    }
}

#[cfg(target_has_atomic = "8")]
unsafe impl Sync for AtomicBool{}

#[cfg(target_has_atomic = "8")]
impl AtomicBool{
    pub const fn new(x: bool) -> Self{
        Self{inner: UnsafeCell::new(x as u8)}
    }

    pub fn get_mut(&mut self) -> &mut bool{
        // SAFETY:
        // This is valid because self is uniquely borrowed for the implicit lifetime
        unsafe{transmute(self.inner.get_mut())}
    }

    #[unstable(feature="atomic_from_mut",issue="76314")]
    pub fn from_mut(x: &mut bool) -> &Self{
        unsafe{&*(x as *mut bool as *mut Self as *const Self)}
    }

    pub fn into_inner(self) -> bool{
        // SAFETY:
        // This is valid because self is uniquely owned here, 
        // So there is a static guarantee that no race occurs. 
        unsafe{*(self.inner.get() as *mut bool)}
    }

    pub fn load(&self,ord: Ordering) -> bool{
        // SAFETY:
        // self.inner.get() is valid because it is contained within &self, which is valid
        // AtomicBool has a "validity" invariant equivalent to the one for bool
        unsafe{transmute(ops::atomic_load(self.inner.get(),ord))}
    }

    pub fn store(&self,val: bool,ord: Ordering){
        // SAFETY:
        // self.inner.get() is valid because it is contained within &self, which is valid
        // self.inner.get() is valid for writing because it points to a mutable subobject of *self
        unsafe{ops::atomic_store(self.inner.get(),val as u8,ord)}
    }

    pub fn swap(&self,mut val: bool,ord: Ordering) -> bool{
        // SAFETY:
        // Same as load and store
        unsafe{ops::atomic_swap(self.inner.get(),&mut val as *mut bool as *mut u8,ord)}
        val
    }

    pub fn compare_and_swap(&self,mut current: bool,val: bool,ord: Ordering) -> bool{
        unsafe{ops::atomic_compare_exchange(self.inner.get(),&mut current as *mut bool as *mut u8,val,ord)}
        current
    }

    pub fn compare_exchange_weak(&self,mut expected: bool,update: bool,success_ord: Ordering,failure_ord: Ordering) -> Result<bool,bool>{
        if match (success_ord,failure_ord){
            (o @ Ordering::SeqCst,Ordering::Acquire | Ordering::SeqCst) => unsafe{ops::atomic_compare_exchange_weak(self.inner.get(),&mut current as *mut bool as *mut u8,val as u8,o)},
            (o @ Ordering::AcqRel | Ordering::Acquire,Ordering::Acquire) => unsafe{ops::atomic_compare_exchange_weak(self.inner.get(),&mut current as *mut bool as *mut u8,val as u8,o)},
            (o,Ordering::Relaxed) => unsafe{ops::atomic_compare_exchange_weak_fail_relaxed(self.inner.get(),&mut current as *mut bool as *mut u8,val as u8,o)}
            _ => panic!("Invalid ordering pair for compare_exchange_weak")
        }{
            Ok(expected)
        }else{
            Err(expected)
        }
    }

    pub fn compare_exchange(&self,mut expected: bool,update: bool,success_ord: Ordering,failure_ord: Ordering) -> Result<bool,bool>{
        if match (success_ord,failure_ord){
            (o @ Ordering::SeqCst,Ordering::Acquire | Ordering::SeqCst) => unsafe{ops::atomic_compare_exchange(self.inner.get(),&mut current as *mut bool as *mut u8,val as u8,o)},
            (o @ Ordering::AcqRel | Ordering::Acquire,Ordering::Acquire) => unsafe{ops::atomic_compare_exchange(self.inner.get(),&mut current as *mut bool as *mut u8,val as u8,o)},
            (o,Ordering::Relaxed) => unsafe{ops::atomic_compare_exchange_fail_relaxed(self.inner.get(),&mut current as *mut bool as *mut u8,val as u8,o)}
            _ => panic!("Invalid ordering pair for compare_exchange")
        }{
            Ok(expected)
        }else{
            Err(expected)
        }
    }

    pub fn fetch_and(&self,)
}

mod ops{
    use crate::sync::atomic::Ordering::{self,*};

    pub unsafe fn atomic_load<T>(mem: *const T, ord: Ordering){
        if ::__lccc::atomic_not_lock_free::<T>() {
            panic!("Cannot perform non-lock free atomic load")
        }
        match ord{
            Acquire | AcqRel => __lccc::xir!(r"(
                indirect
                as_rvalue atomic acquire
            )":[mem]:[yield: T]),
            SeqCst => __lccc::xir!(r"(
                indirect
                as_rvalue atomic seqcst
            )":[mem]:[yield: T]),
            Relaxed => __lccc::xir!(r"
                indirect
                as_rvalue atomic
            ":[mem]:[yield:T]),
            _ => panic!("Bad ordering")
        }
    }

    pub unsafe fn atomic_store<T>(mem: *mut T,val: T, ord: Ordering){
        if ::__lccc::atomic_not_lock_free::<T>() {
            panic!("Cannot perform non-lock free atomic store")
        }

        match ord{
            Release | AcqRel => __lccc::xir!(r"
                assign atomic release
            ":[lvalue *mem,val]),
            Relaxed => __lccc::xir!(r"
                assign atomic
            ":[lvalue *mem,val]),
            SeqCst => __lccc::xir!(r"
                assign atomic seq_cst
            ":[lvalue *mem,val]),
            _ => panic!("Bad ordering")
        }
    }

    pub unsafe fn atomic_compare_exchange<T>(mem: *mut T,expected: *mut T,value: T,ord: Ordering) -> bool{
        if ::__lccc::atomic_not_lock_free::<T>() {
            panic!("Cannot perform non-lock free atomic store")
        }

        match ord{
            Acquire => __lccc::xir!(r"
                cmp_excg atomic acquire
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            Release => __lccc::xir!(r"
                cmp_excg atomic release
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            AcqRel => __lccc::xir!(r"
                cmp_excg atomic acqrel
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            Relaxed => __lccc::xir!(r"
                cmp_excg atomic relaxed
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            SeqCst => __lccc::xir!(r"
                cmp_excg atomic seq_cst
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool])
        }
    }

    pub unsafe fn atomic_compare_exchange_fail_relaxed<T>(mem: *mut T,expected: *mut T,value: T,ord: Ordering) -> bool{
        if ::__lccc::atomic_not_lock_free::<T>() {
            panic!("Cannot perform non-lock free atomic store")
        }

        match ord{
            Acquire => __lccc::xir!(r"
                cmp_excg atomic acquire fail relaxed
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            Release => __lccc::xir!(r"
                cmp_excg atomic release fail relaxed
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            AcqRel => __lccc::xir!(r"
                cmp_excg atomic acqrel fail relaxed
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            Relaxed => __lccc::xir!(r"
                cmp_excg atomic relaxed
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            SeqCst => __lccc::xir!(r"
                cmp_excg atomic seq_cst fail relaxed
                convert strong uint(1)
            ":[lvalue unsafe{*mem},lvalue *expected,val]:[yield:bool])
        }
    }

    pub unsafe fn atomic_compare_exchange_weak<T>(mem: *mut T,expected: *mut T,value: T,ord: Ordering) -> bool{
        if ::__lccc::atomic_not_lock_free::<T>() {
            panic!("Cannot perform non-lock free atomic store")
        }

        match ord{
            Acquire => __lccc::xir!(r"
                cmp_excg_weak atomic acquire
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            Release => __lccc::xir!(r"
                cmp_excg_weak atomic release
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            AcqRel => __lccc::xir!(r"
                cmp_excg_weak atomic acqrel
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            Relaxed => __lccc::xir!(r"
                cmp_excg_weak atomic relaxed
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            SeqCst => __lccc::xir!(r"
                cmp_excg_weak atomic seq_cst
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool])
        }
    }

    pub unsafe fn atomic_compare_exchange_weak_fail_relaxed<T>(mem: *mut T,expected: *mut T,value: T,ord: Ordering) -> bool{
        if ::__lccc::atomic_not_lock_free::<T>() {
            panic!("Cannot perform non-lock free atomic store")
        }

        match ord{
            Acquire => __lccc::xir!(r"
                cmp_excg_weak atomic acquire fail relaxed
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            Release => __lccc::xir!(r"
                cmp_excg_weak atomic release fail relaxed
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            AcqRel => __lccc::xir!(r"
                cmp_excg_weak atomic acqrel fail relaxed
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            Relaxed => __lccc::xir!(r"
                cmp_excg_weak atomic relaxed
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            SeqCst => __lccc::xir!(r"
                cmp_excg_weak atomic seq_cst fail relaxed
                convert strong uint(1)
            ":[lvalue unsafe{*mem},lvalue *expected,val]:[yield:bool])
        }
    }

    pub unsafe fn atomic_swap<T>(mem1: *mut T,mem2: *mut T,ord: Ordering){
        if ::__lccc::atomic_not_lock_free::<T>() {
            panic!("Cannot perform non-lock free atomic store")
        }

        match ord{
            Acquire => __lccc::xir!(r"
                swap atomic acquire
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected]:[yield:bool]),
            Release => __lccc::xir!(r"
                swap atomic release
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected]:[yield:bool]),
            AcqRel => __lccc::xir!(r"
                swap atomic acqrel
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected,val]:[yield:bool]),
            Relaxed => __lccc::xir!(r"
                swap atomic 
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected]:[yield:bool]),
            SeqCst => __lccc::xir!(r"
                swap atomic seq_cst
                convert strong uint(1)
            ":[lvalue *mem,lvalue *expected]:[yield:bool])
        }
    }


    pub fn fetch_and<T: And>(mem: *mut T,value: T,ord: Ordering) -> T{
        match ord{
            Relaxed => __lccc::xir!(r"
                compound_assign fetch and atomic relaxed 
            ":[lvalue *mem,value]:[yield:T]),
            Acquire => __lccc::xir!(r"
                compound_assign fetch and atomic release 
            ":[lvalue *mem,value]:[yield:T]),
            Release => __lccc::xir!(r"
                compound_assign fetch and atomic release 
            ":[lvalue *mem,value]:[yield:T]),
            AcqRel => __lccc::xir!(r"
                compound_assign fetch and atomic acqrel 
            ":[lvalue *mem,value]:[yield:T]),
            SeqCst => __lccc::xir!(r"
                compound_assign fetch and atomic seqcst 
            ":[lvalue *mem,value]:[yield:T]),
        }
    }



}