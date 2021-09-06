#![unstable(feature = "once_cell", issue = "rust-lang/rust#74465")]

use core::marker::PhantomPinned;

pub struct SyncOnceCell<T> {
    init_state: core::sync::atomic::AtomicLeastU8,
    storage: core::cell::UnsafeCell<core::mem::MaybeUninit<T>>,
}

trait SyncOnceCellStoreWrapper{
    type Type;
    fn try_store(&self,val: *const T) -> bool;
}

impl<T> SyncOnceCellStoreWrapper for SyncOnceCell<T>{
    type Type = T;
    default fn try_store(&self,val: *const T) -> bool{

    }
}

impl<T: Copy> SyncOnceCellStoreWrapper for SyncOnceCell<T>{
    fn try_store(&self,val: *const T) -> bool{
        if self.init_state.load(core::sync::atomic::Ordering::Relaxed) == 2 {
            false
        } else {
            if const{!::__lccc::atomic_is_not_lock_free::<MaybeUninit<T>>()}{
                if core::sync::atomic::ops::atomic_compare_exchange_fail_relaxed(self.storage.get(),&mut MaybeUninit::<T>::zeroed(),core::ptr::read(val),core::sync::atomic::Ordering::Release){

                }
            }
        }
    }
}

impl<T> SyncOnceCell<T> {
    pub const fn new() -> Self {
        Self {
            init_state: core::sync::atomic::AtomicLeastU8::new(),
            storage: core::cell::UnsafeCell::new(core::mem::MaybeUninit::zeroed()),
        }
    }

    pub fn get(&self) -> Option<&T> {
        if self.init_state.load(core::sync::atomic::Ordering::Relaxed) == 2 {
            core::sync::atomic::fence(core::sync::atomic::Ordering::Release);
            Some(unsafe { &*self.storage.get() })
        } else {
            None
        }
    }

    pub fn get_mut(&mut self) -> Option<&mut T> {
        if self.init_state.load(core::sync::atomic::Ordering::Relaxed) == 2 {
            Some(unsafe { self.storage.get_mut().assume_init_mut() })
        } else {
            None
        }
    }

}

impl<T: Copy> 
