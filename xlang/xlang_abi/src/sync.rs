use core::cell::UnsafeCell;
use core::mem::MaybeUninit;
use core::ops::Deref;
pub use core::sync::atomic;

use crate::ops::{ChangeOutputType, Residual, Try};

use crate::try_;

extern "C" {
    /// Yields the current thread to the scheduler, if applicable
    ///
    /// This function is considered safe to execute
    pub fn xlang_yield_now();
}

/// A cell that can be written to only once.
///
/// This is a thread-safe type that is safe to use from multiple threads.
/// If multiple threads attempt to initialize it at once, all but one thread is blocked, and, when resumed, will not attempt to reinitalize the cell if initialization was completed.
#[repr(C)]
pub struct OnceCell<T> {
    flag: atomic::AtomicUsize,
    content: UnsafeCell<MaybeUninit<T>>,
}

unsafe impl<T: Send + Sync> Sync for OnceCell<T> {}

const ONCE_CELL_LOCKED: usize = 0x01;
const ONCE_CELL_INIT: usize = 0x02;

impl<T> OnceCell<T> {
    /// Constructs a new, empty, [`OnceCell`]
    pub const fn new() -> Self {
        Self {
            flag: atomic::AtomicUsize::new(0),
            content: UnsafeCell::new(MaybeUninit::uninit()),
        }
    }

    /// Constructs a new, initialized [`OnceCell`]
    ///
    /// This may be used if, based on a compile-time condition, you may be able to const-construct the value
    pub const fn new_init(x: T) -> Self {
        Self {
            flag: atomic::AtomicUsize::new(ONCE_CELL_INIT),
            content: UnsafeCell::new(MaybeUninit::new(x)),
        }
    }

    unsafe fn begin_init(&self) -> usize {
        let mut counter = 0usize;

        let mut val = self
            .flag
            .fetch_or(ONCE_CELL_LOCKED, atomic::Ordering::Relaxed);

        while (val & ONCE_CELL_LOCKED) != 0 {
            counter += 1;
            if counter < 32 {
                core::hint::spin_loop()
            } else {
                // SAFETY: `xlang_yield_now()` is a safe function
                unsafe {
                    xlang_yield_now();
                }
            }

            val = self
                .flag
                .fetch_or(ONCE_CELL_LOCKED, atomic::Ordering::Relaxed);
        }
        atomic::fence(atomic::Ordering::Acquire);

        val
    }

    unsafe fn abort_init(&self) {
        self.flag.store(0, atomic::Ordering::Release)
    }

    unsafe fn finish_init(&self) {
        self.flag.store(ONCE_CELL_INIT, atomic::Ordering::Release)
    }

    /// If the cell is initialized, returns a reference to it,
    /// otherwise returns `None`
    pub fn get(&self) -> Option<&T> {
        if (self.flag.load(atomic::Ordering::Acquire) & ONCE_CELL_INIT) != 0 {
            // Safety: We just confirmed that the cell has been initialized
            Some(unsafe { &*(self.content.get() as *mut T) })
        } else {
            None
        }
    }

    /// Initializes the cell with the provided value if it is empty, then returns a shared reference to the value
    ///
    /// `val` is eagerly evaluated, even if the cell is initialized, so this function should be used only if `T` is cheap to both construct and destroy
    pub fn get_or_insert(&self, val: T) -> &T {
        let ptr = self.content.get() as *mut T;
        if (self.flag.load(atomic::Ordering::Acquire) & ONCE_CELL_INIT) != 0 {
            unsafe { &*ptr }
        } else {
            // SAFETY: We don't own the lock
            let state = unsafe { self.begin_init() };

            if (state & ONCE_CELL_INIT) == 0 {
                // Safety:
                // 1. Points to the interior of an `UnsafeCell` and thus live because of the `self` shared reference
                // 2. We own the once lock, so no other threads can reach this point
                // 3. No shared refernces to the `T` exist because we haven't init the cell yet, thus no `get` functions have returned yet.
                unsafe {
                    ptr.write(val);
                }
            }

            // SAFETY:
            // 1. We own the lock current because of the return from `begin_init`.
            // 2. Either the cell is already written to by another thread, or we just initialized it
            unsafe {
                self.finish_init();
            }

            unsafe { &*ptr }
        }
    }

    /// Initializes the cell with value returned by the provided constructor if it is empty, then returns a shared reference to the value.
    ///
    /// `ctor` is lazily evaluated - it is only called if the cell is empty. This is useful if it performs an expensive operation
    ///
    /// ## Panics
    /// If `ctor` panics, the panic is forwarded. The cell is left in an uninitialized state if this occurs.
    pub fn get_or_insert_with<F: FnOnce() -> T>(&self, ctor: F) -> &T {
        let ptr = self.content.get() as *mut T;
        if (self.flag.load(atomic::Ordering::Acquire) & ONCE_CELL_INIT) != 0 {
            unsafe { &*ptr }
        } else {
            // SAFETY: We don't own the lock
            let state = unsafe { self.begin_init() };

            if (state & ONCE_CELL_INIT) == 0 {
                struct Guard<'a, T>(&'a OnceCell<T>);

                impl<'a, T> Drop for Guard<'a, T> {
                    fn drop(&mut self) {
                        unsafe { self.0.abort_init() }
                    }
                }

                let guard = Guard(self);

                // Safety:
                // 1. Points to the interior of an `UnsafeCell` and thus live because of the `self` shared reference
                // 2. We own the once lock, so no other threads can reach this point
                // 3. No shared refernces to the `T` exist because we haven't init the cell yet, thus no `get` functions have returned yet.
                unsafe {
                    ptr.write(ctor());
                }

                // Guard aborts initialization if `ctor()` panics
                core::mem::forget(guard);
            }

            // SAFETY:
            // 1. We own the lock current because of the return from `begin_init`.
            // 2. Either the cell is already written to by another thread, or we just initialized it
            unsafe {
                self.finish_init();
            }

            unsafe { &*ptr }
        }
    }

    /// Attempts to initialize the cell with `ctor` if it is empty, and returns a reference to it.
    ///
    /// If `ctor` returns an error, the error is forwarded and the cell is left in an uninitialized state
    ///
    /// ## Panics
    ///
    /// If `ctor` panics, the panic is forwarded. The cell is left in an uninitialized state
    pub fn get_or_try_insert_with<'a, R: Try<Output = T>, F: FnOnce() -> R>(
        &'a self,
        ctor: F,
    ) -> ChangeOutputType<R, &T>
    where
        R::Residual: Residual<&'a T>,
    {
        let ptr = self.content.get() as *mut T;
        if (self.flag.load(atomic::Ordering::Acquire) & ONCE_CELL_INIT) != 0 {
            Try::from_output(unsafe { &*ptr })
        } else {
            // SAFETY: We don't own the lock
            let state = unsafe { self.begin_init() };

            if (state & ONCE_CELL_INIT) == 0 {
                struct Guard<'a, T>(&'a OnceCell<T>);

                impl<'a, T> Drop for Guard<'a, T> {
                    fn drop(&mut self) {
                        unsafe { self.0.abort_init() }
                    }
                }

                let guard = Guard(self);

                // Safety:
                // 1. Points to the interior of an `UnsafeCell` and thus live because of the `self` shared reference
                // 2. We own the once lock, so no other threads can reach this point
                // 3. No shared refernces to the `T` exist because we haven't init the cell yet, thus no `get` functions have returned yet.
                unsafe {
                    ptr.write(try_!(ctor()));
                }

                // Guard aborts initialization if `ctor()` panics
                core::mem::forget(guard);
            }

            // SAFETY:
            // 1. We own the lock current because of the return from `begin_init`.
            // 2. Either the cell is already written to by another thread, or we just initialized it
            unsafe {
                self.finish_init();
            }

            Try::from_output(unsafe { &*ptr })
        }
    }
}

/// A Lazily-initialized value
///
/// This is not an ABI-Safe type, as it is likely that `F` is a closure, which does not have a stable ABI.
/// It is provided for a convience-wrapper arround `OnceCell` and cannot be used at
pub struct Lazy<T, F>(OnceCell<T>, F);

impl<T, F> Lazy<T, F> {
    /// Construts a new `Lazy` with a given constructor function
    pub const fn new(f: F) -> Self {
        Self(OnceCell::new(), f)
    }

    /// Tries to obtain a shared reference to the interior of the cell, returning `Some` if it hasn't been initialized yet
    pub fn get(&self) -> Option<&T> {
        self.0.get()
    }
}

impl<T, F: Fn() -> T> Deref for Lazy<T, F> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.get_or_insert_with(&self.1)
    }
}
