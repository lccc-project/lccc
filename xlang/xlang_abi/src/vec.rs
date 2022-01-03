use std::{
    fmt::Debug,
    hash::Hash,
    io::Write,
    iter::FromIterator,
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
};

use crate::{
    alloc::{Allocator, Layout, XLangAlloc},
    ptr::Unique,
};

/// An ABI Safe implementation of [`std::vec::Vec`]
#[repr(C)]
pub struct Vec<T, A: Allocator = XLangAlloc> {
    ptr: Unique<T>,
    cap: usize,
    len: usize,
    alloc: A,
}

impl<T> Vec<T, XLangAlloc> {
    /// Creates a new (empty) Vec
    #[must_use]
    pub fn new() -> Self {
        Self::new_in(XLangAlloc::new())
    }

    /// Allocates a vector with a given capacity
    /// # Panics
    /// Panics if the new capacity in bytes exceeds [`isize::MAX`]
    #[must_use]
    pub fn with_capacity(cap: usize) -> Self {
        Self::with_capacity_in(cap, XLangAlloc::new())
    }

    /// Creates a vector from raw parts,
    ///
    /// # Safety
    /// The following preconditions must hold:
    /// * ptr must have been allocated using an Allocator compatible with [`XLangAlloc`], with the layout `Layout::array::<T>(cap)`,
    /// * ptr must not have been deallocated,
    /// * After this call, `ptr` must not be used to access any memory,
    /// * `len` must be less than `cap`
    /// * The range `[ptr,ptr.add(len))` must contain valid values of type `T`
    pub unsafe fn from_raw_parts(ptr: *mut T, cap: usize, len: usize) -> Self {
        Self::from_raw_parts_in(ptr, cap, len, XLangAlloc::new())
    }
}

impl<T, A: Allocator> Drop for Vec<T, A> {
    fn drop(&mut self) {
        if core::mem::needs_drop::<T>() {
            let ptr = self.ptr.as_ptr();
            for i in 0..self.len {
                unsafe { core::ptr::drop_in_place(ptr.add(i)) }
            }
        }
        let layout = unsafe {
            Layout::from_size_align_unchecked(
                self.cap.checked_mul(core::mem::size_of::<T>()).unwrap(),
                core::mem::align_of::<T>(),
            )
        };
        if layout.size() != 0 {
            unsafe {
                self.alloc.deallocate(self.ptr.as_nonnull().cast(), layout);
            }
        }
    }
}

impl<T, A: Allocator> Vec<T, A> {
    /// Creates a new (empty) Vec in `alloc` without allocating.
    pub fn new_in(alloc: A) -> Self {
        Self {
            ptr: Unique::dangling(),
            cap: 0,
            len: 0,
            alloc,
        }
    }

    /// Allocates a new Vec in `alloc` that can store at least `cap` elements before reallocating
    /// This function does not allocate if `cap` is `0` or `T` is a ZST
    ///
    /// # Panics
    /// Panics if the new capacity in bytes exceeds [`isize::MAX`]
    pub fn with_capacity_in(cap: usize, alloc: A) -> Self {
        let cap = cap.checked_next_power_of_two().unwrap();
        let raw_cap = cap.checked_mul(core::mem::size_of::<T>()).unwrap();
        if raw_cap == 0 {
            Self {
                ptr: Unique::dangling(),
                cap,
                len: 0,
                alloc,
            }
        } else {
            let ptr = alloc
                .allocate(unsafe {
                    Layout::from_size_align_unchecked(raw_cap, core::mem::align_of::<T>())
                })
                .unwrap_or_else(|| {
                    crate::alloc::handle_alloc_error(unsafe {
                        Layout::from_size_align_unchecked(raw_cap, core::mem::align_of::<T>())
                    })
                })
                .cast();
            Self {
                ptr: unsafe { Unique::new_nonnull_unchecked(ptr) },
                cap,
                len: 0,
                alloc,
            }
        }
    }

    /// Creates a vector from raw parts, in `alloc`
    ///
    /// # Safety
    /// The following preconditions must hold:
    /// * ptr must have been allocated using an Allocator compatible with `alloc`, with the layout `Layout::array::<T>(cap)`,
    /// * ptr must not have been deallocated,
    /// * After this call, `ptr` must not be used to access any memory,
    /// * `len` must be less than `cap`
    /// * The range `[ptr,ptr.add(len))` must contain valid values of type `T`
    pub unsafe fn from_raw_parts_in(ptr: *mut T, cap: usize, len: usize, alloc: A) -> Self {
        Self {
            ptr: Unique::new_unchecked(ptr),
            cap,
            len,
            alloc,
        }
    }

    /// Returns a reference to the allocator used by self
    pub fn allocator(&self) -> &A {
        &self.alloc
    }

    /// Converts self into its raw parts, suitable to be passed to [`Vec::from_raw_parts`]
    /// The first element is the pointer, the second element is the capacity, and the third element is the length of the Vector
    pub fn into_raw_parts(self) -> (*mut T, usize, usize) {
        let this = ManuallyDrop::new(self);
        let ptr = this.ptr.as_ptr();
        let cap = this.cap;
        let len = this.len;
        (ptr, cap, len)
    }

    /// Converts self into its raw parts, including the allocator, suitable to be passed to [`Vec::from_raw_parts_in`]
    /// The first element is the pointer, the second element is the capacity, the third element is the length of the Vector, and the fourth element is the allocator
    pub fn into_raw_parts_with_alloc(self) -> (*mut T, usize, usize, A) {
        let this = ManuallyDrop::new(self);
        let ptr = this.ptr.as_ptr();
        let cap = this.cap;
        let len = this.len;
        let alloc = unsafe { core::ptr::read(&this.alloc) };
        (ptr, cap, len, alloc)
    }

    fn reallocate(&mut self, ncap: usize) {
        if ncap > self.cap {
            let raw_cap = ncap.checked_mul(core::mem::size_of::<T>()).unwrap();
            if raw_cap == 0 {
                return;
            }

            let ptr = unsafe {
                self.alloc.grow(
                    self.ptr.as_nonnull().cast(),
                    Layout::from_size_align_unchecked(
                        self.cap * core::mem::size_of::<T>(),
                        core::mem::align_of::<T>(),
                    ),
                    Layout::from_size_align_unchecked(raw_cap, core::mem::align_of::<T>()),
                )
            }
            .unwrap_or_else(|| {
                crate::alloc::handle_alloc_error(unsafe {
                    Layout::from_size_align_unchecked(
                        self.cap * core::mem::size_of::<T>(),
                        core::mem::align_of::<T>(),
                    )
                })
            });
            self.cap = ncap;
            self.ptr = unsafe { Unique::new_nonnull_unchecked(ptr.cast()) };
        }
    }

    /// Unsafely sets the length of the vector, without checking, or dropping any elements.
    ///
    /// ## Safety
    /// The following preconditions must hold:
    /// * len shall be less than `self.capacity()`
    /// * the first `len` elements of self must be valid values of type `T`
    ///
    /// ## Notes
    /// If len is less than `self.len()`, then this function will leak the excess elements (unless the length is restored before dropping the Vec or overwriting them)
    pub unsafe fn set_len(&mut self, len: usize) {
        self.len = len;
    }

    /// Appends a new element to the back of the Vec, reallocating if necessary.
    ///
    /// # Panics
    /// Panics if the new capacity in bytes exceeds [`isize::MAX`]
    pub fn push(&mut self, val: T) {
        if self.len == self.cap {
            let ncap = (self.cap + 1).checked_next_power_of_two().unwrap();
            self.reallocate(ncap);
        }

        let ptr = self.ptr.as_ptr();
        unsafe {
            ptr.add(self.len).write(val);
        }
        self.len += 1;
    }

    /// Removes and returns the last element of the Vec, without reallocating.
    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            let ptr = self.ptr.as_ptr();
            unsafe { Some(ptr.add(self.len).read()) }
        }
    }

    /// Reserves sufficient capacity for this Vec such that at least `additional` more elements can be pushed without reallocating
    ///
    /// # Panics
    /// Panics if the new capacity in bytes exceeds [`isize::MAX`]
    pub fn reserve(&mut self, additional: usize) {
        let ncap = self
            .len
            .checked_add(additional)
            .unwrap()
            .checked_next_power_of_two()
            .unwrap();
        if self.cap < ncap {
            self.reallocate(ncap);
        }
    }

    /// Leaks self into a slice of `T`, without deallocating any elements or excess capacity
    pub fn leak<'a>(self) -> &'a mut [T]
    where
        A: 'a,
    {
        let this = ManuallyDrop::new(self);
        let ptr = this.ptr.as_ptr();
        let len = this.len;
        unsafe { core::slice::from_raw_parts_mut(ptr, len) }
    }

    /// Returns the capacity of the [`Vec`], that is, the total number of elements the [`Vec`] can hold without reallocating
    pub fn capacity(&self) -> usize {
        self.cap
    }

    /// Clears this vector, dropping each element, without changing capacity.
    ///
    /// ## Panic Safety
    /// If the destructor of any element panics, then which elements are dropped are unspecified, and the remaining elements are leaked (but the vector will still be empty).
    pub fn clear(&mut self) {
        let len = self.len;
        self.len = 0;
        if core::mem::needs_drop::<T>() {
            let ptr = self.ptr.as_ptr();
            for i in 0..len {
                unsafe { core::ptr::drop_in_place(ptr.add(i)) }
            }
        }
    }

    /// Clones the elements of `x` and pushes them into the Vec, reallocating at most once.
    pub fn extend_from_slice(&mut self, x: &[T])
    where
        T: Clone,
    {
        self.reserve(x.len());
        for v in x {
            self.push(v.clone());
        }
    }

    /// Returns a mutable pointer to the first element of the Vec
    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.ptr.as_ptr()
    }

    /// Returns a const pointer to the first element of the Vec.
    /// The pointer may not be used to write to the elements
    pub fn as_ptr(&self) -> *const T {
        self.ptr.as_ptr()
    }
}

impl<T, A: Allocator + Default> FromIterator<T> for Vec<T, A> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let (size, _) = iter.size_hint();
        let mut ret = Self::with_capacity_in(size, Default::default());
        for v in iter {
            ret.push(v);
        }
        ret
    }
}

impl<T, A: Allocator> Extend<T> for Vec<T, A> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        let iter = iter.into_iter();
        let (size, _) = iter.size_hint();

        self.reserve(size);
        for v in iter {
            self.push(v);
        }
    }
}

impl<T, A: Allocator> Deref for Vec<T, A> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        let ptr = self.ptr.as_ptr();
        unsafe { core::slice::from_raw_parts(ptr, self.len) }
    }
}

impl<T, A: Allocator> DerefMut for Vec<T, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let ptr = self.ptr.as_ptr();
        unsafe { core::slice::from_raw_parts_mut(ptr, self.len) }
    }
}

impl<T: Clone, A: Allocator + Clone> Clone for Vec<T, A> {
    // Specialization would be nice here
    fn clone(&self) -> Self {
        let nalloc = self.alloc.clone();
        let mut nvec = Self::with_capacity_in(self.len, nalloc);
        let ptr = nvec.ptr.as_ptr();
        for i in 0..self.len {
            unsafe {
                core::ptr::write(ptr.add(i), self[i].clone());
            }
        }

        unsafe {
            nvec.set_len(self.len);
        }

        nvec
    }
}

impl<'a, T, A: Allocator> IntoIterator for &'a Vec<T, A> {
    type IntoIter = core::slice::Iter<'a, T>;
    type Item = &'a T;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T, A: Allocator> IntoIterator for &'a mut Vec<T, A> {
    type IntoIter = core::slice::IterMut<'a, T>;
    type Item = &'a mut T;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<T: Hash, A: Allocator> Hash for Vec<T, A> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for i in 0..self.len() {
            self[i].hash(state);
        }
    }
}

impl<U, T: PartialEq<U>, A: Allocator, A1: Allocator> PartialEq<Vec<U, A1>> for Vec<T, A> {
    fn eq(&self, other: &Vec<U, A1>) -> bool {
        self.len() == other.len() && self.iter().zip(other).all(|(a, b)| a.eq(b))
    }
}

impl<T: Eq, A: Allocator> Eq for Vec<T, A> {}

impl<T, A: Allocator + Default> Default for Vec<T, A> {
    fn default() -> Self {
        Self::new_in(Default::default())
    }
}

impl<T: Debug, A: Allocator> Debug for Vec<T, A> {
    fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
        <[T]>::fmt(self, fmt)
    }
}

impl<A: Allocator> Write for Vec<u8, A> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.reserve(buf.len());
        unsafe {
            core::ptr::copy_nonoverlapping(
                buf.as_ptr(),
                self.ptr.as_ptr().add(self.len),
                buf.len(),
            );
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl<T: Clone, A: Allocator + Default, S: AsRef<[T]> + ?Sized> From<&S> for Vec<T, A> {
    fn from(s: &S) -> Self {
        let s = s.as_ref();
        if s.is_empty() {
            Self::new_in(Default::default())
        } else {
            let mut v = Self::with_capacity_in(s.len(), Default::default());
            v.extend_from_slice(s);
            v
        }
    }
}

impl<T, A: Allocator + Default> From<std::vec::Vec<T>> for Vec<T, A> {
    fn from(s: std::vec::Vec<T>) -> Self {
        if s.is_empty() {
            Self::new_in(Default::default())
        } else {
            let mut s = ManuallyDrop::new(s);
            let mut v = Self::with_capacity_in(s.len(), Default::default());
            let len = s.len();
            let dst = v.as_mut_ptr();
            let src = s.as_ptr();
            unsafe {
                core::ptr::copy_nonoverlapping(src, dst, len);
            }
            unsafe {
                v.set_len(len);
            }
            unsafe {
                s.set_len(0);
            }
            drop(ManuallyDrop::into_inner(s));
            v
        }
    }
}

/// Constructs a new `vec` containing the given elements.
#[macro_export]
macro_rules! vec{
    [$($elems:expr),* $(,)?] => {
        {
            let s = ::core::mem::ManuallyDrop::new([$($elems),*]);
            let mut vec = $crate::vec::Vec::with_capacity(s.len());
            let ptr = vec.as_mut_ptr();
            unsafe{core::ptr::copy_nonoverlapping(s.as_ptr(),ptr,s.len());}
            unsafe{vec.set_len(s.len())}
            vec
        }
    };
    [$elem:expr ; $repeat:expr] => {
        {
            let __repeat: ::core::primitive::usize = $repeat
            fn __check<T: ::core::clone::Clone>(x: &T){}
            let val = $elem;
            __check(&val);
            let mut vec = $crate::vec::Vec::with_capacity(__repeat);
            for i in 0..($repeat){
                vec.push_back(val.clone())
            }
            vec
        }

    }
}
