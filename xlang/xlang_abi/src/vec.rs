use std::{
    fmt::Debug,
    hash::Hash,
    io::Write,
    iter::{FromIterator, FusedIterator},
    marker::PhantomData,
    mem::{size_of, ManuallyDrop},
    ops::{Bound, Deref, DerefMut, RangeBounds},
    ptr::NonNull,
};

use crate::{
    alloc::{Allocator, Layout, XLangAlloc},
    ptr::Unique,
    span::SpanMut,
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
    pub const fn new() -> Self {
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
    /// * ptr must have been allocated using an Allocator compatible with [`XLangAlloc`], with the layout `Layout::array::<T>(cap).align_to_fundamental()`,
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
        }
        .align_to_fundamental()
        .unwrap();
        if layout.size() != 0 {
            unsafe {
                self.alloc.deallocate(self.ptr.as_nonnull().cast(), layout);
            }
        }
    }
}

impl<T, A: Allocator> Vec<T, A> {
    /// Creates a new (empty) Vec in `alloc` without allocating.
    pub const fn new_in(alloc: A) -> Self {
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
            let layout =
                unsafe { Layout::from_size_align_unchecked(raw_cap, core::mem::align_of::<T>()) };

            let layout = layout.align_to_fundamental().unwrap().pad_to_align();
            let ptr = alloc
                .allocate(layout)
                .unwrap_or_else(|| crate::alloc::handle_alloc_error(layout))
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
    /// * ptr must have been allocated using an Allocator compatible with `alloc`, with the layout `Layout::array::<T>(cap).align_to_fundamental()`,
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
                let old_layout = Layout::from_size_align_unchecked(
                    self.cap * core::mem::size_of::<T>(),
                    core::mem::align_of::<T>(),
                )
                .align_to_fundamental()
                .unwrap();
                let new_layout =
                    Layout::from_size_align_unchecked(raw_cap, core::mem::align_of::<T>())
                        .align_to_fundamental()
                        .unwrap();
                self.alloc
                    .grow(self.ptr.as_nonnull().cast(), old_layout, new_layout)
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
    #[inline]
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

    /// Appends a new element to the back of the Vec, reallocating if necessary.
    /// Returns a mutable reference to the newly inserted elements
    /// # Panics
    /// Panics if the new capacity in bytes exceeds [`isize::MAX`]
    #[inline]
    #[must_use = "If you do not need the return value, consider using `Vec::push` instead"]
    pub fn push_mut(&mut self, val: T) -> &mut T {
        if self.len == self.cap {
            let ncap = (self.cap + 1).checked_next_power_of_two().unwrap();
            self.reallocate(ncap);
        }

        let ptr = self.ptr.as_ptr();
        unsafe {
            let ptr = ptr.add(self.len);
            self.len += 1;
            ptr.write(val);
            &mut *ptr
        }
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
    ///
    /// This will permanently leak the allocation and it's elements
    pub fn leak<'a>(self) -> &'a mut [T]
    where
        A: 'a,
    {
        let this = ManuallyDrop::new(self);
        let ptr = this.ptr.as_ptr();
        let len = this.len;
        unsafe { core::slice::from_raw_parts_mut(ptr, len) }
    }

    /// Leaks self into a mutable span of `T`, without deallocating any elements or excess capacity
    ///
    /// This will permanently leak the allocation and its elements.
    ///
    /// This function is a convience over `SpanMut::new(self.leak())`
    pub fn leak_span<'a>(self) -> SpanMut<'a, T>
    where
        A: 'a,
    {
        let this = ManuallyDrop::new(self);
        let ptr = this.ptr.as_ptr();
        let len = this.len;
        unsafe { SpanMut::from_raw_parts(ptr, len) }
    }

    /// Returns the capacity of the [`Vec`], that is, the total number of elements the [`Vec`] can hold without reallocating
    pub const fn capacity(&self) -> usize {
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

    /// Extends the [`Vec`] by [`Clone`]ing the elements from `range`
    ///
    /// ## Panics
    /// Panics if the range ends out of bounds of the [`Vec`], or the end of the range is placed before the beginning.
    pub fn extend_from_within<R: RangeBounds<usize>>(&mut self, range: R)
    where
        T: Clone,
    {
        let begin = match range.start_bound() {
            Bound::Included(x) => *x,
            Bound::Excluded(x) => *x + 1,
            Bound::Unbounded => 0,
        };

        let end = match range.end_bound() {
            Bound::Included(x) => *x + 1,
            Bound::Excluded(x) => *x,
            Bound::Unbounded => self.len,
        };

        if end < begin {
            panic!("Range beginnning {begin} must be less than or equal to range end {end}");
        }

        if self.len < end {
            panic!("Range {begin}..{end} is out of bounds");
        }

        let len = end - begin;

        self.reserve(len);
        let ptr = self.ptr.as_ptr();
        let read_ptr = unsafe { ptr.add(begin) };
        let write_ptr = unsafe { ptr.add(self.len) };

        if is_copy::<T>() {
            unsafe { core::ptr::copy_nonoverlapping(read_ptr, write_ptr, len) }
        } else {
            for i in 0..len {
                unsafe {
                    write_ptr.add(i).write((&*read_ptr.add(i)).clone());
                }
            }
        }

        self.len += len;
    }

    /// Clones the elements of `x` and pushes them into the Vec, reallocating at most once.
    pub fn extend_from_slice(&mut self, x: &[T])
    where
        T: Clone,
    {
        self.reserve(x.len());

        if is_copy::<T>() {
            let write_ptr = unsafe { self.ptr.as_ptr().add(self.len) };
            let read_ptr = x.as_ptr();
            let len = x.len();
            unsafe {
                core::ptr::copy_nonoverlapping(read_ptr, write_ptr, len);
            }
            self.len += len;
        } else {
            for v in x {
                self.push(v.clone());
            }
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

    /// Removes the elements from `n` to the end and returns them in a new [`Vec`]
    ///
    /// # Panics
    ///
    /// If `n` is greater than len
    #[must_use = "If you don't need the split off elements use `Self::shrink` instead"]
    pub fn split_off(&mut self, n: usize) -> Self
    where
        A: Clone,
    {
        assert!(n <= self.len);
        let nlen = self.len - n;
        let mut new = Self::with_capacity_in(nlen, self.alloc.clone());
        unsafe {
            self.set_len(n);
        }
        let src = unsafe { self.ptr.as_ptr().add(n) };
        let dst = new.ptr.as_ptr();
        unsafe { core::ptr::copy_nonoverlapping(src, dst, nlen) }
        unsafe {
            new.set_len(nlen);
        }
        new
    }

    /// Removes the last `n` returns them in a new [`Vec`]
    ///
    /// # Panics
    ///
    /// If `n` is greater than len
    #[must_use = "If you don't need the split off elements use `Self::shrink` instead"]
    pub fn split_off_back(&mut self, n: usize) -> Self
    where
        A: Clone,
    {
        assert!(n <= self.len);
        let nlen = self.len - n;
        let mut new = Self::with_capacity_in(n, self.alloc.clone());
        unsafe {
            self.set_len(nlen);
        }
        let src = unsafe { self.ptr.as_ptr().add(nlen) };
        let dst = new.ptr.as_ptr();
        unsafe { core::ptr::copy_nonoverlapping(src, dst, n) }
        unsafe {
            new.set_len(n);
        }
        new
    }

    /// Returns an iterator that moves out of the back `n` elements of `self`.
    ///
    /// The iterator takes ownership of the values immediately - if dropped, the unconsumed values are dropped, and if forgetten the unconsumed values will also be forgotten.
    /// `self` remains borrowed for the lifetime of the iterator to avoid an extra allocation.
    ///
    /// ## Panics
    ///
    /// Panics if `n` is greater than `len()`
    ///
    #[allow(clippy::manual_assert)] // I don't want the extra "assertion failed" message.
    pub fn drain_back(&mut self, n: usize) -> DrainBack<T> {
        if n > self.len {
            panic!(
                "Index {} is out of range for a vector of length {}",
                n, self.len
            )
        }
        self.len -= n;
        let base = unsafe { self.ptr.as_ptr().add(self.len) };

        DrainBack {
            ptr: unsafe { NonNull::new_unchecked(base) },
            remaining: n,
            _phantom: PhantomData,
        }
    }

    ///
    /// Resizes the vector, shrinking it to `nlen`, dropping any excees elements
    ///
    /// # Panics
    /// Panics if `nlen` is greater than than `self.len()`
    pub fn shrink(&mut self, new_len: usize) {
        assert!(new_len <= self.len);

        let old_len = self.len;

        unsafe {
            self.set_len(new_len);
        }

        if core::mem::needs_drop::<T>() {
            let ptr = self.ptr.as_ptr();
            for i in new_len..old_len {
                unsafe { core::ptr::drop_in_place(ptr.add(i)) }
            }
        }
    }

    /// Inserts `val` into `pos`, shifting all elements after `pos` to the right
    #[allow(clippy::manual_assert)] // I don't want the extra "assertion failed" message.
    pub fn insert(&mut self, val: T, pos: usize) {
        if pos > self.len {
            panic!(
                "Cannot insert into pos {} for vec with length {}",
                pos, self.len
            );
        }

        if self.len == self.cap {
            if self.cap == 0 {
                self.reallocate(16);
            } else {
                self.reallocate(self.cap << 2);
            }
        }

        if core::mem::size_of::<T>() != 0 {
            let rest = self.len - pos;

            let loc = unsafe { self.ptr.as_ptr().add(pos) };

            unsafe {
                core::ptr::copy(loc, loc.offset(1), rest);
            }
            unsafe {
                core::ptr::write(loc, val);
            }
        } else {
            core::mem::forget(val);
        }

        self.len += 1;
    }

    /// Removes and returns the `i`th element of the vector, shifting all subsequent elements down.
    ///
    /// The relative order of remaining elements is preserved by this function
    ///
    /// ## Panics
    ///
    /// Panics if `i` is greater than or equal to `self.len()`
    #[allow(clippy::manual_assert)] // I don't want the extra "assertion failed" message.
    pub fn remove(&mut self, i: usize) -> T {
        if i >= self.len {
            panic!(
                "Index {} is out of range for a Vec of length {}",
                i, self.len
            );
        }
        let rest = self.len - i;
        let loc = unsafe { self.ptr.as_ptr().add(i) };
        let val = unsafe { core::ptr::read(loc) };
        unsafe {
            core::ptr::copy(loc.offset(1), loc, rest);
        }
        self.len -= 1;
        val
    }

    /// Removes and returns the `i`th element of the vector, replacing it with the last element of the vector.
    ///
    /// This operation does not preserve the relative order of the remaining elements
    ///
    /// ## Panics
    ///
    /// Panics if `i` is greater than or equal to `self.len()`
    #[allow(clippy::manual_assert)] // I don't want the extra "assertion failed" message.
    pub fn swap_remove(&mut self, i: usize) -> T {
        if i >= self.len {
            panic!(
                "Index {} is out of range for a Vec of length {}",
                i, self.len
            );
        }
        self.len -= 1;
        let loc = unsafe { self.ptr.as_ptr().add(i) };
        let end_loc = unsafe { self.ptr.as_ptr().add(self.len) };
        if loc != end_loc {
            unsafe { core::ptr::swap_nonoverlapping(loc, end_loc, 1) }
        }

        unsafe { core::ptr::read(end_loc) }
    }

    /// Inserts `val` into the `Vec` at a position such that the list is sorted in ascending order and returns that position.
    ///
    /// If an element that compares equal to `val` is already present in the list, the position relative to any such element is unspecified
    ///
    /// If `self` is not already properly sorted, or the `Ord` impl of `T` is not transitive and total, then the position is unspecified.
    /// Regardless, no element already preent in the list will be reordered relative to any other element
    ///
    /// ## Panics
    ///
    /// Panics if the new capacity exceeds `usize::MAX`, or `isize::MAX` bytes
    pub fn insert_sorted(&mut self, val: T) -> usize
    where
        T: Ord,
    {
        self.insert_sorted_by(val, Ord::cmp)
    }

    /// Inserts `val` into the `Vec` at a position such that the list is sorted in ascending order by the keys returned by `map` and returns that position.
    ///
    /// If an element that compares equal to `val` is already present in the list, the position relative to any such element is unspecified
    ///
    /// If `self` is not already properly sorted, or the `Ord` impl of `U` is not transitive and total, then the position is unspecified.
    /// Regardless, no element already preent in the list will be reordered relative to any other element
    /// ## Panics
    ///
    /// Panics if the new capacity exceeds `usize::MAX`, or `isize::MAX` bytes
    pub fn insert_sorted_by_key<U: Ord, F: Fn(&T) -> U>(&mut self, val: T, map: F) -> usize {
        self.insert_sorted_by(val, |a, b| map(a).cmp(&map(b)))
    }

    /// Inserts `val` into the `Vec` at a position such that the list is sorted in ascending order according to `cmp` and returns that position.
    ///
    /// If an element that compares equal to `val` is already present in the list, the position relative to any such element is unspecified
    ///
    /// If `self` is not already properly sorted according to `cmp`, or if `F` is not transitive and total, then the position is unspecified.
    /// Regardless, no element already preent in the list will be reordered relative to any other element
    /// ## Panics
    ///
    /// Panics if the new capacity exceeds `usize::MAX`, or `isize::MAX` bytes
    pub fn insert_sorted_by<F: Fn(&T, &T) -> core::cmp::Ordering>(
        &mut self,
        val: T,
        cmp: F,
    ) -> usize {
        let mut begin = 0;
        let mut end = self.len;

        loop {
            if end == begin {
                self.insert(val, begin);
                break end;
            }
            let pos = (end + begin) / 2;

            if let Some(old) = self.get(pos) {
                match cmp(old, &val) {
                    core::cmp::Ordering::Less => {
                        end = pos;
                        continue;
                    }
                    core::cmp::Ordering::Greater => {
                        begin = pos;
                        continue;
                    }
                    core::cmp::Ordering::Equal => {
                        self.insert(val, pos);
                        break pos;
                    }
                }
            } else {
                panic!()
            }
        }
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

#[inline(always)]
fn is_copy<T>() -> bool {
    struct TestIsCopy<T>(bool, PhantomData<T>);
    impl<T> Clone for TestIsCopy<T> {
        #[inline(always)]
        fn clone(&self) -> Self {
            Self(false, self.1)
        }
    }

    impl<T: Copy> Copy for TestIsCopy<T> {}

    [TestIsCopy(true, PhantomData::<T>)].clone()[0].0
}

impl<T: Clone, A: Allocator + Clone> Clone for Vec<T, A> {
    // Specialization would be nice here
    fn clone(&self) -> Self {
        let nalloc = self.alloc.clone();
        let mut nvec = Self::with_capacity_in(self.len, nalloc);
        nvec.extend_from_slice(self);
        nvec
    }

    fn clone_from(&mut self, source: &Self) {
        self.clear();
        self.reserve(source.len);
        self.extend_from_slice(source);
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

/// An iterator that takes ownership of elements at the end of a [`Vec`]
pub struct DrainBack<'a, T> {
    ptr: core::ptr::NonNull<T>,
    remaining: usize,
    _phantom: PhantomData<&'a mut [T]>,
}

impl<'a, T> Iterator for DrainBack<'a, T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            let val = unsafe { core::ptr::read(self.ptr.as_ptr()) };
            self.remaining -= 1;
            self.ptr = unsafe { NonNull::new_unchecked(self.ptr.as_ptr().add(1)) };

            Some(val)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl<'a, T> Drop for DrainBack<'a, T> {
    fn drop(&mut self) {
        if core::mem::needs_drop::<T>() {
            for i in 0..self.remaining {
                unsafe { core::ptr::drop_in_place(self.ptr.as_ptr().add(i)) }
            }
        }
    }
}

impl<'a, T> ExactSizeIterator for DrainBack<'a, T> {}
impl<'a, T> FusedIterator for DrainBack<'a, T> {}
impl<'a, T> DoubleEndedIterator for DrainBack<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.remaining -= 1;
            let val = unsafe { core::ptr::read(self.ptr.as_ptr().add(self.remaining)) };
            Some(val)
        }
    }
}

/// An iterator over the values of a [`Vec`]
pub struct IntoIter<T, A: Allocator> {
    ptr: Unique<T>,
    len: usize,
    cap: usize,
    consumed: usize,
    alloc: A,
}

impl<T, A: Allocator> Iterator for IntoIter<T, A> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if self.consumed == self.len {
            None
        } else {
            // SAFETY:
            // self.ptr, self.len, and self.cap all come from `Vec`, so we know that `ptr` is at least self.cap `T`s long
            // and is valid up to `self.len`, which self.consumed is at most here
            let ptr = unsafe { self.ptr.as_ptr().add(self.consumed) };
            self.consumed += 1;
            // SAFETY:
            // Same as above reasoning. Additionally, no !Copy value is duplicated, because we've advanced past the previously `ptr::read` value above
            Some(unsafe { core::ptr::read(ptr) })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len - self.consumed, Some(self.len - self.consumed))
    }
}

impl<T, A: Allocator> ExactSizeIterator for IntoIter<T, A> {}

impl<T, A: Allocator> FusedIterator for IntoIter<T, A> {}

impl<T, A: Allocator> DoubleEndedIterator for IntoIter<T, A> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.consumed == self.len {
            None
        } else {
            self.len -= 1;
            // SAFETY:
            // self.ptr, self.len, and self.cap all come from `Vec`, so we know that `ptr` is at least self.cap `T`s long
            // and is valid up to `self.len`.
            let ptr = unsafe { self.ptr.as_ptr().add(self.len) };

            // SAFETY:
            // Same as above reasoning. Additionally, no !Copy value is duplicated, because we've advanced past the previously `ptr::read` value above
            Some(unsafe { core::ptr::read(ptr) })
        }
    }
}

impl<T, A: Allocator> Drop for IntoIter<T, A> {
    fn drop(&mut self) {
        if core::mem::needs_drop::<T>() {
            for i in self.consumed..self.len {
                // SAFETY:
                // self.ptr, self.len, and self.cap all come from `Vec`, so we know that `ptr` is exactly self.cap `T`s long
                // and is valid up to `self.len`, which i is at most here
                // Additionally, no !Copy value is duplicated, because we've advanced past any previously consumed values of `T`
                unsafe { core::ptr::drop_in_place(self.ptr.as_ptr().add(i)) }
            }
        }
        let raw_cap = self.cap * size_of::<T>();
        if raw_cap != 0 {
            let layout =
                unsafe { Layout::from_size_align_unchecked(raw_cap, core::mem::align_of::<T>()) };

            // SAFETY:
            // self.ptr and self.cap both come from `Vec`, so `ptr` is exactly self.capacity `T`s long,
            // and that it was allocated using an allocator compatible with `self.alloc`
            unsafe { self.alloc.deallocate(self.ptr.as_nonnull().cast(), layout) };
        }
    }
}

impl<T, A: Allocator> IntoIterator for Vec<T, A> {
    type Item = T;
    type IntoIter = IntoIter<T, A>;

    fn into_iter(self) -> Self::IntoIter {
        let (ptr, cap, len, alloc) = self.into_raw_parts_with_alloc();

        IntoIter {
            ptr: unsafe { Unique::new_unchecked(ptr) },
            cap,
            len,
            alloc,
            consumed: 0,
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
            let __repeat: ::core::primitive::usize = $repeat;
            fn __check<T: ::core::clone::Clone>(_: &T) {}
            let val = $elem;
            __check(&val);
            let mut vec = $crate::vec::Vec::with_capacity(__repeat);
            for _ in 0..($repeat){
                vec.push(::core::clone::Clone::clone(&val));
            }
            vec
        }

    }
}

#[cfg(test)]
mod test {
    use super::Vec;

    #[test]
    fn test_vec_new() {
        let vec = Vec::<u32>::new();
        assert_eq!(vec.len(), 0);
    }

    #[test]
    fn test_vec_empty() {
        let vec: Vec<u32> = vec![];
        assert_eq!(vec.len(), 0);
    }

    #[test]
    fn test_vec_new_reserve() {
        let mut vec: Vec<u32> = Vec::new();
        vec.reserve(128);
        vec.push(1); // Just to make sure pushing doesn't crash / cause UB
        assert_eq!(vec.capacity(), 128);
        assert_eq!(vec.len(), 1);
        assert_eq!(vec[0], 1);
    }

    #[test]
    fn test_vec_with_data() {
        let vec: Vec<u32> = vec![314, 159, 265];
        assert_eq!(vec.len(), 3);
        assert_eq!(vec[0], 314);
        assert_eq!(vec[1], 159);
        assert_eq!(vec[2], 265);
    }

    #[test]
    fn test_vec_with_len() {
        let vec: Vec<u32> = vec![42; 7];
        assert_eq!(vec.len(), 7);
        assert_eq!(vec[0], 42);
        assert_eq!(vec[1], 42);
        assert_eq!(vec[5], 42);
    }

    #[test]
    fn test_vec_push() {
        let mut vec = Vec::new();
        vec.push(0);
        assert_eq!(vec.len(), 1);
    }

    #[test]
    fn test_vec_deref() {
        let mut vec = Vec::new();
        vec.push(0);
        assert!(matches!(*vec, [0]));
    }

    #[test]
    fn test_vec_iter() {
        let mut vec = Vec::new();
        vec.push(1);

        let mut iter = vec.iter();
        assert_eq!(iter.next(), Some(&1));
    }

    #[test]
    fn test_vec_subscript() {
        let mut vec = Vec::new();
        vec.push(1);

        assert_eq!(vec[0], 1);
    }

    #[test]
    #[should_panic]
    fn test_vec_subscript_oob() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);

        let _ = vec[2];
    }

    #[test]
    fn test_vec_get() {
        let mut vec = Vec::new();
        vec.push(1);

        assert_eq!(vec.get(0), Some(&1));
    }

    #[test]
    fn test_vec_get_oob() {
        let mut vec = Vec::new();
        vec.push(1);

        assert_eq!(vec.get(1), None);
    }

    #[test]
    fn test_vec_get_unchecked() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);

        assert_eq!(unsafe { vec.get_unchecked(1) }, &2);
    }

    #[test]
    fn test_vec_slice() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);

        assert!(matches!(vec[..2], [1, 2]));
    }

    #[test]
    fn test_vec_slice_inclusive_bound() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);

        assert!(matches!(vec[..=2], [1, 2, 3]));
    }

    #[test]
    fn test_vec_slice_full() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);

        assert!(matches!(vec[..], [1, 2, 3]));
    }

    #[test]
    fn test_vec_iter_mut() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);

        let mut iter_mut = vec.iter_mut();
        *iter_mut.next().unwrap() += 3;
        *iter_mut.next().unwrap() += 3;

        assert_eq!(vec[0], 4);
        assert_eq!(vec[1], 5);
    }

    #[test]
    fn test_vec_macro_slice() {
        let vec = vec![0, 1, 2, 3];

        assert_eq!(vec[..], [0, 1, 2, 3]);
    }

    #[test]
    fn test_vec_macro_repeat() {
        let vec = vec![0; 4];

        assert_eq!(vec[..], [0; 4])
    }

    #[test]
    fn test_vec_extend_from_slice_non_copy() {
        #[derive(Clone, Debug, PartialEq)]
        pub struct NotCopy(u8);

        const NOT_COPY: NotCopy = NotCopy(1);

        let mut base = Vec::new();
        base.extend_from_slice(&[NOT_COPY; 16]);

        assert_eq!(base[..], [NOT_COPY; 16])
    }

    #[test]
    fn test_vec_extend_from_slice_copy() {
        let mut base = Vec::new();
        base.extend_from_slice(&[0u8; 16]);

        assert_eq!(base[..], [0; 16])
    }

    #[test]
    fn test_vec_extend_from_within_non_copy() {
        #[derive(Clone, Debug, PartialEq)]
        pub struct NotCopy(u8);
        const NOT_COPY: NotCopy = NotCopy(1);
        let mut base = vec![NotCopy(1); 32];
        base.extend_from_within(0..8);
        assert_eq!(base[..], [NOT_COPY; 40]);
    }

    #[test]
    fn test_vec_extend_from_within_copy() {
        let mut base = vec![1u8; 32];
        base.extend_from_within(0..8);
        assert_eq!(base[..], [1; 40]);
    }
}
