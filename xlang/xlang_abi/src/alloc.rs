#![cfg_attr(any(miri, test), allow(unused_unsafe, missing_docs))]

pub use std::alloc::GlobalAlloc;
use std::{
    convert::TryFrom,
    hash::{Hash, Hasher},
    ptr::NonNull,
};

use crate::{
    prelude::v1::{DynBox, DynMut, DynRef},
    traits::{AbiSafeTrait, AbiSafeUnsize, AbiSafeVTable, DynPtrSafe},
};

#[cfg(not(any(miri, test)))]
extern "C" {
    /// Function that allocates memory suitable for storing an object of size `size`, with at least the maximum fundamental alignment for that size.
    ///
    /// The function permits allocations of size `0`, returning a pointer that is aligned to at least the maximum fundamental alignment for the platform.
    ///
    /// # Errors
    /// On allocation failure, or other (unspecified) errors, returns a null pointer. Allocations of size `0` always succeed
    ///
    /// # Safety
    /// size, rounded to the next multiple of the alignment used by this function, must not be greater than `isize::MAX`
    pub fn xlang_allocate(size: usize) -> *mut core::ffi::c_void;
    /// Function that allocates memory suitable for storing an object of size `size`, with at least the alignment given by `align`.
    ///
    /// This function permits allocations of size `0`, returning a pointer that is aligned to at least the given alignment
    ///
    /// # Errors
    /// On allocation failure, or other (unspecified) errors, returns a null pointer. Allocations of size `0` always succeed
    ///
    /// # Safety
    /// `align` must be a power of two. size, rounded to the next multiple of `align`, must not be greater than `isize::MAX`
    pub fn xlang_allocate_aligned(size: usize, align: usize) -> *mut core::ffi::c_void;
    /// Deallocates memory allocated using [`xlang_allocate`]
    ///
    /// # Safety
    /// ptr must be a null pointer, or have been allocated using [`xlang_allocate`] with `size`
    pub fn xlang_deallocate(ptr: *mut core::ffi::c_void, size: usize);
    /// Deallocates memory allocated using [`xlang_allocate`]
    ///
    /// # Safety
    /// ptr must be a null pointer, or have been allocated using [`xlang_allocate_aligned`] with `size` and `align`
    pub fn xlang_deallocate_aligned(ptr: *mut core::ffi::c_void, size: usize, align: usize);

    /// Function to call when allocation fails.
    pub fn xlang_on_allocation_failure(size: usize, align: usize) -> !;
}

#[cfg(any(miri, test))]
#[no_mangle]
pub unsafe extern "C" fn xlang_allocate(size: usize) -> *mut core::ffi::c_void {
    if size == 0 {
        return 32usize as *mut core::ffi::c_void;
    }
    xlang_allocate_aligned(
        size,
        if size > 32 {
            32
        } else {
            size.next_power_of_two()
        },
    )
}

#[cfg(any(miri, test))]
#[no_mangle]
pub unsafe extern "C" fn xlang_allocate_aligned(
    size: usize,
    align: usize,
) -> *mut core::ffi::c_void {
    if size == 0 {
        return align as *mut core::ffi::c_void;
    }
    let size = size + (align - size % align) % align;
    let layout = std::alloc::Layout::from_size_align_unchecked(size as usize, align as usize);
    std::alloc::alloc(layout).cast::<_>()
}

#[cfg(any(miri, test))]
#[no_mangle]
pub unsafe extern "C" fn xlang_deallocate(ptr: *mut core::ffi::c_void, size: usize) {
    if size == 0 {
        return;
    }
    #[allow(unused_unsafe)]
    unsafe {
        xlang_deallocate_aligned(
            ptr,
            size,
            if size > 32 {
                32
            } else {
                size.next_power_of_two()
            },
        );
    }
}

#[cfg(any(miri, test))]
#[no_mangle]
pub unsafe extern "C" fn xlang_deallocate_aligned(
    ptr: *mut core::ffi::c_void,
    size: usize,
    align: usize,
) {
    if size == 0 || ptr.is_null() {
        return;
    }
    let size = size + (align - size % align) % align;
    let layout = std::alloc::Layout::from_size_align_unchecked(size as usize, align as usize);
    std::alloc::dealloc(ptr.cast::<_>(), layout);
}

#[cfg(any(miri, test))]
#[no_mangle]
pub extern "C" fn xlang_on_allocation_failure(size: usize, align: usize) -> ! {
    eprintln!(
        "Failed to allocate with size: {}, and alignment: {}",
        size, align
    );
    std::process::abort()
}

/// An abi safe version of the [`std::alloc::Layout`] type
#[repr(C)]
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct Layout {
    size: usize,
    align: usize,
}

/// Error returned when an invalid Layout is produced
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct LayoutError(());

impl core::fmt::Display for LayoutError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Invalid Layout")
    }
}

impl std::error::Error for LayoutError {}

#[allow(
    clippy::checked_conversions,
    clippy::missing_const_for_fn,
    clippy::missing_panics_doc
)] // All of the panics are impossible, or result from a precondition violation (and thus cannot happen)
impl Layout {
    /// Produces the layout for T
    #[must_use]
    pub const fn new<T>() -> Self {
        Self {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
        }
    }

    /// Produces a layout with the given size and alignment.
    ///
    /// # Errors
    /// A [`LayoutError`] is returned if any of the following conditions do not hold:
    /// * `align` is a power of two
    /// * `size`, rounded up to the next multiple of align, does not exceed [`isize::MAX`]
    pub const fn from_size_align(size: usize, align: usize) -> Result<Self, LayoutError> {
        match (
            (size).overflowing_add((align - (size % align)) % align),
            align,
        ) {
            ((x, true), align) if x < (isize::MAX as usize) && align.is_power_of_two() => {
                Ok(Self { size, align })
            }
            _ => Err(LayoutError(())),
        }
    }

    ///
    /// Produces a layout with the given size and alignment, without checking any preconditions
    ///
    /// # Safety
    /// The following conditions must hold:
    /// * `align` is a power of two
    /// * `size`, rounded up to the next multiple of align, does not exceed [`isize::MAX`]
    #[must_use]
    pub const unsafe fn from_size_align_unchecked(size: usize, align: usize) -> Self {
        Self { size, align }
    }

    /// Computes the layout of a given dynamically sized value
    #[must_use]
    pub fn from_val<T: ?Sized>(x: &T) -> Self {
        Self {
            size: core::mem::size_of_val(x),
            align: core::mem::align_of_val(x),
        }
    }

    /// Computes the layout of a given [`AbiSafeTrait`] object
    #[must_use]
    pub fn from_dyn<T: ?Sized + AbiSafeTrait>(a: &dyn DynPtrSafe<T>) -> Self {
        Self {
            size: a.size_of_val(),
            align: a.align_of_val(),
        }
    }

    /// Computes the layout of an array of x values of type `T`.
    ///
    /// ## Errors
    /// Returns a [`LayoutError`] if x, times the size of T, exceeds [`isize::MAX`]
    #[allow(clippy::cast_sign_loss)]
    pub fn array<T>(x: usize) -> Result<Self, LayoutError> {
        match (
            (isize::try_from(core::mem::size_of::<T>()).unwrap())
                .checked_mul(isize::try_from(x).map_err(|_| LayoutError(()))?),
            core::mem::align_of::<T>(),
        ) {
            (Some(size), align) => Ok(Self {
                // Can't be negative becacuse of `checked_mul`
                size: size as usize,
                align,
            }),
            (None, _) => Err(LayoutError(())),
        }
    }

    /// Returns the size of this layout
    #[must_use]
    pub const fn size(&self) -> usize {
        self.size
    }

    /// Returns the alignment of this layout
    #[must_use]
    pub const fn align(&self) -> usize {
        self.align
    }

    /// Aligns up to max fundamental alignment for the current size
    pub fn align_to_fundamental(&self) -> Result<Self, LayoutError> {
        if let Some(align) = self
            .size
            .checked_next_power_of_two()
            .filter(|v| *v < core::mem::align_of::<xlang_host::primitives::max_align_t>())
        {
            Ok(Self {
                size: self.size,
                align,
            })
        } else {
            self.align_to(core::mem::align_of::<xlang_host::primitives::max_align_t>())
        }
    }

    /// Rounds the size of Self up to the next multiple of align
    #[must_use]
    pub const fn pad_to_align(&self) -> Self {
        Self {
            size: self.size + ((self.align - (self.size % self.align)) % self.align),
            align: self.align,
        }
    }

    /// Returns the layout with the same size as this layout, but the greatest alignment between this layout and `align`
    ///
    /// ## Errors
    /// Returns an error if any of the following do not hold:
    /// * align is a power of two
    /// * self.size(), rounded up to the next multiple of align, overflows isize
    pub fn align_to(&self, align: usize) -> Result<Self, LayoutError> {
        match (
            (isize::try_from(self.size).unwrap())
                .overflowing_add(isize::try_from((align - (self.size % align)) % align).unwrap()),
            align.max(self.align()),
        ) {
            ((_, false), align) if align.is_power_of_two() => Ok(Self {
                size: self.size,
                align,
            }),
            _ => Err(LayoutError(())),
        }
    }

    ///
    /// Returns the size (in bytes) of the trailing padding necessary to align this layout to `align`
    #[must_use]
    pub const fn padding_needed_for(&self, align: usize) -> usize {
        (align - (self.size % align)) % align
    }

    /// Computes the layout of n contiguous records with this layout, and returns that layout and the offset from the end of this layout to the beginning of the next layout
    ///
    /// ## Errors
    /// Returns [`LayoutError`] if `self.size()`, padded to `self.align()`, and multiplied by `n`, overflows isize
    pub fn repeat(&self, n: usize) -> Result<(Self, usize), LayoutError> {
        let Self { size, align } = self.pad_to_align();
        match (size.checked_mul(n), align) {
            (Some(asize), align) if asize <= (isize::MAX as usize) => Ok((
                Self {
                    size: asize as usize,
                    align,
                },
                size - self.size,
            )),
            _ => Err(LayoutError(())),
        }
    }

    /// Computes the result of extending this layout by adding a field with the Layout of `next`, and returns that layout and the offset from the end of this layout to the beginning of `next`
    /// The `next` Layout will be well-aligned within the resulting Layout
    /// ## Errors
    /// Returns [`LayoutError`] if the resultant size, rounded up to the next multiple of the resultant alignment, overflows isize
    pub fn extend(&self, next: Self) -> Result<(Self, usize), LayoutError> {
        let Self { size, align } = self.align_to(next.align())?;
        match size
            .checked_add((next.align - (size % next.align)) % next.align)
            .and_then(|v| v.checked_add(next.size()))
        {
            Some(rsize) if rsize <= (isize::MAX as usize) => Ok((
                Self {
                    size: rsize as usize,
                    align,
                },
                size - self.size(),
            )),
            _ => Err(LayoutError(())),
        }
    }

    /// Computes the result of repeating n copies of the current Layout, without aligning adjacent records
    ///
    /// ## Errors
    /// Returns [`LayoutError`] if the resultant size overflows isize
    pub const fn repeat_packed(&self, n: usize) -> Result<Self, LayoutError> {
        match self.size().checked_mul(n) {
            Some(size) if size <= (isize::MAX as usize) => Ok(Self {
                size,
                align: self.align(),
            }),
            _ => Err(LayoutError(())),
        }
    }

    /// Computes the result of extending n this Layout with next, without aligning adjacent records
    ///
    /// ## Errors
    /// Returns [`LayoutError`] if the resultant size overflows isize
    pub const fn extend_packed(&self, next: Self) -> Result<Self, LayoutError> {
        match self.size.checked_add(next.size()) {
            Some(size) if size <= (isize::MAX as usize) => Ok(Self {
                size,
                align: self.align(),
            }),
            _ => Err(LayoutError(())),
        }
    }

    /// Returns a non-null pointer that is well-aligned for this layout, and is valid for accesses of size `0`.
    /// The pointer returned by this function is unspecified.
    #[must_use]
    pub const fn dangling(&self) -> NonNull<u8> {
        // NOTE: this is an implementation detail
        // Future versions may offer randomization or
        unsafe { NonNull::new_unchecked(self.align as *mut u8) }
    }
}

///
/// The Allocator trait provides an interface for collections and smart pointers to obtain memory
///
/// ## Safety
/// Pointers returned from [`Allocator::allocate`] must belong to logically different allocations.
/// That is, deallocating one pointer returned from [`Allocator::allocate`] must not invalidate other pointers returned from other calls.
///
/// Further, all pointers returned from [`Allocator::allocate`], [`Allocator::allocate_zeroed`], [`Allocator::grow`], [`Allocator::grow_zeroed`], and [`Allocator::shrink`] must
///  be aligned to at least the alignment given by layout, and be valid for both reads and writes of at least the size given by the layout.
///
/// The pointers returned from [`Allocator::allocate_zeroed`] and [`Allocator::grow_zeroed`] must contain `size` `0` bytes.
///
/// Allocators must be trivial or cheap to [`Copy`] or [`Clone`]. Further, Allocators obtained by [`Clone::clone`]ing a particular allocator must be mutually able to deallocate memory obtained
///  from other clones of the same allocator
pub unsafe trait Allocator {
    /// Allocates memory suitable for `layout`, and returns a pointer to it, or `None` if an error occurs.
    /// The value of the memory accessed via the pointer is uninitialized
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>>;
    /// Deallocates Memory obtained from a call to [`Allocator::allocate`] with the given layout
    ///
    /// ## Safety
    /// ptr must have been allocated by this allocator, a [`Clone::clone`] of this allocator, or a reference to this allocator, with the given layout
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout);

    /// Allocates memory suitable for `layout`, and returns a pointer to it, or `None` if an error occurs.
    /// Each byte pointed to by the return value up to `layout.size()` is `0`
    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        let ptr = self.allocate(layout)?;
        unsafe {
            core::ptr::write_bytes(ptr.as_ptr().cast::<u8>(), 0, layout.size());
        }
        Some(ptr)
    }

    /// Grows an allocation given by `ptr` and `old_layout`, copying the data from the old allocation into the new allocation.
    /// `ptr` may not be used to access memory after a call to this function. It is unsound to call `grow` on [`core::pin::Pin`]ned memory
    /// ## Safety
    /// ptr must have been allocated by this allocator, a [`Clone::clone`] of this allocator, or a reference to this allocator, with `old_layout`
    ///
    /// The size of `new_layout` must be at least the size of `old_layout`
    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        let nptr = self.allocate(new_layout)?;
        #[allow(unused_unsafe)]
        unsafe {
            core::ptr::copy_nonoverlapping(
                ptr.as_ptr(),
                nptr.as_ptr().cast::<u8>(),
                old_layout.size(),
            );
        }
        self.deallocate(ptr, old_layout);
        Some(nptr)
    }

    /// Grows an allocation given by `ptr` and `old_layout`, copying the data from the old allocation into the new allocation.
    /// `ptr` may not be used to access memory after a call to this function. It is unsound to call `grow_zeroed` on [`core::pin::Pin`]ned memory
    ///
    /// Any bytes between `old_layout.size()` and `new_layout.size()` are set to `0`
    /// ## Safety
    /// ptr must have been allocated by this allocator, a [`Clone::clone`] of this allocator, or a reference to this allocator, with `old_layout`
    ///
    /// The size of `new_layout` must be at least the size of `old_layout`
    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        let nptr = self.allocate_zeroed(new_layout)?;
        #[allow(unused_unsafe)]
        unsafe {
            core::ptr::copy_nonoverlapping(
                ptr.as_ptr(),
                nptr.as_ptr().cast::<u8>(),
                old_layout.size(),
            );
        }
        self.deallocate(ptr, old_layout);
        Some(nptr)
    }

    /// Grows an allocation given by `ptr` and `old_layout`, copying the data from the old allocation into the new allocation.
    /// `ptr` may not be used to access memory after a call to this function. It is unsound to call `shrink` on [`core::pin::Pin`]ned memory
    /// ## Safety
    /// ptr must have been allocated by this allocator, a [`Clone::clone`] of this allocator, or a reference to this allocator, with `old_layout`
    ///
    /// The size of `new_layout` must be at most the size of `old_layout`
    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        let nptr = self.allocate(new_layout)?;
        #[allow(unused_unsafe)]
        unsafe {
            core::ptr::copy_nonoverlapping(
                ptr.as_ptr(),
                nptr.as_ptr().cast::<u8>(),
                new_layout.size(),
            );
        }
        self.deallocate(ptr, old_layout);
        Some(nptr)
    }

    /// Returns exactly self
    fn as_ref(&self) -> &Self
    where
        Self: Sized,
    {
        self
    }
}

unsafe impl<A: ?Sized + Allocator> Allocator for &A {
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        A::allocate(self, layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        A::deallocate(self, ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        A::allocate_zeroed(self, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        A::grow(self, ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        A::grow_zeroed(self, ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        A::shrink(self, ptr, old_layout, new_layout)
    }
}

unsafe impl<A: ?Sized + Allocator> Allocator for &mut A {
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        A::allocate(self, layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        A::deallocate(self, ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        A::allocate_zeroed(self, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        A::grow(self, ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        A::grow_zeroed(self, ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        A::shrink(self, ptr, old_layout, new_layout)
    }
}

/// An Allocator which allocates memory using `xlang_allocate` and/or `xlang_allocate_array`
#[allow(clippy::module_name_repetitions, dead_code)]
// TODO: Should this be changed? No. XLangAlloc is the correct name. This isn't the `Global` or `System` allocator, it's the xlang allocator
#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct XLangAlloc(());

impl XLangAlloc {
    ///
    /// Produces a new [`XLangAlloc`] value.
    ///
    /// All values of type [`XLangAlloc`] are identical, and may be used interchangeably
    #[must_use]
    pub const fn new() -> Self {
        Self(())
    }
}

impl core::fmt::Debug for XLangAlloc {
    fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
        fmt.write_str("XLangAlloc")
    }
}

impl Hash for XLangAlloc {
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

impl PartialEq for XLangAlloc {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Eq for XLangAlloc {}

impl Default for XLangAlloc {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl Allocator for XLangAlloc {
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        NonNull::new(unsafe {
            xlang_allocate_aligned(layout.size() as usize, layout.align() as usize).cast::<u8>()
        })
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        xlang_deallocate_aligned(
            ptr.as_ptr().cast(),
            layout.size() as usize,
            layout.align() as usize,
        );
    }
}

///
/// `VTable` for the [`Allocator`] trait
#[repr(C)]
pub struct AllocatorVTable {
    size: usize,
    align: usize,
    destructor: Option<unsafe extern "C" fn(*mut ())>,
    reserved_dealloc: Option<unsafe extern "C" fn(*mut ())>,
    allocate: unsafe extern "C" fn(*const (), Layout) -> Option<NonNull<u8>>,
    deallocate: unsafe extern "C" fn(*const (), NonNull<u8>, Layout),
    allocate_zeroed: unsafe extern "C" fn(*const (), Layout) -> Option<NonNull<u8>>,
    grow: unsafe extern "C" fn(*const (), NonNull<u8>, Layout, Layout) -> Option<NonNull<u8>>,
    grow_zeroed:
        unsafe extern "C" fn(*const (), NonNull<u8>, Layout, Layout) -> Option<NonNull<u8>>,
    shrink: unsafe extern "C" fn(*const (), NonNull<u8>, Layout, Layout) -> Option<NonNull<u8>>,
}

unsafe impl AbiSafeVTable<dyn Allocator> for AllocatorVTable {}

unsafe impl AbiSafeVTable<dyn Allocator + Send> for AllocatorVTable {}
unsafe impl AbiSafeVTable<dyn Allocator + Sync> for AllocatorVTable {}
unsafe impl AbiSafeVTable<dyn Allocator + Send + Sync> for AllocatorVTable {}

unsafe impl AbiSafeTrait for dyn Allocator {
    type VTable = AllocatorVTable;
}
unsafe impl AbiSafeTrait for dyn Allocator + Send {
    type VTable = AllocatorVTable;
}
unsafe impl AbiSafeTrait for dyn Allocator + Sync {
    type VTable = AllocatorVTable;
}
unsafe impl AbiSafeTrait for dyn Allocator + Send + Sync {
    type VTable = AllocatorVTable;
}

unsafe extern "C" fn vtbl_destroy<T>(this: *mut ()) {
    core::ptr::drop_in_place(this.cast::<T>());
}

unsafe extern "C" fn vtbl_allocate<T: Allocator>(
    this: *const (),
    layout: Layout,
) -> Option<NonNull<u8>> {
    <T as Allocator>::allocate(&*(this.cast::<T>()), layout)
}

unsafe extern "C" fn vtbl_allocate_zeroed<T: Allocator>(
    this: *const (),
    layout: Layout,
) -> Option<NonNull<u8>> {
    <T as Allocator>::allocate_zeroed(&*(this.cast::<T>()), layout)
}

unsafe extern "C" fn vtbl_deallocate<T: Allocator>(
    this: *const (),
    ptr: NonNull<u8>,
    layout: Layout,
) {
    <T as Allocator>::deallocate(&*(this.cast::<T>()), ptr, layout);
}

unsafe extern "C" fn vtbl_grow<T: Allocator>(
    this: *const (),
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
) -> Option<NonNull<u8>> {
    <T as Allocator>::grow(&*(this.cast::<T>()), ptr, old_layout, new_layout)
}

unsafe extern "C" fn vtbl_grow_zeroed<T: Allocator>(
    this: *const (),
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
) -> Option<NonNull<u8>> {
    <T as Allocator>::grow_zeroed(&*(this.cast::<T>()), ptr, old_layout, new_layout)
}

unsafe extern "C" fn vtbl_shrink<T: Allocator>(
    this: *const (),
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
) -> Option<NonNull<u8>> {
    <T as Allocator>::shrink(&*(this.cast::<T>()), ptr, old_layout, new_layout)
}

unsafe impl<T: Allocator> AbiSafeUnsize<T> for dyn Allocator {
    fn construct_vtable_for() -> &'static Self::VTable {
        &AllocatorVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            allocate: vtbl_allocate::<T>,
            deallocate: vtbl_deallocate::<T>,
            allocate_zeroed: vtbl_allocate_zeroed::<T>,
            grow: vtbl_grow::<T>,
            grow_zeroed: vtbl_grow_zeroed::<T>,
            shrink: vtbl_shrink::<T>,
        }
    }
}

unsafe impl<T: Allocator + Send> AbiSafeUnsize<T> for dyn Allocator + Send {
    fn construct_vtable_for() -> &'static Self::VTable {
        &AllocatorVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            allocate: vtbl_allocate::<T>,
            deallocate: vtbl_deallocate::<T>,
            allocate_zeroed: vtbl_allocate_zeroed::<T>,
            grow: vtbl_grow::<T>,
            grow_zeroed: vtbl_grow_zeroed::<T>,
            shrink: vtbl_shrink::<T>,
        }
    }
}

unsafe impl<T: Allocator + Sync> AbiSafeUnsize<T> for dyn Allocator + Sync {
    fn construct_vtable_for() -> &'static Self::VTable {
        &AllocatorVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            allocate: vtbl_allocate::<T>,
            deallocate: vtbl_deallocate::<T>,
            allocate_zeroed: vtbl_allocate_zeroed::<T>,
            grow: vtbl_grow::<T>,
            grow_zeroed: vtbl_grow_zeroed::<T>,
            shrink: vtbl_shrink::<T>,
        }
    }
}

unsafe impl<T: Allocator + Send + Sync> AbiSafeUnsize<T> for dyn Allocator + Send + Sync {
    fn construct_vtable_for() -> &'static Self::VTable {
        &AllocatorVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            allocate: vtbl_allocate::<T>,
            deallocate: vtbl_deallocate::<T>,
            allocate_zeroed: vtbl_allocate_zeroed::<T>,
            grow: vtbl_grow::<T>,
            grow_zeroed: vtbl_grow_zeroed::<T>,
            shrink: vtbl_shrink::<T>,
        }
    }
}

unsafe impl<'lt> Allocator for dyn DynPtrSafe<dyn Allocator> + 'lt {
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        unsafe { (self.vtable().allocate)(self.as_raw(), layout) }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        (self.vtable().deallocate)(self.as_raw(), ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        unsafe { (self.vtable().allocate_zeroed)(self.as_raw(), layout) }
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().grow)(self.as_raw(), ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().grow_zeroed)(self.as_raw(), ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().shrink)(self.as_raw(), ptr, old_layout, new_layout)
    }
}

unsafe impl<'lt> Allocator for dyn DynPtrSafe<dyn Allocator + Send> + 'lt {
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        unsafe { (self.vtable().allocate)(self.as_raw(), layout) }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        (self.vtable().deallocate)(self.as_raw(), ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        unsafe { (self.vtable().allocate_zeroed)(self.as_raw(), layout) }
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().grow)(self.as_raw(), ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().grow_zeroed)(self.as_raw(), ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().shrink)(self.as_raw(), ptr, old_layout, new_layout)
    }
}

unsafe impl<'lt> Allocator for dyn DynPtrSafe<dyn Allocator + Sync> + 'lt {
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        unsafe { (self.vtable().allocate)(self.as_raw(), layout) }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        (self.vtable().deallocate)(self.as_raw(), ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        unsafe { (self.vtable().allocate_zeroed)(self.as_raw(), layout) }
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().grow)(self.as_raw(), ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().grow_zeroed)(self.as_raw(), ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().shrink)(self.as_raw(), ptr, old_layout, new_layout)
    }
}

unsafe impl<'lt> Allocator for dyn DynPtrSafe<dyn Allocator + Send + Sync> + 'lt {
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        unsafe { (self.vtable().allocate)(self.as_raw(), layout) }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        (self.vtable().deallocate)(self.as_raw(), ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        unsafe { (self.vtable().allocate_zeroed)(self.as_raw(), layout) }
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().grow)(self.as_raw(), ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().grow_zeroed)(self.as_raw(), ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        (self.vtable().shrink)(self.as_raw(), ptr, old_layout, new_layout)
    }
}

unsafe impl<'lt, T: ?Sized + AbiSafeTrait> Allocator for DynRef<'lt, T>
where
    dyn DynPtrSafe<T> + 'lt: Allocator,
{
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::allocate(&**self, layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        <dyn DynPtrSafe<T> as Allocator>::deallocate(&**self, ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::allocate_zeroed(&**self, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::grow(&**self, ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::grow_zeroed(&**self, ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::shrink(&**self, ptr, old_layout, new_layout)
    }
}

unsafe impl<'lt, T: ?Sized + AbiSafeTrait> Allocator for DynMut<'lt, T>
where
    dyn DynPtrSafe<T> + 'lt: Allocator,
{
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::allocate(&**self, layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        <dyn DynPtrSafe<T> as Allocator>::deallocate(&**self, ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::allocate_zeroed(&**self, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::grow(&**self, ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::grow_zeroed(&**self, ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::shrink(&**self, ptr, old_layout, new_layout)
    }
}

unsafe impl<T: ?Sized + AbiSafeTrait + 'static, A: Allocator> Allocator for DynBox<T, A>
where
    dyn DynPtrSafe<T>: Allocator,
{
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::allocate(&**self, layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        <dyn DynPtrSafe<T> as Allocator>::deallocate(&**self, ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::allocate_zeroed(&**self, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::grow(&**self, ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::grow_zeroed(&**self, ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::shrink(&**self, ptr, old_layout, new_layout)
    }
}

/// Called when allocation returns an unhandled error
pub fn handle_alloc_error(layout: Layout) -> ! {
    #![cfg_attr(test, allow(unused_unsafe))]
    unsafe { xlang_on_allocation_failure(layout.size() as usize, layout.align() as usize) }
}

#[cfg(all(test, miri))]
mod test {
    use super::{
        xlang_allocate, xlang_allocate_aligned, xlang_deallocate, xlang_deallocate_aligned,
    };
    #[test]
    pub fn test_alloc() {
        let p = unsafe { xlang_allocate(4) } as *mut i32;
        if p.is_null() {
            return;
        }
        unsafe {
            p.write(4i32);
        }
        unsafe { xlang_deallocate(p as *mut _, 4) }
    }

    #[test]
    pub fn test_alloc_align() {
        #[repr(align(256))]
        #[allow(dead_code)]
        struct Foo(u8);
        let p = unsafe { xlang_allocate_aligned(256, 256) } as *mut Foo;
        if p.is_null() {
            return;
        }
        unsafe {
            p.write(Foo(0));
        }
        unsafe { xlang_deallocate_aligned(p as *mut _, 256, 256) }
    }
}
