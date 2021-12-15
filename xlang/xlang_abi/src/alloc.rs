pub use std::alloc::GlobalAlloc;
use std::{
    hash::{Hash, Hasher},
    mem::MaybeUninit,
    ptr::NonNull,
};

use crate::{
    prelude::v1::{DynBox, DynMut, DynRef},
    traits::{AbiSafeTrait, AbiSafeUnsize, AbiSafeVTable, DynPtrSafe},
};

extern "C" {
    pub fn xlang_allocate(size: usize) -> *mut core::ffi::c_void;
    pub fn xlang_allocate_aligned(size: usize, align: usize) -> *mut core::ffi::c_void;
    pub fn xlang_deallocate(ptr: *mut core::ffi::c_void, size: usize);
    pub fn xlang_deallocate_aligned(ptr: *mut core::ffi::c_void, size: usize, align: usize);
    pub fn xlang_on_allocation_failure(size: usize, align: usize) -> !;
}

#[repr(C)]
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct Layout {
    size: usize,
    align: usize,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct LayoutError(());

impl core::fmt::Display for LayoutError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Invalid Layout")
    }
}

impl std::error::Error for LayoutError {}

impl Layout {
    pub const fn new<T>() -> Layout {
        Layout {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
        }
    }

    pub fn from_size_align(size: usize, align: usize) -> Result<Layout, LayoutError> {
        match (size.overflowing_add(size % align), align) {
            ((_, true), align) if align.is_power_of_two() => Ok(Layout { size, align }),
            _ => Err(LayoutError(())),
        }
    }

    pub const unsafe fn from_size_align_unchecked(size: usize, align: usize) -> Layout {
        Layout { size, align }
    }

    pub fn from_val<T: ?Sized>(x: &T) -> Layout {
        Layout {
            size: core::mem::size_of_val(x),
            align: core::mem::align_of_val(x),
        }
    }

    pub fn from_dyn<T: ?Sized + AbiSafeTrait>(a: &dyn DynPtrSafe<T>) -> Layout {
        Layout {
            size: a.size_of_val(),
            align: a.align_of_val(),
        }
    }

    pub fn array<T>(x: usize) -> Result<Layout, LayoutError> {
        match (
            core::mem::size_of::<T>().checked_mul(x),
            core::mem::align_of::<T>(),
        ) {
            (Some(size), align) => Ok(Layout { size, align }),
            (None, _) => Err(LayoutError(())),
        }
    }

    pub const fn size(&self) -> usize {
        self.size
    }

    pub const fn align(&self) -> usize {
        self.align
    }

    pub fn pad_to_align(&self) -> Layout {
        Layout {
            size: self.size + (self.size % self.align),
            align: self.align,
        }
    }

    pub fn align_to(&self, align: usize) -> Result<Self, LayoutError> {
        match (
            self.size.overflowing_add(self.size % align),
            align.max(self.align()),
        ) {
            ((_, true), align) if align.is_power_of_two() => Ok(Layout {
                size: self.size,
                align,
            }),
            _ => Err(LayoutError(())),
        }
    }

    pub fn padding_needed_for(&self, align: usize) -> usize {
        self.size % align
    }

    pub fn repeat(&self, n: usize) -> Result<(Self, usize), LayoutError> {
        let Layout { size, align } = self.pad_to_align();
        match (size.checked_mul(n), align) {
            (Some(asize), align) => Ok((Layout { size: asize, align }, size - self.size)),
            _ => Err(LayoutError(())),
        }
    }

    pub fn extend(&self, next: Self) -> Result<(Self, usize), LayoutError> {
        let Layout { size, align } = self.align_to(next.align())?;
        match size
            .checked_add(size % next.align())
            .map(|v| v.checked_add(next.size()))
            .unwrap_or(None)
        {
            Some(rsize) => Ok((Layout { size: rsize, align }, size - self.size())),
            None => Err(LayoutError(())),
        }
    }

    pub fn repeat_packed(&self, n: usize) -> Result<Self, LayoutError> {
        match self.size().checked_mul(n) {
            Some(size) => Ok(Layout {
                size,
                align: self.align(),
            }),
            None => Err(LayoutError(())),
        }
    }

    pub fn extend_packed(&self, next: Self) -> Result<Self, LayoutError> {
        match self.size.checked_add(next.size()) {
            Some(size) => Ok(Layout {
                size,
                align: self.align(),
            }),
            None => Err(LayoutError(())),
        }
    }

    pub fn dangling(&self) -> NonNull<u8> {
        // NOTE: this is an implementation detail
        // Future versions may offer randomization or
        unsafe { NonNull::new_unchecked(self.align as *mut u8) }
    }
}

pub unsafe trait Allocator {
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>>;
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout);

    fn allocate_zeroed(&self, layout: Layout) -> Option<NonNull<u8>> {
        let ptr = self.allocate(layout)?;
        unsafe {
            core::ptr::write_bytes(ptr.as_ptr().cast::<u8>(), 0, layout.size());
        }
        Some(ptr)
    }

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
            )
        }
        self.deallocate(ptr, old_layout);
        Some(nptr)
    }

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
            )
        }
        self.deallocate(ptr, old_layout);
        Some(nptr)
    }

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
            )
        }
        self.deallocate(ptr, old_layout);
        Some(nptr)
    }

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
        A::deallocate(self, ptr, layout)
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
        A::deallocate(self, ptr, layout)
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

#[derive(Copy, Clone)]
pub struct XLangAlloc(MaybeUninit<u8>);

impl XLangAlloc {
    pub const fn new() -> Self {
        Self(MaybeUninit::uninit())
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
        NonNull::new(unsafe { xlang_allocate_aligned(layout.size(), layout.align()).cast::<u8>() })
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        xlang_deallocate_aligned(ptr.as_ptr().cast(), layout.size(), layout.align())
    }
}

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
    core::ptr::drop_in_place(this as *mut T)
}

unsafe extern "C" fn vtbl_allocate<T: Allocator>(
    this: *const (),
    layout: Layout,
) -> Option<NonNull<u8>> {
    <T as Allocator>::allocate(&*(this as *const T), layout)
}

unsafe extern "C" fn vtbl_allocate_zeroed<T: Allocator>(
    this: *const (),
    layout: Layout,
) -> Option<NonNull<u8>> {
    <T as Allocator>::allocate_zeroed(&*(this as *const T), layout)
}

unsafe extern "C" fn vtbl_deallocate<T: Allocator>(
    this: *const (),
    ptr: NonNull<u8>,
    layout: Layout,
) {
    <T as Allocator>::deallocate(&*(this as *const T), ptr, layout)
}

unsafe extern "C" fn vtbl_grow<T: Allocator>(
    this: *const (),
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
) -> Option<NonNull<u8>> {
    <T as Allocator>::grow(&*(this as *const T), ptr, old_layout, new_layout)
}

unsafe extern "C" fn vtbl_grow_zeroed<T: Allocator>(
    this: *const (),
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
) -> Option<NonNull<u8>> {
    <T as Allocator>::grow_zeroed(&*(this as *const T), ptr, old_layout, new_layout)
}

unsafe extern "C" fn vtbl_shrink<T: Allocator>(
    this: *const (),
    ptr: NonNull<u8>,
    old_layout: Layout,
    new_layout: Layout,
) -> Option<NonNull<u8>> {
    <T as Allocator>::shrink(&*(this as *const T), ptr, old_layout, new_layout)
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
        (self.vtable().deallocate)(self.as_raw(), ptr, layout)
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
        (self.vtable().deallocate)(self.as_raw(), ptr, layout)
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
        (self.vtable().deallocate)(self.as_raw(), ptr, layout)
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
        (self.vtable().deallocate)(self.as_raw(), ptr, layout)
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
        <dyn DynPtrSafe<T> as Allocator>::deallocate(&**self, ptr, layout)
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
        <dyn DynPtrSafe<T> as Allocator>::deallocate(&**self, ptr, layout)
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

unsafe impl<'lt, T: ?Sized + AbiSafeTrait + 'static, A: Allocator> Allocator for DynBox<T, A>
where
    dyn DynPtrSafe<T>: Allocator,
{
    fn allocate(&self, layout: Layout) -> Option<NonNull<u8>> {
        <dyn DynPtrSafe<T> as Allocator>::allocate(&**self, layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        <dyn DynPtrSafe<T> as Allocator>::deallocate(&**self, ptr, layout)
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

pub fn handle_alloc_error(layout: Layout) -> ! {
    unsafe { xlang_on_allocation_failure(layout.size(), layout.align()) }
}