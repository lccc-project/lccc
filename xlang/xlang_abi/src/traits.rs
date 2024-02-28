//!
//! # ABI Safe Trait Objects
//! This module provides the low and mid-level api for ABI-Safe Trait objects
//! Most user code will interact with the `DynRef`, `DynMut`, and `DynBox` types.
//! The rest of the api is to support those types and the `#[abi_safe]` attribute defined by the `xlang_abi_macro` crate
//!
//! ## Defining an ABI Safe Trait
//! Any trait can be defined as ABI Safe if it meets the following requirements:
//! 1. The trait must be Object Safe
//! 2. All trait methods that are not bound by Self: Sized must have an abi-safe receiver (Either `&Self`, `&mut Self`, `Box<Self>`, or `Pin<P>` where `P` is one of the preceeding types)
//! 3. Every trait method not bound by Self: Sized must be declared with an explicit abi, which should not be extern"Rust", extern"rust-call", or extern"rust-intrinsic"
//! 4. Every trait method bound by Self: Sized must have a default impl (this is required as the `#[abi_safe]` macro produces code that implements the trait, and can't implement Self: Sized methods)
//! 5. Unsafe traits may have additional requirements on the Preconditions of implementors, as the macro produces implementations of the trait that forward to the vtable
//!
//! Every trait that satisfies the above requirements can use the `#[abi_safe]` attribute macro to be used with ABI-Safe Trait Object types defined by this library
//! ### Example
//! ```ignore
//! # #[macro_use]
//! # extern crate xlang_abi_macro;
//!
//! #[abi_safe]
//! pub trait Foo{
//!     fn bar(&self);
//!     fn baz(&mut self);
//! }
//! ```
//!
//! ## Using an ABI Safe Trait
//! Abi safe traits can be used with the `DynRef`, `DynMut`, `DynBox`, and `DynPtr` types in this module,
//! by calling the `unsize_*` methods of those types on concrete pointers/references to types implementing the trait (note, this does not work if you have already unsize the type to a language trait object, like `&dyn Foo`).
//! You can then use these types with the methods of the trait
//!
//! ## Manually Implementing `AbiSafeTrait`
//! While it is recommend to use the attribute to implement the various `AbiSafe` traits, it is possible to implement them manually.
//! All of these traits are unsafe and impose numerous preconditions on implmenetors, any of which will result in very unsound code if violated.
//! That being said, if you accept the Nasal Demons you are invoking, it can be fairly simple, and, in some cases, advantageous to implement the traits manually.
//! For example, manual implementations can use additional recievers (usually combined with the `arbitrary_self_types` feature) or to provide custom implementations of
//! some trait methods for abi-safe trait objects (for example, if the trait is unsafe, and forwarding to the vtable would violate the safety invariant).
//! Manual implementations can also provide custom vtables, including for types that don't implement the trait, allowing special types to be unsized into references.
//!
//! Before attempting this, you must ensure the trait in use is object safe, and it is recommended that each method have an explicit abi other than extern"Rust", extern"rust-call", or extern"rust-intrinsic".
//!
//! ### Vtables
//! The first requirement is to create a trait's abi safe `VTable`. This must be a `#[repr(C)]` type,
//! and the first four (non 1-ZST) members must be the members of the `AbiSafeVTableHead` type (size, alignment, destructor, reserved dellocation function)
//! The remainder of the members should typically be the virtual functions of the trait (the methods not bound on Self: Sized). Each member should be public
//!

#![allow(clippy::borrow_as_ptr)]

use core::marker::PhantomData;
use std::{
    marker::PhantomPinned,
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::NonNull,
};

///
///##  Safety
/// * `construct_vtable_for()` must return the same value for the same type Self in repeated calls
/// * `construct_vtable_for().size()` must equal `size_of::<Self>()`
/// * `construct_vtable_for().size()` must equal `align_of::<Self>()`
/// * `construct_vtable_for().drop_in_place(x)` must perform no operation other than `core::ptr::drop_in_place(x as *mut Self)` if the preconditions are met
pub unsafe trait AbiSafeUnsize<T>: AbiSafeTrait {
    /// Returns a reference to the vtable for `T`
    fn construct_vtable_for() -> &'static Self::VTable;
}

///
/// A type that can be created from a raw pointer erased by this library
/// # Safety
/// Implementors of the trait shall ensure that:
/// 1. The parameter of [`FromReciever::from_raw_ptr`] is not modified by that function
/// 2. Given the preconditions of [`FromReciever::from_raw_ptr`] are satisfied, the returned value shall refer to the same storage as x, and shall have a subset of the provenance of x (and shall have provenance to it's entire storage)
/// 3. It is valid to access a value of type T from the return value according to the rules of [`Self`]
pub unsafe trait FromReciever {
    /// The target type that `Self` dereferences to
    type Target;
    ///
    /// Produces a pointer with the type of [`Self`] to the memory region pointed to by x
    /// ## Safety
    /// The preconditions of this function depends on [`Self`]
    unsafe fn from_raw_ptr(x: *mut ()) -> Self;
}

unsafe impl<'lt, T> FromReciever for &'lt mut T {
    type Target = T;
    /// # Safety
    /// All requirements of a &mut T must be satisfied for 'lt
    /// In particular:
    /// - x must be non-null and well-aligned to T
    /// - x must be valid for reads and writes for the size of T
    /// - No other references may exist to the same memory region as x
    /// - Neither x nor any pointer derived from x or to the same memory region may be used to access that memory region for the duration of 'lt
    /// - x must point to a valid, initialized, value of type T
    unsafe fn from_raw_ptr(x: *mut ()) -> &'lt mut T {
        &mut *(x.cast::<T>())
    }
}

unsafe impl<'lt, T> FromReciever for &'lt T {
    type Target = T;
    /// # Safety
    /// All requirements of a &T must be satisfied for 'lt
    /// In particular:
    /// - x must be non-null and well-aligned to T
    /// - x must be valid for reads for the size of T
    /// - No mutable references may exist to the same memory region as x
    /// - Neither x nor any pointer derived from x or to the same memory region may be used to modify that memory region for the duration of 'lt, unless T is or contains an `UnsafeCell`.
    /// - x must point to a valid, initialized, value of type T
    unsafe fn from_raw_ptr(x: *mut ()) -> &'lt T {
        &*(x.cast::<T>())
    }
}

unsafe impl<P: FromReciever> FromReciever for Pin<P>
where
    P: Deref<Target = <P as FromReciever>::Target>,
{
    type Target = <P as FromReciever>::Target;
    /// # Safety
    /// The preconditions of [`FromReciever::from_raw_ptr`] for P must be upheld.
    /// In addition, this method Pins the pointed to T.
    /// Unless T implements the Unpin, then the T pointed to by x must not be moved after this method call.
    /// Additionaly, unless T implements the Unpin triyat, it's destructor must be called on the memory region before the storage is deallocated or reused for any other type
    /// These restriictions apply whether or not the pointee of x was previously pinned.
    unsafe fn from_raw_ptr(x: *mut ()) -> Self {
        Self::new_unchecked(P::from_raw_ptr(x))
    }
}

/// A valid reciever for a `T` when using `#[xlang_abi::abi_safe_trait]`
///
/// # Safety
/// `Self` must be a valid `DispatchForDyn` reciever for `T`
pub unsafe trait AbiSafeReciever<T: ?Sized + AbiSafeTrait> {}

unsafe impl<T: ?Sized + AbiSafeTrait> AbiSafeReciever<T> for &mut T {}
unsafe impl<T: ?Sized + AbiSafeTrait> AbiSafeReciever<T> for &T {}
unsafe impl<T: ?Sized + AbiSafeTrait, P: Deref + AbiSafeReciever<T>> AbiSafeReciever<T> for Pin<P> {}

/// A trait object type which can be used with the interfaces within this module to be passed safely accros module bounderies.
///
/// ## Safety
/// The implementor of `AbiSafeTrait` shall be `dyn Trait`.
pub unsafe trait AbiSafeTrait {
    /// The `VTable` for `Self`
    type VTable: AbiSafeVTable<Self> + 'static;
}

/// The header of any `VTable` produced by this module, containing fields common to the vtables of all traits
#[repr(C)]
pub struct AbiSafeVTableHead {
    /// The size of the type
    pub size: usize,
    /// The alignment of the type
    pub align: usize,
    /// Either a pointer to a function that, when called, drops the value at the pointer in place, or None.
    pub destructor: Option<xlang_host::rustcall!(unsafe extern "rustcall" fn(*mut ()))>,
    /// A function pointer that should be set to None.
    pub reserved_deallocate: Option<xlang_host::rustcall!(unsafe extern "rustcall" fn(*mut ()))>,
}

/// Types which are `VTables` for
///
/// ## Safety
/// It shall be valid to transmute &Self to &`AbiSafeVTableHead`
/// Additionally, if T: `AbiSafeTrait`, then it must be valid to transmute between implementors of `AbiSafeVTable<T>` and references thereto
pub unsafe trait AbiSafeVTable<T: ?Sized>: Sized {
    /// Returns the size field of the `VTable`
    fn size_of(&self) -> usize {
        unsafe { (*(self as *const Self).cast::<AbiSafeVTableHead>()).size }
    }
    /// Returns the align field of the `VTable`
    fn align_of(&self) -> usize {
        unsafe { (*(self as *const Self).cast::<AbiSafeVTableHead>()).align }
    }
    /// Invokes the destructor (if any) on ptr
    /// ## Safety
    /// For the duration of this function call, ptr must be:
    /// * Valid for reads and writes of size self.size()
    /// * Aligned to self.align()
    /// * Satisfy all safety and validity invariants of the pointed to type expected by the destructor
    #[allow(unused_unsafe)]
    unsafe fn drop_in_place(&self, ptr: *mut ()) {
        if let Some(f) = unsafe { (*(self as *const Self).cast::<AbiSafeVTableHead>()).destructor }
        {
            unsafe { (f)(ptr) };
        }
    }
}

/// Implemented for VTable types that match the layout of `VTable<T>` defined by the [LCRust v0 ABI](https://lightningcreations.github.io/lccc/lcrust/abi/v0#Trait-Object-Vtable-Layout)
///
/// # Safety
/// Self must match the layout prescribed above
#[allow(clippy::doc_markdown)]
pub unsafe trait TrustedVTable<T: ?Sized>: AbiSafeVTable<T> {}

use crate::alloc::{Allocator, XLangAlloc};

/// A type that wraps a raw pointer to an abi safe trait object
#[repr(C)]
pub struct DynPtr<T: ?Sized + AbiSafeTrait> {
    /// The raw, data pointer, if any
    pub ptr: *mut (),
    /// The vtable for the pointer
    pub vtable: &'static <T as AbiSafeTrait>::VTable,
    phantom: PhantomData<*const T>,
}

impl<T: ?Sized + AbiSafeTrait> DynPtr<T> {
    /// Unsizes a raw pointer into a [`DynPtr`] for `T`
    pub fn unsize_raw<U>(x: *mut U) -> Self
    where
        T: AbiSafeUnsize<U>,
    {
        Self {
            ptr: x.cast::<()>(),
            vtable: T::construct_vtable_for(),
            phantom: PhantomData,
        }
    }

    /// Obtains a `DynPtr` from the given raw parts
    pub fn from_ptr_vtable(ptr: *mut (), vtable: &'static <T as AbiSafeTrait>::VTable) -> Self {
        Self {
            ptr,
            vtable,
            phantom: PhantomData,
        }
    }

    /// Converts self into an immutable (shared) [`DynRef`].
    ///
    /// # Safety
    /// For the duration of `'lt`, the following shall hold for `self.ptr`:
    /// * It is valid for reads of the size given by `self.vtable.size()`,
    /// * It is well-aligned to `self.vtable.align()`,
    /// * The value is valid for the type the vtable was produced for, and
    /// * The pointer must not be mutated or converted into a mutable reference from any other source
    #[must_use]
    pub unsafe fn into_ref<'lt>(self) -> DynRef<'lt, T> {
        DynRef {
            inner: self,
            phantom: PhantomData,
        }
    }

    /// Converts self into an immutable (shared) [`DynRef`].
    ///
    /// # Safety
    /// For the duration of `'lt`, the following shall hold for `self.ptr`:
    /// * It is valid for reads and writes of the size given by `self.vtable.size()`,
    /// * It is well-aligned to `self.vtable.align()`,
    /// * The value is valid for the type the vtable was produced for, and
    /// * The pointer must not be accessed from another other code
    #[must_use]
    pub unsafe fn into_mut<'lt>(self) -> DynMut<'lt, T> {
        DynMut {
            inner: self,
            phantom: PhantomData,
        }
    }
}

impl<T: ?Sized + AbiSafeTrait> Clone for DynPtr<T> {
    fn clone(&self) -> Self {
        Self { ..*self }
    }
}

impl<T: ?Sized + AbiSafeTrait> Copy for DynPtr<T> {}

/// An Abi safe wrapper arround a shared reference to a trait object
#[repr(C)]
pub struct DynRef<'lt, T: ?Sized + AbiSafeTrait> {
    inner: DynPtr<T>,
    phantom: PhantomData<&'lt T>,
}

impl<'lt, T: ?Sized + AbiSafeTrait> DynRef<'lt, T> {
    ///
    /// Derefences `x` inside a pin
    #[must_use]
    pub fn as_pinned(x: &Pin<Self>) -> Pin<&(dyn DynPtrSafe<T> + 'lt)> {
        x.as_ref()
    }

    /// Unsizes a shared reference into a [`DynRef`] to `T`
    pub fn unsize_ref<U>(x: &'lt U) -> Self
    where
        T: AbiSafeUnsize<U>,
    {
        Self {
            inner: DynPtr::unsize_raw(x as *const U as *mut U),
            phantom: PhantomData,
        }
    }
}

impl<'lt, T: ?Sized + AbiSafeTrait> Clone for DynRef<'lt, T> {
    fn clone(&self) -> Self {
        Self { ..*self }
    }
}

impl<'lt, T: ?Sized + AbiSafeTrait> Copy for DynRef<'lt, T> {}

unsafe impl<'lt, T: ?Sized + AbiSafeTrait + Sync> Send for DynRef<'lt, T> {}
unsafe impl<'lt, T: ?Sized + AbiSafeTrait + Sync> Sync for DynRef<'lt, T> {}

impl<'lt, T: ?Sized + AbiSafeTrait> Deref for DynRef<'lt, T> {
    type Target = dyn DynPtrSafe<T> + 'lt;
    fn deref(&self) -> &Self::Target {
        unsafe { &*((&self.inner as *const DynPtr<T>).cast::<DynPtrSafeWrap<T>>()) }
    }
}

/// A trait is used to refer to an abi-safe pointer to a trait object
/// # Safety
/// Cannot be implemented outside of this crate
pub unsafe trait DynPtrSafe<T: ?Sized + AbiSafeTrait> {
    /// Obtains a raw (type erased) pointer
    /// This pointer is well-aligned to [`DynPtrSafe::align_of_val`] and is valid for reads of size [`DynPtrSafe::size_of_val`]
    fn as_raw(&self) -> *const ();
    /// Obtains a raw (type erased) pointer
    /// This pointer is well-aligned to [`DynPtrSafe::align_of_val`] and is valid for reads and writes of size [`DynPtrSafe::size_of_val`]
    fn as_raw_mut(&mut self) -> *mut ();
    /// Obtains a reference to the vtable
    fn vtable(&self) -> &'static T::VTable;

    /// Returns the size of the value
    fn size_of_val(&self) -> usize {
        self.vtable().size_of()
    }

    /// Returns the alignment of the value
    fn align_of_val(&self) -> usize {
        self.vtable().align_of()
    }
}

#[repr(transparent)]
struct DynPtrSafeWrap<T: ?Sized + AbiSafeTrait> {
    inner: DynPtr<T>,
    _pin: PhantomPinned,
    phantom: PhantomData<T>,
}

impl<T: ?Sized + AbiSafeTrait + Unpin> Unpin for DynPtrSafeWrap<T> {}
unsafe impl<T: ?Sized + AbiSafeTrait + Send> Send for DynPtrSafeWrap<T> {}
unsafe impl<T: ?Sized + AbiSafeTrait + Sync> Sync for DynPtrSafeWrap<T> {}

unsafe impl<T: ?Sized + AbiSafeTrait> DynPtrSafe<T> for DynPtrSafeWrap<T> {
    fn as_raw(&self) -> *const () {
        self.inner.ptr
    }

    fn as_raw_mut(&mut self) -> *mut () {
        self.inner.ptr
    }

    fn vtable(&self) -> &'static T::VTable {
        self.inner.vtable
    }
}

/// An Abi safe wrapper arround a mutable reference to a trait object
#[repr(transparent)]
pub struct DynMut<'lt, T: ?Sized + AbiSafeTrait> {
    inner: DynPtr<T>,
    phantom: PhantomData<&'lt mut T>,
}

impl<'lt, T: ?Sized + AbiSafeTrait> DynMut<'lt, T> {
    /// Reborrows self as a shared [`DynRef`] to `T`
    #[must_use]
    pub fn as_ref(&self) -> DynRef<T> {
        DynRef {
            inner: self.inner,
            phantom: PhantomData,
        }
    }

    /// Converts self into a [`DynRef`] to `T` for. Note that this consumes `self`
    #[must_use]
    pub fn into_ref(self) -> DynRef<'lt, T> {
        DynRef {
            inner: self.inner,
            phantom: PhantomData,
        }
    }

    /// Unsizes a mutable reference to `r` into a [`DynMut`] to `T`
    pub fn unsize_mut<U>(r: &'lt mut U) -> Self
    where
        T: AbiSafeUnsize<U>,
    {
        DynMut {
            inner: DynPtr::unsize_raw(r),
            phantom: PhantomData,
        }
    }

    /// Dereferences a Pinned self
    pub fn as_pinned_mut(x: &mut Pin<Self>) -> Pin<&mut (dyn DynPtrSafe<T> + 'lt)> {
        x.as_mut()
    }

    /// Dereferences a Pinned self
    #[must_use]
    pub fn as_pinned(x: &Pin<Self>) -> Pin<&dyn DynPtrSafe<T>> {
        x.as_ref()
    }

    /// Forces a reborrow coercion to shrink 'lt
    pub fn reborrow_mut(&mut self) -> DynMut<T> {
        DynMut {
            inner: self.inner,
            phantom: PhantomData,
        }
    }
}

unsafe impl<'lt, T: ?Sized + AbiSafeTrait + Send> Send for DynMut<'lt, T> {}
unsafe impl<'lt, T: ?Sized + AbiSafeTrait + Sync> Sync for DynMut<'lt, T> {}

impl<'lt, T: ?Sized + AbiSafeTrait> Deref for DynMut<'lt, T> {
    type Target = dyn DynPtrSafe<T> + 'lt;
    fn deref(&self) -> &Self::Target {
        unsafe { &*((&self.inner as *const DynPtr<T>).cast::<DynPtrSafeWrap<T>>()) }
    }
}

impl<'lt, T: ?Sized + AbiSafeTrait> DerefMut for DynMut<'lt, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *((&mut self.inner as *mut DynPtr<T>).cast::<DynPtrSafeWrap<T>>()) }
    }
}

/// An abi safe `Box` to a trait object.
#[repr(C)]
pub struct DynBox<T: ?Sized + AbiSafeTrait, A: Allocator = XLangAlloc> {
    ptr: DynPtr<T>,
    phantom: PhantomData<T>,
    alloc: A,
}

unsafe impl<T: ?Sized + AbiSafeTrait + Send, A: Allocator + Send> Send for DynBox<T, A> {}
unsafe impl<T: ?Sized + AbiSafeTrait + Sync, A: Allocator + Sync> Sync for DynBox<T, A> {}

impl<T: ?Sized + AbiSafeTrait, A: Allocator> DynBox<T, A> {
    /// Unsizes an [`Box`] into a [`DynBox`] to `T`
    pub fn unsize_box<U>(x: crate::boxed::Box<U, A>) -> Self
    where
        T: AbiSafeUnsize<U>,
    {
        let (ptr, alloc) = crate::boxed::Box::into_raw_with_alloc(x);
        let ptr = DynPtr::unsize_raw(ptr);
        Self {
            ptr,
            phantom: PhantomData,
            alloc,
        }
    }
}

impl<T: ?Sized + AbiSafeTrait, A: Allocator> Drop for DynBox<T, A> {
    fn drop(&mut self) {
        let layout = unsafe {
            crate::alloc::Layout::from_size_align_unchecked(
                self.ptr.vtable.size_of(),
                self.ptr.vtable.align_of(),
            )
        };
        let ptr = self.ptr.ptr;
        unsafe {
            self.alloc
                .deallocate(NonNull::new_unchecked(ptr.cast()), layout);
        }
    }
}

impl<T: ?Sized + AbiSafeTrait + 'static, A: Allocator> Deref for DynBox<T, A> {
    type Target = dyn DynPtrSafe<T>;

    fn deref(&self) -> &(dyn DynPtrSafe<T> + 'static) {
        unsafe { &*((&self.ptr as *const DynPtr<T>).cast::<DynPtrSafeWrap<T>>()) }
    }
}

impl<T: ?Sized + AbiSafeTrait + 'static, A: Allocator> DerefMut for DynBox<T, A> {
    fn deref_mut(&mut self) -> &mut (dyn DynPtrSafe<T> + 'static) {
        unsafe { &mut *((&mut self.ptr as *mut DynPtr<T>).cast::<DynPtrSafeWrap<T>>()) }
    }
}

xlang_host::rustcall! {
    #[doc(hidden)]
    pub unsafe extern "rustcall" fn __drop_in_place<T>(ptr: *mut ()){
        core::ptr::drop_in_place(ptr.cast::<T>())
    }
}
