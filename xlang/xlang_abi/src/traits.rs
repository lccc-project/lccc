//!
//! # ABI Safe Trait Objects
//! This module provides the low and mid-level api for ABI-Safe Trait objects
//! Most user code will interact with the DynRef, DynMut, and DynBox types.
//! The rest of the api is to support those types and the `#[abi_safe]` attribute defined by the xlang_abi_macro crate
//!
//! ## Defining an ABI Safe Trait
//! Any trait can be defined as ABI Safe if it meets the following requirements:
//! 1. The trait must be Object Safe
//! 2. All trait methods that are not bound by Self: Sized must have an abi-safe receiver (Either &Self, &mut Self, Box<Self>, or Pin<P> where P is one of the preceeding types)
//! 3. Every trait method not bound by Self: Sized must be declared with an explicit abi, which should not be extern"Rust", extern"rust-call", or extern"rust-intrinsic"
//! 4. Every trait method bound by Self: Sized must have a default impl (this is required as the `#[abi_safe]` macro produces code that implements the trait, and can't implement Self: Sized methods)
//! 5. Unsafe traits may have additional requirements on the Preconditions of implementors, as the macro produces implementations of the trait that forward to the vtable
//!
//! Every trait that satisfies the above requirements can use the `#[abi_safe]` attribute macro to be used with ABI-Safe Trait Object types defined by this library
//! ### Example
//! ```rust
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
//! Abi safe traits can be used with the DynRef, DynMut, DynBox, and DynPtr types in this module,
//! by calling the `unsize_*` methods of those types on concrete pointers/references to types implementing the trait (note, this does not work if you have already unsize the type to a language trait object, like `&dyn Foo`).
//! You can then use these types with the methods of the trait
//!
//! ## Manually Implementing [`AbiSafeTrait`]
//! While it is recommend to use the attribute to implement the various AbiSafe traits, it is possible to implement them manually.
//! All of these traits are unsafe and impose numerous preconditions on implmenetors, any of which will result in very unsound code if violated.
//! That being said, if you accept the Nasal Demons you are invoking, it can be fairly simple, and, in some cases, advantageous to implement the traits manually.
//! For example, manual implementations can use additional recievers (usually combined with the `arbitrary_self_types` feature) or to provide custom implementations of
//! some trait methods for abi-safe trait objects (for example, if the trait is unsafe, and forwarding to the vtable would violate the safety invariant).
//! Manual implementations can also provide custom vtables, including for types that don't implement the trait, allowing special types to be unsized into references.
//!
//! Before attempting this, you must ensure the trait in use is object safe, and it is recommended that each method have an explicit abi other than extern"Rust", extern"rust-call", or extern"rust-intrinsic".
//!
//! ### Vtables
//! The first requirement is to create a trait's abi safe VTable. This must be a `#[repr(C)]` type,
//! and the first four (non 1-ZST) members must be the members of the [`AbiSafeVTableHead`] type (size, alignment, destructor, reserved dellocation function)
//! The remainder of the members should typically be the virtual functions of the trait (the methods not bound on Self: Sized). Each member should be public
//!

use core::marker::{PhantomData, Send, Sync};
use std::{
    marker::PhantomPinned,
    ops::{Deref, DerefMut},
    pin::Pin,
};

pub struct AbiSafeToken<T: ?Sized + AbiSafeTrait, U>(PhantomData<T>, PhantomData<U>);

///
///##  SAFETY
/// * construct_vtable_for() must return the same value for the same type Self in repeated calls
/// * construct_vtable_for().size() must equal size_of::<Self>()
/// * construct_vtable_for().size() must equal align_of::<Self>()
/// * construct_vtable_for().drop_in_place(x) must perform no operation other than `core::ptr::drop_in_place(x as *mut Self)` if the preconditions are met
pub unsafe trait AbiSafeUnsize<T>: AbiSafeTrait {
    fn construct_vtable_for() -> &'static Self::VTable;
}

pub trait AbiSafeReciever<T: ?Sized + AbiSafeTrait> {}

impl<T: ?Sized + AbiSafeTrait> AbiSafeReciever<T> for &mut T {}
impl<T: ?Sized + AbiSafeTrait> AbiSafeReciever<T> for &T {}
impl<T: ?Sized + AbiSafeTrait, P: Deref + AbiSafeReciever<T>> AbiSafeReciever<T> for Pin<P> {}
impl<T: ?Sized + AbiSafeTrait> AbiSafeReciever<T> for Box<T> {}

///
/// ## SAFETY:
/// The implementor of AbiSafeTrait shall be `dyn Trait`
pub unsafe trait AbiSafeTrait {
    type VTable: AbiSafeVTable<Self> + 'static;
}

#[repr(C)]
pub struct AbiSafeVTableHead {
    pub size: usize,
    pub align: usize,
    pub destructor: Option<unsafe extern "C" fn(*mut ())>,
    pub reserved_deallocate: Option<unsafe extern "C" fn(*mut ())>,
}

///
/// ## SAFETY:
/// It shall be valid to transmute &Self to &AbiSafeVTableHead
/// Additionally, if T: AbiSafeTrait, then it must be valid to transmute between implementors of AbiSafeVTable<T> and references thereto
pub unsafe trait AbiSafeVTable<T: ?Sized>: Sized {
    /// Returns the size field of the VTable
    fn size_of(&self) -> usize {
        unsafe { (*(self as *const _ as *const AbiSafeVTableHead)).size }
    }
    /// Returns the align field of the VTable
    fn align_of(&self) -> usize {
        unsafe { (*(self as *const _ as *const AbiSafeVTableHead)).align }
    }
    /// Invokes the destructor (if any) on ptr
    /// ## SAFETY:
    /// For the duration of this function call, ptr must be:
    /// * Valid for reads and writes of size self.size()
    /// * Aligned to self.align()
    /// * Satisfy all safety and validity invariants of the pointed to type expected by the destructor
    #[allow(unused_unsafe)]
    unsafe fn drop_in_place(&self, ptr: *mut ()) {
        if let Some(f) = unsafe { (*(self as *const _ as *const AbiSafeVTableHead)).destructor } {
            unsafe { (f)(ptr) };
        }
    }
}

pub use xlang_abi_macro::*;

#[repr(C)]
pub struct DynPtr<T: ?Sized + AbiSafeTrait> {
    pub ptr: *mut (),
    pub vtable: &'static <T as AbiSafeTrait>::VTable,
    phantom: PhantomData<*const T>,
}

impl<T: ?Sized + AbiSafeTrait> DynPtr<T> {
    pub fn unsize_raw<U>(x: *mut U) -> Self
    where
        T: AbiSafeUnsize<U>,
    {
        Self {
            ptr: x as *mut (),
            vtable: T::construct_vtable_for(),
            phantom: PhantomData,
        }
    }

    pub fn from_ptr_vtable(ptr: *mut (), vtable: &'static <T as AbiSafeTrait>::VTable) -> Self {
        Self {
            ptr,
            vtable,
            phantom: PhantomData,
        }
    }

    pub unsafe fn into_ref<'lt>(self) -> DynRef<'lt, T> {
        DynRef {
            inner: self,
            phantom: PhantomData,
        }
    }

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

#[repr(C)]
pub struct DynRef<'lt, T: ?Sized + AbiSafeTrait> {
    inner: DynPtr<T>,
    phantom: PhantomData<&'lt T>,
}

impl<'lt, T: ?Sized + AbiSafeTrait> DynRef<'lt, T> {
    pub fn as_pinned(x: &Pin<Self>) -> Pin<&DynPtrSafe<T>> {
        unsafe { x.as_ref().map_unchecked(|f| &*f) }
    }

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
    type Target = DynPtrSafe<'lt, T>;
    fn deref(&self) -> &DynPtrSafe<'lt, T> {
        unsafe { &*(&self.inner as *const DynPtr<T> as *const DynPtrSafe<T>) }
    }
}

#[repr(transparent)]
pub struct DynPtrSafe<'lt, T: ?Sized + AbiSafeTrait>(
    DynPtr<T>,
    PhantomPinned,
    PhantomData<&'lt mut &'lt mut ()>,
);

// !Unpin is needed for as_pinned_ptr to be safe
impl<'lt, T: ?Sized + AbiSafeTrait + Unpin> Unpin for DynPtrSafe<'lt, T> {}

unsafe impl<'lt, T: ?Sized + AbiSafeTrait + Send> Send for DynPtrSafe<'lt, T> {}

unsafe impl<'lt, T: ?Sized + AbiSafeTrait + Sync> Sync for DynPtrSafe<'lt, T> {}

impl<'lt, T: ?Sized + AbiSafeTrait> DynPtrSafe<'lt, T> {
    ///
    /// The resulting pointer is valid for the dynamic lifetime of self, as follows:
    /// * It is NonNull
    /// * It is dereferenceable and valid for reads for self.vtable().size() bytes
    /// * It is aligned to self.vtable().align()
    /// * The pointed to value is valid to be used to call functions with &self recievers on the vtable
    /// * The pointer is immutable and Shall not be written to
    pub fn as_ptr(&self) -> *const () {
        self.0.ptr
    }

    ///
    /// The resulting pointer is valid for the dynamic lifetime of self, as follows:
    /// * It is NonNull
    /// * It is dereferenceable and valid for writes for self.vtable().size() bytes
    /// * It is aligned to self.vtable().align()
    /// * The pointed to value is valid to be used to call functions with &mut self recievers on the vtable
    pub fn as_mut_ptr(&mut self) -> *mut () {
        self.0.ptr
    }

    ///
    /// The resulting pointer is valid for the dynamic lifetime of self, as follows:
    /// * It is NonNull
    /// * It is dereferenceable and valid for reads for self.vtable().size() bytes
    /// * It is aligned to self.vtable().align()
    /// * The pointed to value is valid to be used to call functions with Pin<&Self> recievers on the vtable
    /// * The pointer is immutable and Shall not be written to
    ///
    /// Safety:
    /// The Resulting pointer is to pinned memory. You are responsible for ensuring any usage of the pointer does not violate the pinning invariant,
    /// unless the underlying (erased) type of the pointee is Unpin
    pub fn as_pinned_ptr(self: Pin<&'lt Self>) -> *const () {
        self.0.ptr
    }

    ///
    /// The resulting pointer is valid for the dynamic lifetime of self, as follows:
    /// * It is NonNull
    /// * It is dereferenceable and valid for writes for self.vtable().size() bytes
    /// * It is aligned to self.vtable().align()
    /// * The pointed to value is valid to be used to call functions with &mut self recievers on the ///
    /// The resulting pointer is valid for the dynamic lifetime of self, as follows:
    /// * It is NonNull
    /// * It is dereferenceable and valid for reads for self.vtable().size() bytes
    /// * It is aligned to self.vtable().align()
    /// * The pointed to value is valid to be used to call functions with Pin<&Self> recievers on the vtable
    /// * The pointer is immutable and Shall not be written to
    ///
    /// Safety:
    /// The Resulting pointer is to pinned memory. You are responsible for ensuring any usage of the pointer does not violate the pinning invariant,
    /// unless the underlying (erased) type of the pointee is Unpin
    pub fn as_ptr_mut_ptr(self: Pin<&'lt mut Self>) -> *mut () {
        unsafe { self.get_unchecked_mut().0.ptr }
    }

    pub fn vtable(&self) -> &'static T::VTable {
        self.0.vtable
    }
}

#[repr(C)]
pub struct DynMut<'lt, T: ?Sized + AbiSafeTrait> {
    inner: DynPtr<T>,
    phantom: PhantomData<&'lt mut T>,
}

impl<'lt, T: ?Sized + AbiSafeTrait> DynMut<'lt, T> {
    pub fn as_ref(&self) -> DynRef<T> {
        DynRef {
            inner: self.inner,
            phantom: PhantomData,
        }
    }

    pub fn into_ref(self) -> DynRef<'lt, T> {
        DynRef {
            inner: self.inner,
            phantom: PhantomData,
        }
    }

    pub fn unsize_mut<U>(r: &'lt mut U) -> Self
    where
        T: AbiSafeUnsize<U>,
    {
        DynMut {
            inner: DynPtr::unsize_raw(r),
            phantom: PhantomData,
        }
    }

    pub fn as_pinned_mut(x: &'lt mut Pin<Self>) -> Pin<&mut DynPtrSafe<T>> {
        unsafe { x.as_mut().map_unchecked_mut(|f| f) }
    }

    pub fn as_pinned(x: &Pin<Self>) -> Pin<&DynPtrSafe<T>> {
        unsafe { x.as_ref().map_unchecked(|f| f) }
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
    type Target = DynPtrSafe<'lt, T>;
    fn deref(&self) -> &Self::Target {
        unsafe { &*(&self.inner as *const DynPtr<T> as *const DynPtrSafe<T>) }
    }
}

impl<'lt, T: ?Sized + AbiSafeTrait> DerefMut for DynMut<'lt, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(&mut self.inner as *mut DynPtr<T> as *mut DynPtrSafe<T>) }
    }
}
