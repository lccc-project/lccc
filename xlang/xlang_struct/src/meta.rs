use core::{fmt, marker::PhantomData, mem::MaybeUninit};
use std::mem::ManuallyDrop;

use core::option::Option::{self, None, Some};
use std::ops::{Deref, DerefMut};

use xlang_abi::prelude::v1::*;

use xlang_abi::result::Result::{self, Ok};

pub mod block;
pub mod body;
pub mod file;

#[repr(C)]
#[cfg_attr(target_pointer_width = "32", repr(align(32)))]
#[cfg_attr(target_pointer_width = "64", repr(align(64)))]
pub struct Payload([MaybeUninit<usize>; 8]);

#[repr(C)]
pub struct Metadata<T> {
    data: Payload,
    ty: StringView<'static>,
    drop: Option<rustcall!(unsafe extern "rustcall" fn(SpanMut<MaybeUninit<usize>>))>,
    clone: rustcall!(
        unsafe extern "rustcall" fn(SpanMut<MaybeUninit<usize>>, Span<MaybeUninit<usize>>, bool)
    ),
    fmt: rustcall!(
        unsafe extern "rustcall" fn(
            Span<MaybeUninit<usize>>,
            *mut (),
            rustcall!(unsafe extern "rustcall" fn(*mut (), StringView) -> Result<(), ()>),
            bool,
        ) -> Result<(), ()>
    ),
    _phantom: PhantomData<T>,
}

rustcall! {
    unsafe extern "rustcall" fn fmtter_write_str(f: *mut (), st: StringView) -> Result<(), ()>{
        let fmt = unsafe{&mut *f.cast::<core::fmt::Formatter>()};
        Ok(try_!(fmt.write_str(&st).map_err(|_| ())))
    }
}

impl<T> core::fmt::Debug for Metadata<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("#[")?;
        f.write_str(&self.ty)?;
        try_!(unsafe {
            (self.fmt)(
                Span::new(&self.data.0),
                f as *mut _ as *mut (),
                fmtter_write_str,
                false,
            )
            .map_err(|_| core::fmt::Error)
        });
        f.write_str("]")
    }
}

impl<T> core::fmt::Display for Metadata<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("#[")?;
        f.write_str(&self.ty)?;
        try_!(unsafe {
            (self.fmt)(
                Span::new(&self.data.0),
                f as *mut _ as *mut (),
                fmtter_write_str,
                false,
            )
            .map_err(|_| core::fmt::Error)
        });
        f.write_str("]")
    }
}

rustcall! {
    unsafe extern "rustcall" fn clone_copy(mut a: SpanMut<MaybeUninit<usize>>, b: Span<MaybeUninit<usize>>, _: bool){
        a.copy_from_slice(&*b)
    }
}

rustcall! {
    unsafe extern "rustcall" fn fmt_empty_body(_: Span<MaybeUninit<usize>>, _: *mut (), _: rustcall!(unsafe extern "rustcall" fn(*mut (), StringView) -> Result<(), ()>), _: bool) -> Result<(), ()>{
        Ok(())
    }
}

const unsafe fn transmute_unchecked<T, U>(val: T) -> U {
    union Transmuter<T, U> {
        input: ManuallyDrop<T>,
        output: ManuallyDrop<U>,
    }

    unsafe {
        ManuallyDrop::into_inner(
            Transmuter {
                input: ManuallyDrop::new(val),
            }
            .output,
        )
    }
}

impl<T> Metadata<T> {
    pub const fn empty() -> Self {
        Self {
            ty: StringView::new("{empty}"),
            drop: None,
            clone: clone_copy,
            _phantom: PhantomData,
            fmt: fmt_empty_body,
            data: Payload([MaybeUninit::uninit(); 8]),
        }
    }

    pub const fn new<M: MetadataFor<T>>(meta: M) -> Self {
        rustcall! {
            unsafe extern "rustcall" fn clone_into_slot<T, M: MetadataFor<T>>(mut a: SpanMut<MaybeUninit<usize>>, b: Span<MaybeUninit<usize>>, same_type: bool){
                let m = unsafe{&*(b.as_ptr().cast::<M>())};

                m.clone_into_slot(&mut *a, same_type);
            }
        }

        rustcall! {
            unsafe extern "rustcall" fn drop<M>(mut a: SpanMut<MaybeUninit<usize>>){
                unsafe{core::ptr::drop_in_place(a.as_mut_ptr().cast::<M>())}
            }
        }

        rustcall! {
            unsafe extern "rustcall" fn fmt<M: core::fmt::Display + core::fmt::Debug>(a: Span<MaybeUninit<usize>>, fmt_udata: *mut (), write_str: rustcall!(unsafe extern "rustcall" fn(*mut (), StringView) -> Result<(), ()>), is_display: bool)  -> Result<(), ()> {
                use core::fmt::Write;
                struct ErasedWriter{
                    fmt_udata: *mut (),
                    write_str: rustcall!(unsafe extern "rustcall" fn(*mut (), StringView) -> Result<(), ()>),
                }

                impl core::fmt::Write for ErasedWriter{
                    fn write_str(&mut self, s: &str) -> core::fmt::Result{
                        core::fmt::Result::Ok(try_!(unsafe{(self.write_str)(self.fmt_udata, StringView::new(s))}.map_err(|_| core::fmt::Error)))
                    }
                }
                let val = unsafe{&*a.as_ptr().cast::<M>()};

                if is_display{
                    Ok(try_!(core::write!(ErasedWriter{fmt_udata, write_str}, "{val}").map_err(|_| ())))
                }else{
                    Ok(try_!(core::write!(ErasedWriter{fmt_udata, write_str}, "{val:?}").map_err(|_| ())))
                }

            }
        }

        Self {
            _phantom: PhantomData,
            ty: M::TAG,
            drop: if core::mem::needs_drop::<M>() {
                None
            } else {
                Some(drop::<M>)
            },
            clone: clone_into_slot::<T, M>,
            fmt: fmt::<M>,
            // SAFETY: All types that implement `MetadataFor` are size-bound
            data: unsafe { transmute_unchecked(meta) },
        }
    }

    pub fn get<M: MetadataFor<T>>(&self) -> Option<&M> {
        if self.ty == M::TAG {
            Some(unsafe { transmute_unchecked(&self.data) })
        } else {
            None
        }
    }

    pub fn get_mut<M: MetadataFor<T>>(&mut self) -> Option<&mut M> {
        if self.ty == M::TAG {
            Some(unsafe { transmute_unchecked(&mut self.data) })
        } else {
            None
        }
    }
}

impl<T> Default for Metadata<T> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<T> Clone for Metadata<T> {
    fn clone(&self) -> Self {
        let mut new = Self::empty();
        new.clone_from(self);
        new
    }

    fn clone_from(&mut self, source: &Self) {
        struct AbortBomb;
        impl Drop for AbortBomb {
            #[allow(unconditional_recursion)] // Wrong Lint, the recursion is needed to force an abort
            fn drop(&mut self) {
                let _bomb = AbortBomb;
                panic!();
            }
        }

        let bomb = AbortBomb;
        if self.ty == source.ty {
            unsafe {
                (self.clone)(
                    SpanMut::new(&mut self.data.0),
                    Span::new(&source.data.0),
                    true,
                );
            }
        } else {
            if let Some(drop) = self.drop {
                unsafe { drop(SpanMut::new(&mut self.data.0)) }
            }

            unsafe {
                (self.clone)(
                    SpanMut::new(&mut self.data.0),
                    Span::new(&source.data.0),
                    false,
                );
            }
        }
        core::mem::forget(bomb);
    }
}

impl<T, M> From<M> for Metadata<T>
where
    M: MetadataFor<T> + sealed::Sealed,
{
    fn from(value: M) -> Self {
        Self::new(value)
    }
}

mod sealed {
    pub trait Sealed {}
}

pub trait MetadataFor<T>: 'static + sealed::Sealed + Clone + fmt::Debug + fmt::Display {
    const TAG: StringView<'static>;

    /// Clones `self` into the specified slot.
    /// If `same_type_hint` is true, the callee may assume that the slot already contains `Self` (note that it is responsible for dropping it if it doesn't wish to apply this optimization)
    /// # Safety
    /// `clone_into_slot` must be called with a valid pointer to an array with at least 8 elements.
    /// Additionally, if `same_type_hint` is true, `slot` must contain data (up to `size_of::<Self>()`) which is initialized for `Self`
    unsafe fn clone_into_slot(&self, slot: *mut [MaybeUninit<usize>], same_type: bool) {
        if same_type {
            (unsafe { &mut *slot.cast::<Self>() }).clone_from(self)
        } else {
            unsafe { slot.cast::<Self>().write(self.clone()) }
        }
    }
}

#[derive(Clone, Debug)]
pub struct MetadataList<T>(pub Vec<Metadata<T>>);

impl<T> MetadataList<T> {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push<M: Into<Metadata<T>>>(&mut self, m: M) {
        self.0.push(m.into())
    }

    pub fn get<M: MetadataFor<T>>(&self) -> Option<&M> {
        self.0.iter().find_map(Metadata::get)
    }

    pub fn get_mut<M: MetadataFor<T>>(&mut self) -> Option<&mut M> {
        self.0.iter_mut().find_map(Metadata::get_mut)
    }
}

impl<T, A: Into<Metadata<T>>> core::iter::FromIterator<A> for MetadataList<T> {
    fn from_iter<I: IntoIterator<Item = A>>(iter: I) -> Self {
        Self(iter.into_iter().map(Into::into).collect())
    }
}

impl<T, A: Into<Metadata<T>>> core::iter::Extend<A> for MetadataList<T> {
    fn extend<I: IntoIterator<Item = A>>(&mut self, iter: I) {
        self.0.extend(iter.into_iter().map(Into::into))
    }
}

impl<T> core::fmt::Display for MetadataList<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for elem in &self.0 {
            f.write_str(" ")?;
            elem.fmt(f)?;
        }
        core::fmt::Result::Ok(())
    }
}

impl<T> Default for MetadataList<T> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<T> Deref for MetadataList<T> {
    type Target = Vec<Metadata<T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for MetadataList<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
