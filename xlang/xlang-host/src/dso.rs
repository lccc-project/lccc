use std::ffi::{c_void, OsStr};

#[cfg(unix)] // TODO: Replace with proper host feature detection
mod dlcfn;

#[cfg(unix)]
use dlcfn as sys;

/// SAFETY:
/// [`Self`] must be a fn pointer type
pub unsafe trait FnPtr: Sized {
    /// SAFETY:
    /// This can produce a safe function pointer
    /// The function must be callable within the lifetime of the bounding dso, must match the abi of the symbol,
    /// And, if safe, must have defined behaviour for all statically valid inputs
    unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>;
}

macro_rules! impl_a_lot_of_fn_ptrs{
    {
        $(($($id:ident),*)),*
    } => {
            $(
            unsafe impl<R, $($id),*> FnPtr for extern"C" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }
            unsafe impl<R, $($id),*> FnPtr for unsafe extern"C" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }
            #[cfg(all(windows,target_arch="x86"))]
            unsafe impl<R, $($id),*> FnPtr for extern"fastcall" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }
            #[cfg(all(windows,target_arch="x86"))]
            unsafe impl<R, $($id),*> FnPtr for unsafe extern"fastcall" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }

            #[cfg(all(windows,target_arch="x86"))]
            unsafe impl<R, $($id),*> FnPtr for extern"stdcall" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }
            #[cfg(all(windows,target_arch="x86"))]
            unsafe impl<R, $($id),*> FnPtr for unsafe extern"stdcall" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }
            )*
    }
}

impl_a_lot_of_fn_ptrs! {
    (),
    (A),
    (A,B),
    (A,B,C),
    (A,B,C,D),
    (A,B,C,D,E),
    (A,B,C,D,E,F)
}

pub struct Handle {
    raw: sys::RawHandle,
}

impl Handle {
    pub fn open<S: AsRef<OsStr> + ?Sized>(s: &S) -> std::io::Result<Self> {
        sys::RawHandle::open(s.as_ref()).map(|raw| Self { raw })
    }

    pub fn open_self() -> std::io::Result<Self> {
        sys::RawHandle::open_self().map(|raw| Self { raw })
    }

    pub unsafe fn data_sym<T, S: AsRef<OsStr> + ?Sized>(&self, s: &S) -> Option<&T> {
        self.raw.get_sym_raw(s.as_ref()).cast::<T>().as_ref()
    }

    pub unsafe fn data_sym_mut<T, S: AsRef<OsStr> + ?Sized>(&self, s: &S) -> Option<&mut T> {
        self.raw.get_sym_raw(s.as_ref()).cast::<T>().as_mut()
    }

    /// SAFETY:
    /// The returned function cannot be called outside of the lifetime of the entire Handle.
    pub unsafe fn function_sym<F: FnPtr, S: AsRef<OsStr> + ?Sized>(&self, s: &S) -> Option<F> {
        F::from_raw_ptr(self.raw.get_sym_raw(s.as_ref()))
    }
}
