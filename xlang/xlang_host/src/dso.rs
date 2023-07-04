use std::ffi::{c_void, OsStr};

#[cfg(unix)] // TODO: Replace with proper host feature detection
mod dlcfn;

#[cfg(unix)]
use dlcfn as sys;

#[cfg(windows)] // TODO: Replace with proper host feature detection
mod libloaderapi;

#[cfg(windows)]
use libloaderapi as sys;

///
/// An unsafe helper trait used by [`Handle::function_sym`]
///
/// # Safety
/// [`Self`] must be a fn pointer type.
/// Note that it is invalid to implemented this trait in downstream code as function pointer types are not supported.
pub unsafe trait FnPtr: Sized {
    /// Converts a raw pointer into a
    ///
    /// # Safety
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
            unsafe impl<R, $($id),*> FnPtr for extern "C" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }
            unsafe impl<R, $($id),*> FnPtr for unsafe extern "C" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }

            #[cfg(any(has_feature_c_unwind ="stable",has_feature_c_unwind="nightly"))]
            unsafe impl<R, $($id),*> FnPtr for extern "C-unwind" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }

            #[cfg(any(has_feature_c_unwind ="stable",has_feature_c_unwind="nightly"))]
            unsafe impl<R, $($id),*> FnPtr for unsafe extern "C-unwind" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }

            #[cfg(all(windows,target_arch="x86",any(has_feature_c_unwind ="stable",has_feature_c_unwind="nightly")))]
            unsafe impl<R, $($id),*> FnPtr for extern "fastcall-unwind" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }
            #[cfg(all(windows,target_arch="x86",any(has_feature_c_unwind ="stable",has_feature_c_unwind="nightly")))]
            unsafe impl<R, $($id),*> FnPtr for unsafe extern "fastcall-unwind" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }

            #[cfg(all(windows,target_arch="x86"))]
            unsafe impl<R, $($id),*> FnPtr for extern "fastcall" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }
            #[cfg(all(windows,target_arch="x86"))]
            unsafe impl<R, $($id),*> FnPtr for unsafe extern "fastcall" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }

            #[cfg(all(windows,target_arch="x86",any(has_feature_c_unwind ="stable",has_feature_c_unwind="nightly")))]
            unsafe impl<R, $($id),*> FnPtr for extern "stdcall-unwind" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }
            #[cfg(all(windows,target_arch="x86",any(has_feature_c_unwind ="stable",has_feature_c_unwind="nightly")))]
            unsafe impl<R, $($id),*> FnPtr for unsafe extern "stdcall-unwind" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }

            #[cfg(all(windows,target_arch="x86"))]
            unsafe impl<R, $($id),*> FnPtr for extern "stdcall" fn($($id),*)->R{
                unsafe fn from_raw_ptr(p: *mut c_void) -> Option<Self>{
                    core::mem::transmute(p)
                }
            }
            #[cfg(all(windows,target_arch="x86"))]
            unsafe impl<R, $($id),*> FnPtr for unsafe extern "stdcall" fn($($id),*)->R{
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

/// A type that models a handle to a dynamically loaded library.
pub struct Handle {
    raw: sys::RawHandle,
}

impl Handle {
    /// Obtains a Handle to a file with a given name. This may be an absolute or relative path, or a full library file name (in which case, it is searched for in a platform specific manner).
    /// Returns an Error if loading the file fails
    ///
    /// Dropping the handle will close the dynamic library. All references and functions must not be used after that point, and any vtable types MUST be dropped before dropping the handle
    /// If the handle is leaked (IE. via [`core::mem::forget`]), it is sound to use functions and references obtained from it
    ///
    /// # Errors
    /// Returns an unspecified, descriptiive error, if opening the module with the given name fails, or if the current platform does not support dynamic loading of modules
    pub fn open<S: AsRef<OsStr> + ?Sized>(s: &S) -> std::io::Result<Self> {
        sys::RawHandle::open(s.as_ref()).map(|raw| Self { raw })
    }

    /// Obtains a Handle to the current module if possible.
    /// Returns an Error if accessing the current module fails.
    ///
    /// The handle returned by [`Handle::open_self`] does not close the module it references on drop.
    /// Thus it is sound to maintain functions and references obtained from the handle after dropping the module
    ///
    /// # Errors
    /// Returns an unspecified, descriptive error, if opening the current module fails or cannot be performed on the current platform
    pub fn open_self() -> std::io::Result<Self> {
        sys::RawHandle::open_self().map(|raw| Self { raw })
    }

    /// Given the name of a symbol, returns a reference to the data of that symbol if it exists, or None otherwise
    ///
    /// # Safety
    ///
    /// This function statically bounds the lifetime of the returned reference to avoid dangling references.
    /// However, care must still be taken to avoid undefined behaviour.
    /// * The type of the data must be at least the same size as and compatible with `T` (in particular, the memory at the symbol must not produce an invalid value of `T`)
    /// * The symbol must not be mutated for the lifetime of `T` in any code, and
    /// * A mutable reference may not be taken (including via [`Handle::data_sym_mut`]) to the same symbol
    pub unsafe fn data_sym<T, S: AsRef<OsStr> + ?Sized>(&self, s: &S) -> Option<&T> {
        self.raw.get_sym_raw(s.as_ref()).cast::<T>().as_ref()
    }

    /// Given the name of a symbol, returns a reference to the data of that symbol if it exists, or None otherwise
    ///
    /// # Safety
    ///
    /// This function statically bounds the lifetime of the returned reference to avoid dangling references.
    /// However, care must still be taken to avoid undefined behaviour.
    /// * The type of the data must be at least the same size as and compatible with `T` (in particular, the memory at the symbol must not produce an invalid value of `T`)
    /// * The symbol must not be accessed for the lifetime of `T` in any code,
    /// * No other references may be taken to the same symbol,
    /// * The symbol must reside in mutable memory, and (reguardless of whether it is) must not be a function that is called by any code
    pub unsafe fn data_sym_mut<T, S: AsRef<OsStr> + ?Sized>(&self, s: &S) -> Option<&mut T> {
        self.raw.get_sym_raw(s.as_ref()).cast::<T>().as_mut()
    }

    /// Given the name of a symbol, returns a function pointer to that symbol if it exists, or None otherwise
    ///
    /// # Safety
    ///
    /// This function does *not* statically bound the lifetime of the function (as rust has no native mechanism for that).
    /// Thus, the caller is responsible for ensuring that the returned pointer (which may be a safe function pointer) is not called after dropping the Handle.
    /// Further, certain types may call functions within the defining modules (such as `DynBox`]or other abi-safe trait object wrappers),
    ///  and such types cannot be soundly used after dropping the handle.
    ///
    /// In addition to lifetime concerns, the following restrictions are placed on the symbol, and apply when calling the function pointer:
    /// * The symbol must reside in executable memory and that memory may not be modified by any code after obtaining fn pointer,
    /// * The symbol must have the ABI given by `F`, including it's parameters and ABI tag, and
    /// * The function must be called with arguments that produce valid values of the corresponding parameter types, and the return value must be a valid value of the return type
    ///
    /// # Notes
    /// Due to limitations of the rust type system, this may only be called with (safe or unsafe) function pointer types with up to 6 parameters, and that use the `extern "C"`,
    /// `extern "fastcall"` (only on 32-bit x86 targets using the win32 abi), `extern"stdcall"` (only on 32-bit x86 targets using the win32 abi), or function pointer types obtained using the `rustcall` macro.
    ///
    /// Other function pointer types that may be supported by this function are unspecified and unstable.
    pub unsafe fn function_sym<F: FnPtr, S: AsRef<OsStr> + ?Sized>(&self, s: &S) -> Option<F> {
        F::from_raw_ptr(self.raw.get_sym_raw(s.as_ref()))
    }
}
