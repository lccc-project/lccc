
#[__lccc::mangle_itanium_type_as("v")]
#[repr(i8)]
pub enum c_void{
    #[unstable(feature="lccc_c_void_internal_field",issue="none")]
    #[doc(hidden)]
    __lccc_c_void_internal_field = 0xFF,
    #[unstable(feature="lccc_c_void_internal_field_2",issue="none")]
    #[doc(hidden)]
    __lccc_c_void_internal_field_2 = 0
}

#[repr(transparent)]
#[unstable (feature="c_variadic",issue="44930")]
pub struct VaList<'a,'f: 'a >{
    #[cfg(any(
        all(
            not(target_arch = "aarch64"),
            not(target_arch = "powerpc"),
            not(target_arch = "x86_64")
        ),
        all(target_arch = "aarch64", target_os = "ios"),
        target_arch = "wasm32",
        target_arch = "asmjs",
        windows
    ))]
    inner: VaListImpl<'f>,
    #[cfg(all(
        any(target_arch = "aarch64", target_arch = "powerpc", target_arch = "x86_64"),
        any(not(target_arch = "aarch64"), not(target_os = "ios")),
        not(target_arch = "wasm32"),
        not(target_arch = "asmjs"),
        not(windows)
    ))]
    inner: &'a mut VaListImpl<'f>,
    phantom: PhantomData<&'a mut VaListImpl<'f>>
}



impl<'a,'f: 'a> Deref for VaList<'a,'f>{
    type Target = VaListImpl<'f>;
    #[cfg(any(
        all(
            not(target_arch = "aarch64"),
            not(target_arch = "powerpc"),
            not(target_arch = "x86_64")
        ),
        all(target_arch = "aarch64", target_os = "ios"),
        target_arch = "wasm32",
        target_arch = "asmjs",
        windows
    ))]
    fn deref(&self) ->&VaListImpl<'f>{
        &self.inner
    }

    #[cfg(all(
        any(target_arch = "aarch64", target_arch = "powerpc", target_arch = "x86_64"),
        any(not(target_arch = "aarch64"), not(target_os = "ios")),
        not(target_arch = "wasm32"),
        not(target_arch = "asmjs"),
        not(windows)
    ))]
    fn deref(&self) -> &VaListImpl<'f>{
        self.inner
    }
}

impl<'a,'f: 'a> DerefMut for VaList<'a,'f>{
    #[cfg(any(
        all(
            not(target_arch = "aarch64"),
            not(target_arch = "powerpc"),
            not(target_arch = "x86_64")
        ),
        all(target_arch = "aarch64", target_os = "ios"),
        target_arch = "wasm32",
        target_arch = "asmjs",
        windows
    ))]
    fn deref(&self) ->&mut VaListImpl<'f>{
        &mut self.inner
    }

    #[cfg(all(
        any(target_arch = "aarch64", target_arch = "powerpc", target_arch = "x86_64"),
        any(not(target_arch = "aarch64"), not(target_os = "ios")),
        not(target_arch = "wasm32"),
        not(target_arch = "asmjs"),
        not(windows)
    ))]
    fn deref(&self) -> &mut VaListImpl<'f>{
        self.inner
    }
}

#[repr(transparent)]
#[unstable (feature="c_variadic",issue="44930")]
pub struct VaListImpl<'f>{
    // typedef va_list __builtin_va_list;
    _impl: ::__lccc::builtins::C::__builtin_va_list,
    phantom: PhantomData<&'f mut &'f c_void>
}

mod private{
    #[unstable (feature="c_variadic",issue="44930")]
    pub unsafe trait VaArgSafe{} 
}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for i8{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for u8{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for i16{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for u16{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for i32{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for u32{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for i64{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for u64{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for isize{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for usize{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for f32{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl private::VaArgSafe for f64{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl<T> private::VaArgSafe for *mut T{}
#[unstable (feature="c_variadic",issue="44930")]
unsafe impl<T> private::VaArgSafe for *const T{}

#[unstable (feature="c_variadic",issue="44930")]
impl<'f> VaListImpl<'f>{
    #[unstable (feature="c_variadic",issue="44930")]
    pub unsafe fn arg<T: private::VaArgSafe>(&mut self) -> T{
        __lccc::builtins::C::__builtin_va_arg<T>(&mut self._impl)
    }
    #[unstable (feature="c_variadic",issue="44930")]
    pub unsafe fn with_copy<F: FnOnce(VaList<'_,'f>)->R,R>(&mut self,f: F) -> R{
        let x =  core::mem::zeroed();
        __lccc::builtins::C::__builtin_va_copy(&mut self._impl,&mut x.inner._impl);
        f(x)
    }
    #[unstable (feature="c_variadic",issue="44930")]
    #[cfg(any(
        all(
            not(target_arch = "aarch64"),
            not(target_arch = "powerpc"),
            not(target_arch = "x86_64")
        ),
        all(target_arch = "aarch64", target_os = "ios"),
        target_arch = "wasm32",
        target_arch = "asmjs",
        windows
    ))]
    pub fn as_va_list(&mut self) -> VaList<'_,'f>{
        VaList{inner: VaListImpl{..*self},phantom: PhantomData}
    }
    #[unstable (feature="c_variadic",issue="44930")]
    #[cfg(all(
        any(target_arch = "aarch64", target_arch = "powerpc", target_arch = "x86_64"),
        any(not(target_arch = "aarch64"), not(target_os = "ios")),
        not(target_arch = "wasm32"),
        not(target_arch = "asmjs"),
        not(windows)
    ))]
    pub fn as_va_list(&mut self) -> VaList<'_,'f>{
        VaList{inner: self,phantom: PhantomData}
    }

}

#[unstable (feature="c_variadic",issue="44930")]
impl<'f> Drop for VaListImpl<'f>{
    #[lang = "va_list_drop"]
    fn drop(&mut self){}
}
