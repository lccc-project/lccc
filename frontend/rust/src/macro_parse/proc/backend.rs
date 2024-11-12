pub trait LCRustFnOnce<A> {
    type Output;

    fn call_once(self, arg0: A) -> Self::Output
    where
        Self: Sized;
}

impl<A, R, F: FnOnce(A) -> R> LCRustFnOnce<A> for F {
    type Output = R;

    fn call_once(self, arg0: A) -> Self::Output
    where
        Self: Sized,
    {
        self(arg0)
    }
}

pub trait LCRustFnMut<A>: LCRustFnOnce<A> {
    fn call_mut(&mut self, arg0: A) -> Self::Output;
}

impl<A, R, F: FnMut(A) -> R> LCRustFnMut<A> for F {
    fn call_mut(&mut self, arg0: A) -> Self::Output {
        self(arg0)
    }
}

pub trait LCRustFn<A>: LCRustFnMut<A> {
    fn call(&self, arg0: A) -> Self::Output;
}

impl<A, R, F: Fn(A) -> R> LCRustFn<A> for F {
    fn call(&self, arg0: A) -> Self::Output {
        self(arg0)
    }
}

mod abi {
    use super::{LCRustFn, LCRustFnMut, LCRustFnOnce};
    use xlang::abi::rustcall;
    use xlang::abi::traits::*;

    #[repr(C)]
    pub struct LCRustFnOnceVtable<A, R> {
        pub head: AbiSafeVTableHead,
        pub call_once_unsized: rustcall!(unsafe extern "rustcall" fn(*mut (), A) -> R),
    }

    #[repr(C)]
    pub struct LCRustFnMutVtable<A, R> {
        pub fnonce: LCRustFnOnceVtable<A, R>,
        pub call_mut: rustcall!(unsafe extern "rustcall" fn(*mut (), A) -> R),
    }

    #[repr(C)]
    pub struct LCRustFnVtable<A, R> {
        pub fnmut: LCRustFnMutVtable<A, R>,
        pub call: rustcall!(unsafe extern "rustcall" fn(*const (), A) -> R),
    }

    macro_rules! generate_vtable_impls {
        (impl vtable($tr:ident) for $vtable:ident{
            for<$ty:ident> $vtable_val:expr
        }) => {
            unsafe impl<'a, A, R> AbiSafeVTable<dyn 'a + $tr<A, Output = R>> for $vtable<A, R> {}
            unsafe impl<'a, A, R> AbiSafeVTable<dyn 'a + Send + $tr<A, Output = R>>
                for $vtable<A, R>
            {
            }
            unsafe impl<'a, A, R> AbiSafeVTable<dyn 'a + Sync + $tr<A, Output = R>>
                for $vtable<A, R>
            {
            }
            unsafe impl<'a, A, R> AbiSafeVTable<dyn 'a + Send + Sync + $tr<A, Output = R>>
                for $vtable<A, R>
            {
            }

            unsafe impl<'a, A, R> TrustedVTable<dyn 'a + $tr<A, Output = R>> for $vtable<A, R> {}
            unsafe impl<'a, A, R> TrustedVTable<dyn 'a + Send + $tr<A, Output = R>>
                for $vtable<A, R>
            {
            }
            unsafe impl<'a, A, R> TrustedVTable<dyn 'a + Sync + $tr<A, Output = R>>
                for $vtable<A, R>
            {
            }
            unsafe impl<'a, A, R> TrustedVTable<dyn 'a + Send + Sync + $tr<A, Output = R>>
                for $vtable<A, R>
            {
            }

            unsafe impl<'a, A: 'static, R: 'static> AbiSafeTrait for dyn 'a + $tr<A, Output = R> {
                type VTable = $vtable<A, R>;
            }

            unsafe impl<'a, A: 'static, R: 'static> AbiSafeTrait
                for dyn 'a + Send + $tr<A, Output = R>
            {
                type VTable = $vtable<A, R>;
            }

            unsafe impl<'a, A: 'static, R: 'static> AbiSafeTrait
                for dyn 'a + Sync + $tr<A, Output = R>
            {
                type VTable = $vtable<A, R>;
            }

            unsafe impl<'a, A: 'static, R: 'static> AbiSafeTrait
                for dyn 'a + Send + Sync + $tr<A, Output = R>
            {
                type VTable = $vtable<A, R>;
            }

            unsafe impl<'a, A: 'static, R: 'static, $ty: 'a + $tr<A, Output = R>> AbiSafeUnsize<$ty>
                for dyn 'a + $tr<A, Output = R>
            {
                fn construct_vtable_for() -> &'static Self::VTable {
                    &$vtable_val
                }
            }

            unsafe impl<'a, A: 'static, R: 'static, $ty: 'a + Send + $tr<A, Output = R>>
                AbiSafeUnsize<$ty> for dyn 'a + Send + $tr<A, Output = R>
            {
                fn construct_vtable_for() -> &'static Self::VTable {
                    &$vtable_val
                }
            }

            unsafe impl<'a, A: 'static, R: 'static, $ty: 'a + Sync + $tr<A, Output = R>>
                AbiSafeUnsize<$ty> for dyn 'a + Sync + $tr<A, Output = R>
            {
                fn construct_vtable_for() -> &'static Self::VTable {
                    &$vtable_val
                }
            }

            unsafe impl<'a, A: 'static, R: 'static, $ty: 'a + Send + Sync + $tr<A, Output = R>>
                AbiSafeUnsize<$ty> for dyn 'a + Send + Sync + $tr<A, Output = R>
            {
                fn construct_vtable_for() -> &'static Self::VTable {
                    &$vtable_val
                }
            }
        };
    }

    rustcall! {
        unsafe extern "rustcall" fn destroy<T>(p: *mut ()) {
            core::ptr::drop_in_place(p as *mut T)
        }
    }

    rustcall! {
        unsafe extern "rustcall" fn call_once_unsized<T: LCRustFnOnce<A,Output=R>,A,R>(p: *mut (),arg0: A) -> R {
            unsafe { (p as *mut T).read().call_once(arg0) }
        }
    }

    rustcall! {
        unsafe extern "rustcall" fn call_mut<T: LCRustFnMut<A,Output=R>,A,R>(p: *mut (),arg0: A) -> R{
            unsafe{ &mut *(p as *mut T) }.call_mut(arg0)
        }
    }

    rustcall! {
        unsafe extern "rustcall" fn call<T: LCRustFn<A,Output=R>,A,R>(p: *const (),arg0: A) -> R{
            unsafe{ &*(p as *const T) }.call(arg0)
        }
    }

    generate_vtable_impls! {
        impl vtable(LCRustFnOnce) for LCRustFnOnceVtable {
            for<T> LCRustFnOnceVtable {
                head: AbiSafeVTableHead {
                    size: core::mem::size_of::<T>(),
                    align: core::mem::align_of::<T>(),
                    destructor: Some(destroy::<T>),
                    reserved_deallocate: None,
                },
                call_once_unsized: call_once_unsized::<T, _, _>,
            }
        }
    }

    generate_vtable_impls! {
        impl vtable(LCRustFnMut) for LCRustFnMutVtable {
            for<T> LCRustFnMutVtable {
                fnonce: LCRustFnOnceVtable {
                    head: AbiSafeVTableHead {
                        size: core::mem::size_of::<T>(),
                        align: core::mem::align_of::<T>(),
                        destructor: Some(destroy::<T>),
                        reserved_deallocate: None,
                    },
                    call_once_unsized: call_once_unsized::<T, _, _>,
                },
                call_mut: call_mut::<T, _, _>,
            }
        }
    }

    generate_vtable_impls! {
        impl vtable(LCRustFn) for LCRustFnVtable {
            for<T> LCRustFnVtable {
                fnmut: LCRustFnMutVtable {
                    fnonce: LCRustFnOnceVtable {
                        head: AbiSafeVTableHead {
                            size: core::mem::size_of::<T>(),
                            align: core::mem::align_of::<T>(),
                            destructor: Some(destroy::<T>),
                            reserved_deallocate: None,
                        },
                        call_once_unsized: call_once_unsized::<T, _, _>,
                    },
                    call_mut: call_mut::<T, _, _>,
                },
                call: call::<T, _, _>,
            }
        }
    }
}
