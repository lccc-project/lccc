use crate::marker::{Sized, Sync};
use crate::ops::CoereceUnsized;

#[repr(transparent)]
pub struct UnsafeCell<T: ?Sized>{
    #[__lccc::xlang::field_attributes(mutable)]
    inner: T
}


impl<T> UnsafeCell<T>{
    pub const fn new(val: T) -> Self{
        Self{inner: val}
    }
    pub fn into_inner(self) -> T{
        self.inner
    }
}

impl<T: ?Sized> UnsafeCell<T>{
    pub const fn get(&self) -> *mut T{
        self as *const Self as *mut Self as *mut T
    }

    #[unstable(feature = "unsafe_cell_raw_get", issue = "66358")]
    pub const fn get_raw(this: *const Self) -> *mut T{
        // NOTE:
        // This is valid in lccc because of pointer-interconvertibility,
        //  even if `this` comes from a &UnsafeCell<T>. 
        // It is valid under strict SB for an entirely different reason
        this as *mut Self as *mut T
    }

    pub fn get_mut(&mut self) -> &mut T{
        &mut self.inner
    }
}

impl<T: ?Sized> !Sync for UnsafeCell<T>{}

impl<T: CoereceUnsized<U>,U> CoereceUnsized<UnsafeCell<U>> for UnsafeCell<T>{}


#[repr(transparent)]
pub struct Cell<T: ?Sized>{
    inner: UnsafeCell<T>
}

impl<T> Cell<T>{
    pub const fn new(x: T) -> Self{
        Self{inner: UnsafeCell::new(x)}
    }

    pub fn set(&self,x: T){
        // SAFETY:
        // Pointer is from &self
        unsafe{crate::ptr::write(self.inner.get(),x)}
    }

    pub fn swap(&self,other: &Self){
        // SAFETY:
        // Pointers are from &Self
        unsafe{crate::ptr::swap(self.inner.get(),other.inner.get())}
    }

    pub fn replace(&self,x: T) -> T{
        // SAFETY:
        // pointers are from &self, and ops are balanced (no double drop)
        unsafe{
            let tmp = crate::ptr::read(self.inner.get());
            crate::ptr::write(self.inner.get(),x);
            tmp
        }
    }

    pub fn into_inner(self) -> T{
        self.inner.into_inner()
    }
}

impl<T: Copy> Cell<T>{
    pub fn get(&self) -> T{
        // SAFETY:
        // pointer is from &self, and T: Copy
        unsafe{core::ptr::read(self.inner.get())}
    }

    #[unstable(feature="cell_update",issue="50186")]
    pub fn update<F: FnOnce(T)->T>(&self,f: F) -> T{
        let val = (f)(self.get());
        self.set(val);
        val
    }
}

impl<T: ?Sized> Cell<T>{
    pub const fn as_ptr(&self) -> *mut T{
        self.inner.get()
    }

    pub fn get_mut(&mut self) -> &mut T{
        // SAFETY:
        // self is borrowed mutably, which enforces the inner borrow
        unsafe{&mut *self.inner.get()}
    }

    pub fn from_mut(r: &mut T) -> &Cell<T>{
        // SAFETY:
        // pointer is from a &mut T.
        // So this is valid to borrow as & with a mutable field.
        unsafe{&*(r as *mut T as *mut Self)}
    }
}

impl<T: Default> Cell<T>{
    pub fn take(&self) -> T{
        // SAFETY:
        // ptr from &self, ops are balanced.
        unsafe{
            let tmp = core::ptr::read(self.inner.get());
            core::ptr::write(self.inner.get(),Default::default());
            tmp
        }
    }
}

impl<T> Cell<[T]>{
    pub fn as_slice_of_cells(&self) -> &[Cell<T>]{
        // SAFETY:
        // 
        unsafe{
            let ptr = self as *const Self as *const [T];
            core::slice::from_raw_parts(ptr.as_ptr() as *const Cell<T>,ptr.len())
        }
    }
}

pub struct RefCell<T: ?Sized>{
    mborrow: Cell<bool>,
    scount: Cell<usize>,
    inner: UnsafeCell<T>
}

pub struct Ref<'a,T: ?Sized + 'a>{
    inner: &'a T,
    scount: &'a Cell<usize>
}

impl<'a,T: ?Sized + 'a> Ref<'a,T>{
    pub fn clone<'b>(this: &Self) -> Ref<'b,T> where 'a: 'b{
        this.scount.update(|s|s.checked_add(1).expect("Too many RefCell borrows (exceeds usize::MAX)"));
        Ref{inner: this.inner,scount: this.scount}
    }
}


impl<'a,T: ?Sized + 'a> Deref for Ref<'a,T>{
    type Target = T;
    fn deref(&self) -> &T{
        self.inner
    }
}

impl<'a,T: ?Sized + 'a> Drop for Ref<'a,T>{
    fn drop(&mut self){
        // Why do we need atomics for something clearly thread safe?
        // ¯\_(ツ)_/¯
        ::__lccc::xir!("destroy sequence atomic acquire":[self.inner]);
        self.scount.update(|s|s-1);
    }
}
