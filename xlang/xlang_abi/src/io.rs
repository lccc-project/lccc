use crate::{
    alloc::Allocator,
    span::{Span, SpanMut},
    string::String,
    traits::{AbiSafeTrait, AbiSafeUnsize, AbiSafeVTable, DynBox, DynMut, DynPtrSafe},
};

#[repr(u8)]
pub enum Error {
    Message(String),
}

pub type Result<T> = crate::result::Result<T, Error>;

pub trait Read {
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize>;
}

impl<R: ?Sized + Read> Read for &mut R {
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        R::read(self, buf)
    }
}

impl<R: ?Sized + Read> Read for Box<R> {
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        R::read(self, buf)
    }
}

#[repr(C)]
pub struct ReadVTable {
    size: usize,
    align: usize,
    destructor: Option<unsafe extern "C" fn(*mut ())>,
    reserved_dealloc: Option<unsafe extern "C" fn(*mut ())>,
    read: unsafe extern "C" fn(*mut (), SpanMut<u8>) -> Result<usize>,
}

unsafe impl AbiSafeVTable<dyn Read> for ReadVTable {}
unsafe impl AbiSafeVTable<dyn Read + Send> for ReadVTable {}
unsafe impl AbiSafeVTable<dyn Read + Sync> for ReadVTable {}
unsafe impl AbiSafeVTable<dyn Read + Send + Sync> for ReadVTable {}

unsafe impl AbiSafeTrait for dyn Read {
    type VTable = ReadVTable;
}
unsafe impl AbiSafeTrait for dyn Read + Send {
    type VTable = ReadVTable;
}
unsafe impl AbiSafeTrait for dyn Read + Sync {
    type VTable = ReadVTable;
}
unsafe impl AbiSafeTrait for dyn Read + Send + Sync {
    type VTable = ReadVTable;
}

unsafe extern "C" fn vtbl_destroy<T>(this: *mut ()) {
    core::ptr::drop_in_place(this.cast::<T>());
}

unsafe extern "C" fn vtbl_read<T: Read>(this: *mut (), buf: SpanMut<u8>) -> Result<usize> {
    <T as Read>::read(&mut *(this.cast::<T>()), buf)
}

unsafe impl<T: Read> AbiSafeUnsize<T> for dyn Read {
    fn construct_vtable_for() -> &'static Self::VTable {
        &ReadVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            read: vtbl_read::<T>,
        }
    }
}

unsafe impl<T: Read + Send> AbiSafeUnsize<T> for dyn Read + Send {
    fn construct_vtable_for() -> &'static Self::VTable {
        &ReadVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            read: vtbl_read::<T>,
        }
    }
}

unsafe impl<T: Read + Sync> AbiSafeUnsize<T> for dyn Read + Sync {
    fn construct_vtable_for() -> &'static Self::VTable {
        &ReadVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            read: vtbl_read::<T>,
        }
    }
}

unsafe impl<T: Read + Send + Sync> AbiSafeUnsize<T> for dyn Read + Send + Sync {
    fn construct_vtable_for() -> &'static Self::VTable {
        &ReadVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            read: vtbl_read::<T>,
        }
    }
}

impl<'lt> Read for dyn DynPtrSafe<dyn Read> + 'lt {
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        unsafe { (self.vtable().read)(self.as_raw_mut(), buf) }
    }
}

impl<'lt, T: ?Sized + AbiSafeTrait> Read for DynMut<'lt, T>
where
    dyn DynPtrSafe<T> + 'lt: Read,
{
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        <dyn DynPtrSafe<T> as Read>::read(&mut **self, buf)
    }
}

impl<'lt, T: ?Sized + AbiSafeTrait + 'static, A: Allocator> Read for DynBox<T, A>
where
    dyn DynPtrSafe<T> + 'lt: Read,
{
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        <dyn DynPtrSafe<T> as Read>::read(&mut **self, buf)
    }
}

pub trait Write {
    fn write(&mut self, buf: Span<u8>) -> Result<usize>;
}

impl<W: ?Sized + Write> Write for &mut W {
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        W::write(self, buf)
    }
}

impl<W: ?Sized + Write> Write for Box<W> {
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        W::write(self, buf)
    }
}

#[repr(C)]
pub struct WriteVTable {
    size: usize,
    align: usize,
    destructor: Option<unsafe extern "C" fn(*mut ())>,
    reserved_dealloc: Option<unsafe extern "C" fn(*mut ())>,
    write: unsafe extern "C" fn(*mut (), Span<u8>) -> Result<usize>,
}

unsafe impl AbiSafeVTable<dyn Write> for WriteVTable {}
unsafe impl AbiSafeVTable<dyn Write + Send> for WriteVTable {}
unsafe impl AbiSafeVTable<dyn Write + Sync> for WriteVTable {}
unsafe impl AbiSafeVTable<dyn Write + Send + Sync> for WriteVTable {}

unsafe impl AbiSafeTrait for dyn Write {
    type VTable = WriteVTable;
}
unsafe impl AbiSafeTrait for dyn Write + Send {
    type VTable = WriteVTable;
}
unsafe impl AbiSafeTrait for dyn Write + Sync {
    type VTable = WriteVTable;
}
unsafe impl AbiSafeTrait for dyn Write + Send + Sync {
    type VTable = WriteVTable;
}

unsafe extern "C" fn vtbl_write<T: Write>(this: *mut (), buf: Span<u8>) -> Result<usize> {
    <T as Write>::write(&mut *(this.cast::<T>()), buf)
}

unsafe impl<T: Write> AbiSafeUnsize<T> for dyn Write {
    fn construct_vtable_for() -> &'static Self::VTable {
        &WriteVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            write: vtbl_write::<T>,
        }
    }
}

unsafe impl<T: Write + Send> AbiSafeUnsize<T> for dyn Write + Send {
    fn construct_vtable_for() -> &'static Self::VTable {
        &WriteVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            write: vtbl_write::<T>,
        }
    }
}

unsafe impl<T: Write + Sync> AbiSafeUnsize<T> for dyn Write + Sync {
    fn construct_vtable_for() -> &'static Self::VTable {
        &WriteVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            write: vtbl_write::<T>,
        }
    }
}

unsafe impl<T: Write + Send + Sync> AbiSafeUnsize<T> for dyn Write + Send + Sync {
    fn construct_vtable_for() -> &'static Self::VTable {
        &WriteVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            write: vtbl_write::<T>,
        }
    }
}

impl<'lt> Write for dyn DynPtrSafe<dyn Write> + 'lt {
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        unsafe { (self.vtable().write)(self.as_raw_mut(), buf) }
    }
}

impl<'lt, T: ?Sized + AbiSafeTrait> Write for DynMut<'lt, T>
where
    dyn DynPtrSafe<T> + 'lt: Write,
{
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        <dyn DynPtrSafe<T> as Write>::write(&mut **self, buf)
    }
}

impl<'lt, T: ?Sized + AbiSafeTrait + 'static, A: Allocator> Write for DynBox<T, A>
where
    dyn DynPtrSafe<T> + 'lt: Write,
{
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        <dyn DynPtrSafe<T> as Write>::write(&mut **self, buf)
    }
}
