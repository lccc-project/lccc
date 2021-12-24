use crate::{
    alloc::Allocator,
    span::{Span, SpanMut},
    string::String,
    traits::{AbiSafeTrait, AbiSafeUnsize, AbiSafeVTable, DynBox, DynMut, DynPtrSafe},
};

#[repr(u8)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Error {
    Message(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Message(m) => m.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::Message(crate::format!("{}", e))
    }
}

impl From<Error> for std::io::Error {
    fn from(e: Error) -> Self {
        Self::new(std::io::ErrorKind::Other, e) // FIXME: This really needs to handle error codes properly
    }
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

pub trait IntoChars {
    type IntoChars: Iterator<Item = char>;
    fn into_chars(self) -> Self::IntoChars;
}

pub struct ReadIntoChars<R: Read> {
    buf: Vec<u8>,
    pos: usize,
    len: usize,
    source: R,
}

impl<R: Read> ReadIntoChars<R> {
    fn next_byte(&mut self) -> Option<u8> {
        if self.pos >= self.len {
            self.pos = 0;
            self.len = if let Result::Ok(len) = self.source.read(SpanMut::new(&mut self.buf)) {
                len
            } else {
                return None;
            }
        }
        if self.len == 0 {
            return None;
        }
        let result = self.buf[self.pos];
        self.pos += 1;
        Some(result)
    }
}

impl<R: Read> Iterator for ReadIntoChars<R> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        let first = self.next_byte()?;
        if first & 0x80 == 0 {
            Some(first as char)
        } else if first & 0xE0 == 0xC0 {
            let next = self.next_byte()?;
            assert!(
                next & 0xC0 == 0x80,
                "Got invalid UTF-8 sequence [{:#04X}, {:#04X}]",
                first,
                next
            );
            Some(unsafe {
                char::from_u32_unchecked((u32::from(first & 0x1F) << 6) | u32::from(next & 0x7F))
            })
        } else if first & 0xF0 == 0xE0 {
            let next1 = self.next_byte()?;
            let next2 = self.next_byte()?;
            assert!(
                next1 & 0xC0 == 0x80,
                "Got invalid UTF-8 sequence [{:#04X}, {:#04X}, {:#04X}]",
                first,
                next1,
                next2
            );
            assert!(
                next2 & 0xC0 == 0x80,
                "Got invalid UTF-8 sequence [{:#04X}, {:#04X}, {:#04X}]",
                first,
                next1,
                next2
            );
            Some(unsafe {
                char::from_u32_unchecked(
                    (u32::from(first & 0x0F) << 12)
                        | (u32::from(next1 & 0x7F) << 6)
                        | u32::from(next2 & 0x7F),
                )
            })
        } else {
            assert!(
                first & 0xF8 == 0xF0,
                "Got invalid UTF-8 start char {:#04X}",
                first
            );
            let next1 = self.next_byte()?;
            let next2 = self.next_byte()?;
            let next3 = self.next_byte()?;
            assert!(
                next1 & 0xC0 == 0x80,
                "Got invalid UTF-8 sequence [{:#04X}, {:#04X}, {:#04X}, {:#04X}]",
                first,
                next1,
                next2,
                next3
            );
            assert!(
                next2 & 0xC0 == 0x80,
                "Got invalid UTF-8 sequence [{:#04X}, {:#04X}, {:#04X}, {:#04X}]",
                first,
                next1,
                next2,
                next3
            );
            assert!(
                next3 & 0xC0 == 0x80,
                "Got invalid UTF-8 sequence [{:#04X}, {:#04X}, {:#04X}, {:#04X}]",
                first,
                next1,
                next2,
                next3
            );
            Some(unsafe {
                char::from_u32_unchecked(
                    (u32::from(first & 0x07) << 18)
                        | (u32::from(next1 & 0x7F) << 12)
                        | (u32::from(next2 & 0x7F) << 6)
                        | u32::from(next3 & 0x7F),
                )
            })
        }
    }
}

impl<R: Read> IntoChars for R {
    type IntoChars = ReadIntoChars<R>;
    fn into_chars(self) -> Self::IntoChars {
        ReadIntoChars::<Self> {
            buf: vec![0; 8],
            pos: 0,
            len: 0,
            source: self,
        }
    }
}

pub struct ReadAdapter<R>(R);

impl<R> ReadAdapter<R> {
    pub const fn new(r: R) -> Self {
        Self(r)
    }

    #[allow(clippy::missing_const_for_fn)]
    pub fn into_inner(self) -> R {
        self.0
    }
}

impl<R: self::Read> std::io::Read for ReadAdapter<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(SpanMut::new(buf)).map_err(Into::into).into()
    }
}

impl<R: std::io::Read> self::Read for ReadAdapter<R> {
    fn read(&mut self, mut buf: SpanMut<u8>) -> Result<usize> {
        self.0
            .read(&mut buf)
            .map_err(Into::into) // TODO: Proper error codes here
            .into()
    }
}

pub trait Write {
    fn write(&mut self, buf: Span<u8>) -> Result<usize>;
    fn flush(&mut self) -> Result<()>;
}

impl<W: ?Sized + Write> Write for &mut W {
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        W::write(self, buf)
    }
    fn flush(&mut self) -> Result<()> {
        W::flush(self)
    }
}

impl<W: ?Sized + Write> Write for Box<W> {
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        W::write(self, buf)
    }

    fn flush(&mut self) -> Result<()> {
        W::flush(self)
    }
}

#[repr(C)]
pub struct WriteVTable {
    size: usize,
    align: usize,
    destructor: Option<unsafe extern "C" fn(*mut ())>,
    reserved_dealloc: Option<unsafe extern "C" fn(*mut ())>,
    write: unsafe extern "C" fn(*mut (), Span<u8>) -> Result<usize>,
    flush: unsafe extern "C" fn(*mut ()) -> Result<()>,
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

unsafe extern "C" fn vtbl_flush<T: Write>(this: *mut ()) -> Result<()> {
    <T as Write>::flush(&mut *(this.cast::<T>()))
}

unsafe impl<T: Write> AbiSafeUnsize<T> for dyn Write {
    fn construct_vtable_for() -> &'static Self::VTable {
        &WriteVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            write: vtbl_write::<T>,
            flush: vtbl_flush::<T>,
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
            flush: vtbl_flush::<T>,
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
            flush: vtbl_flush::<T>,
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
            flush: vtbl_flush::<T>,
        }
    }
}

impl<'lt> Write for dyn DynPtrSafe<dyn Write> + 'lt {
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        unsafe { (self.vtable().write)(self.as_raw_mut(), buf) }
    }

    fn flush(&mut self) -> Result<()> {
        unsafe { (self.vtable().flush)(self.as_raw_mut()) }
    }
}

impl<'lt, T: ?Sized + AbiSafeTrait> Write for DynMut<'lt, T>
where
    dyn DynPtrSafe<T> + 'lt: Write,
{
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        <dyn DynPtrSafe<T> as Write>::write(&mut **self, buf)
    }

    fn flush(&mut self) -> Result<()> {
        <dyn DynPtrSafe<T> as Write>::flush(&mut **self)
    }
}

impl<'lt, T: ?Sized + AbiSafeTrait + 'static, A: Allocator> Write for DynBox<T, A>
where
    dyn DynPtrSafe<T> + 'lt: Write,
{
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        <dyn DynPtrSafe<T> as Write>::write(&mut **self, buf)
    }

    fn flush(&mut self) -> Result<()> {
        <dyn DynPtrSafe<T> as Write>::flush(&mut **self)
    }
}

pub struct WriteAdapter<W>(W);

impl<W> WriteAdapter<W> {
    pub const fn new(r: W) -> Self {
        Self(r)
    }

    #[allow(clippy::missing_const_for_fn)]
    pub fn into_inner(self) -> W {
        self.0
    }
}

impl<W: self::Write> std::io::Write for WriteAdapter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(Span::new(buf)).map_err(Into::into).into()
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush().map_err(Into::into).into()
    }
}

impl<W: std::io::Write> self::Write for WriteAdapter<W> {
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        self.0
            .write(&buf)
            .map_err(Into::into) // TODO: Proper error codes here
            .into()
    }

    fn flush(&mut self) -> Result<()> {
        self.0
            .flush()
            .map_err(Into::into) // TODO: Proper error codes here
            .into()
    }
}
