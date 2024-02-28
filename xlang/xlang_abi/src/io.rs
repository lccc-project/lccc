#![allow(improper_ctypes_definitions)]

use crate::{
    alloc::Allocator,
    span::{Span, SpanMut},
    string::String,
    traits::{AbiSafeTrait, AbiSafeUnsize, AbiSafeVTable, DynBox, DynMut, DynPtrSafe},
};

/// The Error type returned when an io error occurs
#[repr(u8)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Error {
    /// An Error that simply contains a message and no error code
    Message(String),
    /// Indicates that something interrupted a previous I/O operation, and the operation may be repeated to resume it
    Interrupted,
    /// Indicates that an operation encountered an unexpected end of file
    UnexpectedEof,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Message(m) => m.fmt(f),
            Self::Interrupted => f.write_str("I/O Operation Interrupted"),
            Self::UnexpectedEof => f.write_str("Unexpected End of File"),
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

/// Type alias to a Result with [`Error`].
pub type Result<T> = crate::result::Result<T, Error>;

/// ABI Safe version of the [`std::io::Read`] trait
pub trait Read {
    /// Reads from `self` into `buf`
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize>;

    /// Reads the entire `buf` from `self`. Returns an error if not all of the requested bytes are available
    fn read_exact(&mut self, mut buf: SpanMut<u8>) -> Result<()>
    where
        Self: Sized,
    {
        while !buf.is_empty() {
            match self.read(buf.reborrow_mut()) {
                Result::Ok(0) => return Result::Err(Error::UnexpectedEof),
                Result::Ok(n) => {
                    buf = buf.into_subspan(n..).unwrap();
                }
                Result::Err(Error::Interrupted) => {}
                Result::Err(e) => return Result::Err(e),
            }
        }
        Result::Ok(())
    }
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

/// The `VTable` used for the [`Read`] trait
#[derive(Clone, Copy)]
#[repr(C)]
pub struct ReadVTable {
    size: usize,
    align: usize,
    destructor: Option<unsafe extern "C" fn(*mut ())>,
    reserved_dealloc: Option<unsafe extern "C" fn(*mut ())>,
    read: unsafe extern "C" fn(*mut (), SpanMut<u8>) -> Result<usize>,
}

unsafe impl<'a> AbiSafeVTable<dyn Read + 'a> for ReadVTable {}
unsafe impl<'a> AbiSafeVTable<dyn Read + Send + 'a> for ReadVTable {}
unsafe impl<'a> AbiSafeVTable<dyn Read + Sync + 'a> for ReadVTable {}
unsafe impl<'a> AbiSafeVTable<dyn Read + Send + Sync + 'a> for ReadVTable {}

unsafe impl<'a> AbiSafeTrait for dyn Read + 'a {
    type VTable = ReadVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn Read + Send + 'a {
    type VTable = ReadVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn Read + Sync + 'a {
    type VTable = ReadVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn Read + Send + Sync + 'a {
    type VTable = ReadVTable;
}

unsafe extern "C" fn vtbl_destroy<T>(this: *mut ()) {
    core::ptr::drop_in_place(this.cast::<T>());
}

unsafe extern "C" fn vtbl_read<T: Read>(this: *mut (), buf: SpanMut<u8>) -> Result<usize> {
    <T as Read>::read(&mut *(this.cast::<T>()), buf)
}

unsafe impl<'a, T: Read + 'a> AbiSafeUnsize<T> for dyn Read + 'a {
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

unsafe impl<'a, T: Read + Send + 'a> AbiSafeUnsize<T> for dyn Read + Send + 'a {
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

unsafe impl<'a, T: Read + Sync + 'a> AbiSafeUnsize<T> for dyn Read + Sync + 'a {
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

unsafe impl<'a, T: Read + Send + Sync + 'a> AbiSafeUnsize<T> for dyn Read + Send + Sync + 'a {
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

impl<'a, 'lt> Read for dyn DynPtrSafe<dyn Read + 'a> + 'lt
where
    'a: 'lt,
{
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        unsafe { (self.vtable().read)(self.as_raw_mut(), buf) }
    }
}

impl<'a, 'lt> Read for dyn DynPtrSafe<dyn Read + Send + 'a> + 'lt
where
    'a: 'lt,
{
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        unsafe { (self.vtable().read)(self.as_raw_mut(), buf) }
    }
}

impl<'a, 'lt> Read for dyn DynPtrSafe<dyn Read + Sync + 'a> + 'lt
where
    'a: 'lt,
{
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        unsafe { (self.vtable().read)(self.as_raw_mut(), buf) }
    }
}

impl<'a, 'lt> Read for dyn DynPtrSafe<dyn Read + Send + Sync + 'a> + 'lt
where
    'a: 'lt,
{
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

/// A trait to produce iterators over the [`char`]s read from an input stream
pub trait IntoChars {
    /// The iterator returned from `.into_chars`
    type IntoChars: Iterator<Item = char>;

    /// produces an iterator over the chars read from `self`
    fn into_chars(self) -> Self::IntoChars;
}

/// An Iterator over the chars of `R`
pub struct ReadIntoChars<R> {
    // Why is there a trait bound here
    buf: Vec<u8>,
    pos: usize,
    len: usize,
    source: R,
}

impl<R: Read> ReadIntoChars<R> {
    /// Reads a single byte from the stream
    pub fn next_byte(&mut self) -> Option<u8> {
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

/// A type which Adapts [`std::io::Read`]ers into [`self::Read`]ers and [`self::Read`]ers into [`std::io::Read`]ers
pub struct ReadAdapter<R>(R);

impl<R> ReadAdapter<R> {
    /// Produces a new [`ReadAdapter`] over `r`
    pub const fn new(r: R) -> Self {
        Self(r)
    }

    /// Obtains the inner `Read`er contained by `self`
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

#[repr(u8)]
/// Enumeration of possible methods to seek within an I/O object.
pub enum SeekFrom {
    /// Sets the offset to the provided number of bytes.
    Start(u64),
    /// Sets the offset to the size of this object plus the specified number of bytes.
    End(i64),
    /// Sets the offset to the current position plus the specified number of bytes.
    Current(i64),
}

impl From<std::io::SeekFrom> for SeekFrom {
    fn from(other: std::io::SeekFrom) -> Self {
        match other {
            std::io::SeekFrom::Start(x) => Self::Start(x),
            std::io::SeekFrom::End(x) => Self::End(x),
            std::io::SeekFrom::Current(x) => Self::Current(x),
        }
    }
}

impl From<SeekFrom> for std::io::SeekFrom {
    fn from(other: SeekFrom) -> Self {
        match other {
            SeekFrom::Start(x) => Self::Start(x),
            SeekFrom::End(x) => Self::End(x),
            SeekFrom::Current(x) => Self::Current(x),
        }
    }
}

/// ABI Safe version of the [`std::io::Seek`] trait
pub trait Seek {
    /// Seek to an offset, in bytes, in a stream.
    fn seek(&mut self, pos: SeekFrom) -> Result<u64>;
}

impl<S: ?Sized + Seek> Seek for &mut S {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        S::seek(self, pos)
    }
}

impl<S: ?Sized + Seek> Seek for Box<S> {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        S::seek(self, pos)
    }
}

/// The `VTable` used for the [`Seek`] trait
#[derive(Clone, Copy)]
#[repr(C)]
pub struct SeekVTable {
    size: usize,
    align: usize,
    destructor: Option<unsafe extern "C" fn(*mut ())>,
    reserved_dealloc: Option<unsafe extern "C" fn(*mut ())>,
    seek: unsafe extern "C" fn(*mut (), SeekFrom) -> Result<u64>,
}

unsafe impl<'a> AbiSafeVTable<dyn Seek + 'a> for SeekVTable {}
unsafe impl<'a> AbiSafeVTable<dyn Seek + Send + 'a> for SeekVTable {}
unsafe impl<'a> AbiSafeVTable<dyn Seek + Sync + 'a> for SeekVTable {}
unsafe impl<'a> AbiSafeVTable<dyn Seek + Send + Sync + 'a> for SeekVTable {}

unsafe impl<'a> AbiSafeTrait for dyn Seek + 'a {
    type VTable = SeekVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn Seek + Send + 'a {
    type VTable = SeekVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn Seek + Sync + 'a {
    type VTable = SeekVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn Seek + Send + Sync + 'a {
    type VTable = SeekVTable;
}

unsafe extern "C" fn vtbl_seek<T: Seek>(this: *mut (), pos: SeekFrom) -> Result<u64> {
    <T as Seek>::seek(&mut *(this.cast::<T>()), pos)
}

unsafe impl<'a, T: Seek + 'a> AbiSafeUnsize<T> for dyn Seek + 'a {
    fn construct_vtable_for() -> &'static Self::VTable {
        &SeekVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            seek: vtbl_seek::<T>,
        }
    }
}
unsafe impl<'a, T: Seek + Send + 'a> AbiSafeUnsize<T> for dyn Seek + Send + 'a {
    fn construct_vtable_for() -> &'static Self::VTable {
        &SeekVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            seek: vtbl_seek::<T>,
        }
    }
}
unsafe impl<'a, T: Seek + Send + 'a> AbiSafeUnsize<T> for dyn Seek + Sync + 'a {
    fn construct_vtable_for() -> &'static Self::VTable {
        &SeekVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            seek: vtbl_seek::<T>,
        }
    }
}
unsafe impl<'a, T: Seek + Send + 'a> AbiSafeUnsize<T> for dyn Seek + Send + Sync + 'a {
    fn construct_vtable_for() -> &'static Self::VTable {
        &SeekVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(vtbl_destroy::<T>),
            reserved_dealloc: None,
            seek: vtbl_seek::<T>,
        }
    }
}

impl<'a, 'lt> Seek for dyn DynPtrSafe<dyn Seek + 'a> + 'lt
where
    'a: 'lt,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        unsafe { (self.vtable().seek)(self.as_raw_mut(), pos) }
    }
}
impl<'a, 'lt> Seek for dyn DynPtrSafe<dyn Seek + Send + 'a> + 'lt
where
    'a: 'lt,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        unsafe { (self.vtable().seek)(self.as_raw_mut(), pos) }
    }
}
impl<'a, 'lt> Seek for dyn DynPtrSafe<dyn Seek + Sync + 'a> + 'lt
where
    'a: 'lt,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        unsafe { (self.vtable().seek)(self.as_raw_mut(), pos) }
    }
}
impl<'a, 'lt> Seek for dyn DynPtrSafe<dyn Seek + Send + Sync + 'a> + 'lt
where
    'a: 'lt,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        unsafe { (self.vtable().seek)(self.as_raw_mut(), pos) }
    }
}
impl<'lt, T: ?Sized + AbiSafeTrait> Seek for DynMut<'lt, T>
where
    dyn DynPtrSafe<T> + 'lt: Seek,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        <dyn DynPtrSafe<T> as Seek>::seek(&mut **self, pos)
    }
}
impl<'lt, T: ?Sized + AbiSafeTrait + 'static, A: Allocator> Seek for DynBox<T, A>
where
    dyn DynPtrSafe<T> + 'lt: Seek,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        <dyn DynPtrSafe<T> as Seek>::seek(&mut **self, pos)
    }
}

/// A type which Adapts [`std::io::Seek`]ers into [`self::Seek`]ers and [`self::Seek`]ers into
/// [`std::io::Seek`]ers
pub struct SeekAdapter<S>(S);

impl<S> SeekAdapter<S> {
    /// Produces a new [`SeekAdapter`] over `s`
    pub const fn new(s: S) -> Self {
        Self(s)
    }

    /// Obtains the inner `Seek`er contained by `self`
    #[allow(clippy::missing_const_for_fn)]
    pub fn into_inner(self) -> S {
        self.0
    }
}

impl<S: self::Seek> std::io::Seek for SeekAdapter<S> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        self.0.seek(pos.into()).map_err(Into::into).into()
    }
}

impl<S: std::io::Seek> self::Seek for SeekAdapter<S> {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        self.0.seek(pos.into()).map_err(Into::into).into()
    }
}

/// An abi safe combination of the [`std::io::Read`] and [`std::io::Seek`] traits
pub trait ReadSeek: Read + Seek {}

impl<T: Read + Seek> ReadSeek for T {}

/// The `VTable` used for the [`ReadSeek`] helper trait
#[derive(Clone, Copy)]
#[repr(C)]
pub struct ReadSeekVTable {
    read: ReadVTable,
    seek: SeekVTable,
}

unsafe impl<'a> AbiSafeVTable<dyn ReadSeek + 'a> for ReadSeekVTable {}
unsafe impl<'a> AbiSafeVTable<dyn ReadSeek + Send + 'a> for ReadSeekVTable {}
unsafe impl<'a> AbiSafeVTable<dyn ReadSeek + Sync + 'a> for ReadSeekVTable {}
unsafe impl<'a> AbiSafeVTable<dyn ReadSeek + Send + Sync + 'a> for ReadSeekVTable {}

unsafe impl<'a> AbiSafeTrait for dyn ReadSeek + 'a {
    type VTable = ReadSeekVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn ReadSeek + Send + 'a {
    type VTable = ReadSeekVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn ReadSeek + Sync + 'a {
    type VTable = ReadSeekVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn ReadSeek + Send + Sync + 'a {
    type VTable = ReadSeekVTable;
}

unsafe impl<'a, T: ReadSeek + 'a> AbiSafeUnsize<T> for dyn ReadSeek + 'a {
    fn construct_vtable_for() -> &'static Self::VTable {
        &ReadSeekVTable {
            read: ReadVTable {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                destructor: Some(vtbl_destroy::<T>),
                reserved_dealloc: None,
                read: vtbl_read::<T>,
            },
            seek: SeekVTable {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                destructor: Some(vtbl_destroy::<T>),
                reserved_dealloc: None,
                seek: vtbl_seek::<T>,
            },
        }
    }
}
unsafe impl<'a, T: ReadSeek + 'a> AbiSafeUnsize<T> for dyn ReadSeek + Send + 'a {
    fn construct_vtable_for() -> &'static Self::VTable {
        &ReadSeekVTable {
            read: ReadVTable {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                destructor: Some(vtbl_destroy::<T>),
                reserved_dealloc: None,
                read: vtbl_read::<T>,
            },
            seek: SeekVTable {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                destructor: Some(vtbl_destroy::<T>),
                reserved_dealloc: None,
                seek: vtbl_seek::<T>,
            },
        }
    }
}
unsafe impl<'a, T: ReadSeek + 'a> AbiSafeUnsize<T> for dyn ReadSeek + Sync + 'a {
    fn construct_vtable_for() -> &'static Self::VTable {
        &ReadSeekVTable {
            read: ReadVTable {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                destructor: Some(vtbl_destroy::<T>),
                reserved_dealloc: None,
                read: vtbl_read::<T>,
            },
            seek: SeekVTable {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                destructor: Some(vtbl_destroy::<T>),
                reserved_dealloc: None,
                seek: vtbl_seek::<T>,
            },
        }
    }
}
unsafe impl<'a, T: ReadSeek + 'a> AbiSafeUnsize<T> for dyn ReadSeek + Send + Sync + 'a {
    fn construct_vtable_for() -> &'static Self::VTable {
        &ReadSeekVTable {
            read: ReadVTable {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                destructor: Some(vtbl_destroy::<T>),
                reserved_dealloc: None,
                read: vtbl_read::<T>,
            },
            seek: SeekVTable {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                destructor: Some(vtbl_destroy::<T>),
                reserved_dealloc: None,
                seek: vtbl_seek::<T>,
            },
        }
    }
}
impl<'a, 'lt> Read for dyn DynPtrSafe<dyn ReadSeek + 'a> + 'lt
where
    'a: 'lt,
{
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        unsafe { (self.vtable().read.read)(self.as_raw_mut(), buf) }
    }
}
impl<'a, 'lt> Read for dyn DynPtrSafe<dyn ReadSeek + Send + 'a> + 'lt
where
    'a: 'lt,
{
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        unsafe { (self.vtable().read.read)(self.as_raw_mut(), buf) }
    }
}
impl<'a, 'lt> Read for dyn DynPtrSafe<dyn ReadSeek + Sync + 'a> + 'lt
where
    'a: 'lt,
{
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        unsafe { (self.vtable().read.read)(self.as_raw_mut(), buf) }
    }
}
impl<'a, 'lt> Read for dyn DynPtrSafe<dyn ReadSeek + Send + Sync + 'a> + 'lt
where
    'a: 'lt,
{
    fn read(&mut self, buf: SpanMut<u8>) -> Result<usize> {
        unsafe { (self.vtable().read.read)(self.as_raw_mut(), buf) }
    }
}

impl<'a, 'lt> Seek for dyn DynPtrSafe<dyn ReadSeek + 'a> + 'lt
where
    'a: 'lt,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        unsafe { (self.vtable().seek.seek)(self.as_raw_mut(), pos) }
    }
}
impl<'a, 'lt> Seek for dyn DynPtrSafe<dyn ReadSeek + Send + 'a> + 'lt
where
    'a: 'lt,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        unsafe { (self.vtable().seek.seek)(self.as_raw_mut(), pos) }
    }
}
impl<'a, 'lt> Seek for dyn DynPtrSafe<dyn ReadSeek + Sync + 'a> + 'lt
where
    'a: 'lt,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        unsafe { (self.vtable().seek.seek)(self.as_raw_mut(), pos) }
    }
}
impl<'a, 'lt> Seek for dyn DynPtrSafe<dyn ReadSeek + Send + Sync + 'a> + 'lt
where
    'a: 'lt,
{
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        unsafe { (self.vtable().seek.seek)(self.as_raw_mut(), pos) }
    }
}

/// A type fusing [`ReadAdapter`] and [`SeekAdapter`]
pub struct ReadSeekAdapter<T>(T);

impl<T> ReadSeekAdapter<T> {
    /// Produces a new [`ReadSeekAdapter`] over `t`
    pub const fn new(t: T) -> Self {
        Self(t)
    }

    /// Obtains the inner `ReadSeek`er contained by `self`\
    #[allow(clippy::missing_const_for_fn)]
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<R: self::Read> std::io::Read for ReadSeekAdapter<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(SpanMut::new(buf)).map_err(Into::into).into()
    }
}

impl<R: std::io::Read> self::Read for ReadSeekAdapter<R> {
    fn read(&mut self, mut buf: SpanMut<u8>) -> Result<usize> {
        self.0.read(&mut buf).map_err(Into::into).into()
    }
}

impl<S: self::Seek> std::io::Seek for ReadSeekAdapter<S> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        self.0.seek(pos.into()).map_err(Into::into).into()
    }
}

impl<S: std::io::Seek> self::Seek for ReadSeekAdapter<S> {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        self.0.seek(pos.into()).map_err(Into::into).into()
    }
}

/// An abi safe version of the [`std::io::Write`] trait
pub trait Write {
    /// Writes the bytes in `buf` into `self` and returns the number of bytes written, or an error if the write fails
    fn write(&mut self, buf: Span<u8>) -> Result<usize>;
    /// Flushes the internal buffer of `self`, if any, and returns an errors that occur
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

/// The `VTable` to use for the [`Write`] trait.
#[repr(C)]
pub struct WriteVTable {
    size: usize,
    align: usize,
    destructor: Option<unsafe extern "C" fn(*mut ())>,
    reserved_dealloc: Option<unsafe extern "C" fn(*mut ())>,
    write: unsafe extern "C" fn(*mut (), Span<u8>) -> Result<usize>,
    flush: unsafe extern "C" fn(*mut ()) -> Result<()>,
}

unsafe impl<'a> AbiSafeVTable<dyn Write + 'a> for WriteVTable {}
unsafe impl<'a> AbiSafeVTable<dyn Write + Send + 'a> for WriteVTable {}
unsafe impl<'a> AbiSafeVTable<dyn Write + Sync + 'a> for WriteVTable {}
unsafe impl<'a> AbiSafeVTable<dyn Write + Send + Sync + 'a> for WriteVTable {}

unsafe impl<'a> AbiSafeTrait for dyn Write + 'a {
    type VTable = WriteVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn Write + Send + 'a {
    type VTable = WriteVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn Write + Sync + 'a {
    type VTable = WriteVTable;
}
unsafe impl<'a> AbiSafeTrait for dyn Write + Send + Sync + 'a {
    type VTable = WriteVTable;
}

unsafe extern "C" fn vtbl_write<T: Write>(this: *mut (), buf: Span<u8>) -> Result<usize> {
    <T as Write>::write(&mut *(this.cast::<T>()), buf)
}

unsafe extern "C" fn vtbl_flush<T: Write>(this: *mut ()) -> Result<()> {
    <T as Write>::flush(&mut *(this.cast::<T>()))
}

unsafe impl<'a, T: Write + 'a> AbiSafeUnsize<T> for dyn Write + 'a {
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

unsafe impl<'a, T: Write + Send + 'a> AbiSafeUnsize<T> for dyn Write + Send + 'a {
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

unsafe impl<'a, T: Write + Sync + 'a> AbiSafeUnsize<T> for dyn Write + Sync + 'a {
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

unsafe impl<'a, T: Write + Send + Sync + 'a> AbiSafeUnsize<T> for dyn Write + Send + Sync + 'a {
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

impl<'a, 'lt> Write for dyn DynPtrSafe<dyn Write + 'a> + 'lt
where
    'a: 'lt,
{
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        unsafe { (self.vtable().write)(self.as_raw_mut(), buf) }
    }

    fn flush(&mut self) -> Result<()> {
        unsafe { (self.vtable().flush)(self.as_raw_mut()) }
    }
}

impl<'a, 'lt> Write for dyn DynPtrSafe<dyn Write + Send + 'a> + 'lt
where
    'a: 'lt,
{
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        unsafe { (self.vtable().write)(self.as_raw_mut(), buf) }
    }

    fn flush(&mut self) -> Result<()> {
        unsafe { (self.vtable().flush)(self.as_raw_mut()) }
    }
}

impl<'a, 'lt> Write for dyn DynPtrSafe<dyn Write + Sync + 'a> + 'lt
where
    'a: 'lt,
{
    fn write(&mut self, buf: Span<u8>) -> Result<usize> {
        unsafe { (self.vtable().write)(self.as_raw_mut(), buf) }
    }

    fn flush(&mut self) -> Result<()> {
        unsafe { (self.vtable().flush)(self.as_raw_mut()) }
    }
}

impl<'a, 'lt> Write for dyn DynPtrSafe<dyn Write + Send + Sync + 'a> + 'lt
where
    'a: 'lt,
{
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

/// A type which Adapts [`std::io::Write`]rs into [`self::Write`]rs and [`self::Write`]rs into [`std::io::Write`]rs
pub struct WriteAdapter<W>(W);

impl<W> WriteAdapter<W> {
    /// Returns a new [`WriteAdapter`] for `W`
    pub const fn new(r: W) -> Self {
        Self(r)
    }

    /// Returns the value contained within `self`.
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
