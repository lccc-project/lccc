# TCR: x86_64-*-lilium-std 

## Basic Properties

This target is for the Lilium OS using the System V ABI on x86_64. Pointers are 8 bytes in size, the target supports `std`, and unwinding is supported.

The target supports any value for the vendor field. The standard vendor is `pc`, and all other vendors are aliases that differ only in the `target_vendor` cfg which is set to the value of this field.

The standard cfg values are
```rust
target_abi=""
target_arch="x86_64"
target_endian="little"
target_env="std"
target_family="lilium"
target_has_atomic
target_has_atomic="16"
target_has_atomic="32"
target_has_atomic="64"
target_has_atomic="8"
target_has_atomic="ptr"
target_has_atomic_equal_alignment="16"
target_has_atomic_equal_alignment="32"
target_has_atomic_equal_alignment="64"
target_has_atomic_equal_alignment="8"
target_has_atomic_equal_alignment="ptr"
target_has_atomic_load_store
target_has_atomic_load_store="16"
target_has_atomic_load_store="32"
target_has_atomic_load_store="64"
target_has_atomic_load_store="8"
target_has_atomic_load_store="ptr"
target_os="lilium"
target_pointer_width="64"
target_thread_local
```

The type aliases used for `core::ffi` types are:
```rust
pub type c_char = u8;
pub type c_double = f64;
pub type c_float = f32;
pub type c_int = i32;
pub type c_long = i64;
pub type c_longlong = i64;
pub type c_schar = i8;
pub type c_short = i16;
pub type c_uchar = u8;
pub type c_uint = u32;
pub type c_ulong = u64;
pub type c_ulonglong = u64;
pub type c_ushort = u16;
pub type c_ptrdiff_t = isize;
pub type c_size_t = usize;
pub type c_ssize_t = isize;
```

## Language Feature Interactions

The following special `extern` ABIs are supported. Each ABI supports a `-unwind` variant:
* `extern "win64"`
* `extern "sysv64"`
* `extern "vectorcall"`
* `extern "efiabi"`

The `extern "C"` and `extern "system"` ABIs match `extern "sysv64"`.

All x86_64 standard target features are supported. The default target features are:
* `sse`
* `sse2`
* `fxsr`

## Target-specific Modules

The standard `core::arch::x86_64` module is defined.

The `std::ffi::OsStr` type is guaranteed to store valid UTF-8. Invalid UTF-8 in strings is rejected by kernel interfaces, and is not produced by any kernel interface that returns succesfully.

A new `std::os::lilium` module is defined, to support OS-Specific Facilities on Lilium. The initial API Surface is:
```rust
// std::os::lilium

pub trait OsStrExt : Sealed{
    /// Infallibly converts from an [`OsStr`] to a [`str`]
    fn as_str(&self) -> &str;
    /// Infallibly converts from a mutable [`OsStr`] to a [`str`]
    fn as_str_mut(&mut self) -> &mut str;
    /// Infallibly converts from a [`str`] to an [`OsStr`]
    fn from_str(st: &str) -> &Self;
    /// Infallibly converts from a mutable [`str`] to an [`OsStr`]
    fn from_str_mut(st: &mut str) -> &mut Self;
}

impl OsStrExt for std::ffi::OsStr;

pub trait OsStringExt : Sealed{
    /// Infallibly converts from an [`OsString`] to a [`String`]
    fn into_string(self) -> String;
}

impl OsStringExt for std::ffi::OsString;

#[repr(C,align(16))]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct OsUuid{
    pub lo: u64,
    pub hi: u64,
}

pub mod except{
    #[repr(C)]
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub struct ExceptionStatusInfo {
        pub except_code: OsUuid,
        pub except_info: u64,
        pub except_reason: u64,
    }
}

pub mod handle{
    /// Type representing a pointer to a handle, for doing raw operations on OS Primitives.
    #[repr(transparent)]
    pub struct RawHandlePtr<T>(*mut T);

    pub trait HandleType: Sealed{}

    pub struct AsRawHandle<T: HandleType>{
        fn as_raw_handle(&self) -> RawHandlePtr<T>;
    }

    pub struct FromRawHandle<T: HandleType> {
        unsafe fn from_raw_handle(hdl: RawHandlePtr<T>) -> Self;
    }
}

pub mod io{
    pub struct IOHandle(/*private fields*/);

    impl HandleType for IOHandle{}

    impl AsRawHandle<IOHandle> for std::fs::File;
    impl AsRawHandle<IOHandle> for std::net::TcpStream;
    impl AsRawHandle<IOHandle> for std::net::UdpSocket;
    impl AsRawHandle<IOHandle> for std::io::Stdin;
    impl AsRawHandle<IOHandle> for std::io::StdinLock;
    impl AsRawHandle<IOHandle> for std::io::Stderr;
    impl AsRawHandle<IOHandle> for std::io::StderrLock;
    impl AsRawHandle<IOHandle> for std::io::Stdout;
    impl AsRawHandle<IOHandle> for std::io::StdoutLock;
    impl AsRawHandle<IOHandle> for std::process::ChildStdin;
    impl AsRawHandle<IOHandle> for std::process::ChildStdout;
    impl AsRawHandle<IOHandle> for std::process::ChildStderr;

    impl FromRawHandle<IOHandle> for std::process::Stdio;
    
}

pub mod fs{
    pub struct FileHandle(/*private fields*/);

    impl HandleType for FileHandle;

    impl AsRawHandle<FileHandle> for std::fs::File;
    impl FromRawHandle<FileHandle> for std::fs::File;
    impl AsRawHandle<FileHandle> for std::fs::ReadDir;
    impl FromRawHandle<FileHandle> for std::fs::ReadDir{
        /// Constructs a new [`ReadDir`] from the specified handle.
        /// 
        /// ## Note
        /// If the `hdl` originally came from a [`ReadDir`], this method will return a fresh iterator that will iterate from the beginning 
        unsafe fn from_raw_handle(hdl: RawHandlePtr<FileHandle>) -> Self;
    }

    pub fn symlink<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> Result<()>;

    /// Creates a new weak link
    /// 
    /// Weak Links are features of the Lilium Filesystem, allowing a weak reference to a filesystem object.
    /// They act similiarily to hard links (cannot cross filesystems), but does not keep a hold on the object data (or most metadata).
    /// 
    /// Weak links are generally not needed for most use cases.
    pub fn weak_link<P: AsRef<Path>, Q: AsRef<Path>>(original:P, link: Q) -> Result<()>;
}

pub mod thread{
    pub struct ThreadHandle(/*private fields*/);

    impl HandleType for ThreadHandle;
    impl AsRawHandle<ThreadHandle> for std::thread::Thread;
    impl<T> AsRawHandle<ThreadHandle> for std::thread::JoinHandle<T>;
    impl<'scope, T> AsRawHandle<ThreadHandle> for std::thread::ScopedJoinHandle<'scope, T>;
}

pub mod process{
    pub struct ProcessHandle(/*private fields*/);
    impl HandleType for ProcessHandle{}

    impl AsRawHandle<ProcessHandle> for std::process::Child;

    impl FromRawHandle<ProcessHandle> for std::process::Child;

    pub trait ExitStatusExt: Sealed{
        fn exception(&self) -> Option<ExceptionStatusInfo>;
    }

    pub trait CommandExt : Sealed{
        fn arg0<S: AsRef<OsStr>>(&mut self, arg: S) -> &mut Self;
    }
}
```

The `*Handle` types are opaque in the OS design, but are provided to allow third party crates that provide more comprehensive access to the Lilium SCI and USI to use or access standard types.

### Platform Specific Behaviour

The following documents the initial implementation correspondance of certain standard library functions to Lilium. None of these are guarantees

#### `std::io::copy`

This corresponds to calling the SCI function `IOCopyFull` if possible. Support is available based on implementation of the `AsRawHandle<IOHandle>` trait. 
If the stream being read is larger than `u64::MAX` bytes, and the stream being written to accepts the full size copy, returns `u64::MAX`

#### `std::fs::copy`

This corresponds to calling the SCI function `CopyObject` and specifying all streams are to gbe copied. 
The return value is the total size of all streams copied. Like `std::io::copy` if that value exceeds `u64::MAX` bytes, it returns `u64::MAX`.
 Note that if the stream copied carries out of band data (such as a device file) only the size of the actual stream (not the out-of-band data) is counted. 
 For example, copying 

#### `std::env::var`

This corresponds to calling the SCI function `GetEnvironmentVariable` with a null map (the default map for the process). 

Due to the fact that the kernel will only yield (and permit) UTF-8 values for environment variables, this function will never return `std::env::VarError::NotUnicode`.

#### `std::io::IsTerminal`

The `is_terminal` method corresponds to querying the `Interactive` device feature on the device corresponding with the object's `IOHandle` (with flags set to 0). The device is obtained from the raw `IOHandle` as follows:
* If the Handle is a `DeviceHandle`, uses it directly in `TestDeviceFeature`
* If the Handle is a `FileHandle`, the `DeviceHandle` is obtained using `OpenDeviceFromFile`. If this succeeds, the result is used in `TestDeviceFeature`
* If it's neither a `FileHandle` nor a `DeviceHandle` or it is a `FileHandle` and `OpenDeviceFromFile` fails, the function returns `false`.

The function returns `true` iff the call to `TestDeviceFeature` succeeds.

## Deviations

The Lilium OS is a non-Unix-like non-Windows OS - it does not support POSIX-like interfaces, or win32 api. 
It is thus incompatible with any platform-dependant code in existance today. 
The OS was specificially designed to not be constrained by POSIX, allowing more interesting design space to be explored in its design. The Rust design, and the implementation of the platform in lccc reflects this unique design, rather than trying to incompletely weld an interface compatible with most Rust programs (those using `std::os::unix`) onto an incompatible OS Interface. 

A new value for `target_family`, `"lilium"` is used by the target. This reflects the uniqueness of its interface, and the possibility of other kernels matching the published standard kernel interface.

Like most targets supported by lccc, the `target_vendor` cfg is determined by the target name, rather than using a fixed value for all of the covered targets.