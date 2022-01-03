use std::{
    ffi::{c_void, CStr, CString, OsStr},
    io::ErrorKind,
    os::{
        raw::{c_char, c_int},
        unix::prelude::OsStringExt,
    },
};

#[link(name = "dl")]
extern "C" {
    fn dlopen(c: *const c_char, flags: c_int) -> *mut c_void;
    fn dlclose(hdl: *mut c_void) -> c_int;
    fn dlsym(hdl: *mut c_void, sym: *const c_char) -> *mut c_void;
    fn dlerror() -> *const c_char;
}

const RTLD_NOW: c_int = 0x00002;

pub struct RawHandle {
    hdl: *mut c_void,
}

impl RawHandle {
    pub fn open(s: &OsStr) -> std::io::Result<Self> {
        let cstr = CString::new(OsStringExt::into_vec(s.to_os_string()))
            .map_err(|e| std::io::Error::new(ErrorKind::InvalidInput, e))?;
        let hdl = unsafe { dlopen(cstr.as_ptr(), RTLD_NOW) };
        if hdl.is_null() {
            let err = unsafe { dlerror() };
            assert!(!err.is_null(), "dlopen returned null but there's no error");
            let opt = unsafe { CStr::from_ptr(err) };

            Err(std::io::Error::new(
                ErrorKind::Other,
                format!("{}", opt.to_string_lossy()),
            ))
        } else {
            Ok(Self { hdl })
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    pub const fn open_self() -> std::io::Result<Self> {
        Ok(Self {
            hdl: core::ptr::null_mut(),
        })
    }

    pub fn get_sym_raw(&self, s: &OsStr) -> *mut c_void {
        let cstr = CString::new(OsStringExt::into_vec(s.to_os_string())).unwrap();
        unsafe { dlsym(self.hdl, cstr.as_ptr()) }
    }
}

impl Drop for RawHandle {
    fn drop(&mut self) {
        if !self.hdl.is_null() {
            unsafe {
                dlclose(self.hdl);
            }
        }
    }
}
