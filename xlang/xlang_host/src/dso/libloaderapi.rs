use std::{
    ffi::{c_void, CString, OsStr},
    io::ErrorKind,
    os::{
        raw::c_char,
        windows::ffi::OsStrExt,
    },
};

extern "C" {
    fn LoadLibraryW(lpLibFileName: *const u16) -> *mut c_void; // (LPCWSTR) -> HMODULE
    fn FreeLibrary(hLibModule: *mut c_void) -> u32; // (HMODULE) -> BOOL
    fn GetProcAddress(hLibModule: *mut c_void, lpProcName: *const c_char) -> *mut c_void; // (HMODULE, LPCSTR) -> FARPROC
    fn GetLastError() -> u32; // () -> DWORD
}

pub struct RawHandle {
    hdl: *mut c_void,
}

impl RawHandle {
    pub fn open(s: &OsStr) -> std::io::Result<Self> {
        let mut cstr = OsStrExt::encode_wide(s).collect::<Vec<_>>();
        cstr.push(0);
        let hdl = unsafe { LoadLibraryW(cstr.as_ptr()) };
        if hdl.is_null() {
            let err = unsafe { GetLastError() };

            Err(std::io::Error::new(
                ErrorKind::Other,
                format!("Windows error code {}", err),
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
        let cstr = CString::new(s.to_os_string().into_string().unwrap()).unwrap();
        unsafe { GetProcAddress(self.hdl, cstr.as_ptr()) }
    }
}

impl Drop for RawHandle {
    fn drop(&mut self) {
        if !self.hdl.is_null() {
            unsafe {
                FreeLibrary(self.hdl);
            }
        }
    }
}
