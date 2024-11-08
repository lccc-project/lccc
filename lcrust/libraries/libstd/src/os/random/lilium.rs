use crate::os::lilium::OsUuid;

const RANDOM_DEVICE: OsUuid = OsUuid {
    lo: 0xb80c9a996bd8001c,
    hi: 0x43c320fafe2e3322,
};

extern "system" {
    pub fn GetRandomBytes(out: *mut c_void, len: usize, source: *const OsUuid)
        -> core::ffi::c_long;
}

pub fn seed_rand_buffer(x: &mut [u8]) -> std::io::Result<()> {
    let buf = x as *mut [u8];
    let err = unsafe { GetRandomBytes(buf.cast(), buf.len(), &RANDOM_DEVICE) };
    if err != 0 {
        Err(std::io::Error::from_raw_os_error(err))
    } else {
        Ok(())
    }
}
