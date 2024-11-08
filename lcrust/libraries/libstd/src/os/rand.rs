crate::sys::generate_sys_crate! {
    mod random{
        getrandom;
        winrand;
        lilium;
        devfile;
        hardware;
        generic;
    }
}

pub fn seed_rand_buffer(x: &mut [u8]) -> std::io::Result<()> {
    sys::seed_rand_buffer(x)
}
