xlang_host::rustcall! {
    extern "rustcall"{
        pub fn __xlang_driver_init_rng(key: u64);
    }
}
