xlang_host::rustcall! {
    #[link(name = "xlang_interface", kind = "dylib")]
    extern "rustcall"{
        pub fn __xlang_driver_init_rng(key: u64);
    }
}
