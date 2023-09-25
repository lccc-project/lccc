use xlang_abi::string::StringView;

xlang_host::rustcall! {
    #[link(name = "xlang_interface", kind = "dylib")]
    extern "rustcall" {
        pub fn xlang_get_version() -> StringView<'static>;

        pub fn xlang_gen_rand() -> u64;
    }
}
