use xlang_abi::string::StringView;

#[link(name = "xlang_interface", kind = "dylib")]
extern "C" {
    pub fn xlang_get_version() -> StringView<'static>;
}
