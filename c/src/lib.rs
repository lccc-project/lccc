use xlang::abi::{span::Span, string::StringView};

#[no_mangle]
pub extern "C" fn xlang_plugin_main(_args: Span<StringView>) /* -> TODO: DynBox<dyn FileVisitor> */
{
}
