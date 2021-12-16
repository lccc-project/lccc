#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
use xlang::abi::{span::Span, string::StringView};

#[allow(clippy::missing_const_for_fn)]
#[no_mangle]
pub extern "C" fn xlang_plugin_main(_args: Span<StringView>) /* -> TODO: DynBox<dyn FileVisitor> */
{
}
