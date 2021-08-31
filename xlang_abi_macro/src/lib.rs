extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn xlang_trait(_attrs: TokenStream, ts: TokenStream) -> TokenStream {
    ts
}
