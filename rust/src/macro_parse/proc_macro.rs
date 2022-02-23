use xlang::abi::vec::Vec;

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TokenStream {
    tokens: Vec<TokenTree>,
}

#[repr(i8)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TokenTree {
    Ident(String),
}
