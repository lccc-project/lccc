use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

pub fn group(ts: TokenStream, delim: Delimiter) -> impl Iterator<Item = TokenTree> {
    [TokenTree::Group(Group::new(delim, ts))].into_iter()
}

pub fn ident(id: &str, span: Span) -> impl Iterator<Item = TokenTree> {
    [TokenTree::Ident(Ident::new(id, span))].into_iter()
}

pub fn punct(punct: &str) -> impl Iterator<Item = TokenTree> {
    let mut punct = punct.chars();

    let last = punct
        .next_back()
        .expect("`punct` requires at least one character");

    let mut ts = TokenStream::new();

    ts.extend(
        punct
            .map(|c| Punct::new(c, Spacing::Joint))
            .map(TokenTree::Punct),
    );
    ts.extend([TokenTree::Punct(Punct::new(last, Spacing::Alone))]);

    ts.into_iter()
}
