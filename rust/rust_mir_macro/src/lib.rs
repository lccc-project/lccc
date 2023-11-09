use parse::Error;
use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use xlang_frontend::iter::Peekmore;

// mod ast;
mod parse;

fn write_global_path<'a, I: IntoIterator<Item = &'a str>>(
    tt: &mut TokenStream,
    span: Span,
    path: I,
) {
    let mut colon1 = Punct::new(':', Spacing::Joint);
    colon1.set_span(span);
    let mut colon2 = Punct::new(':', Spacing::Alone);
    colon2.set_span(span);

    let colon1 = TokenTree::Punct(colon1);
    let colon2 = TokenTree::Punct(colon2);
    let coloncolon = [colon1, colon2];

    for id in path {
        tt.extend(coloncolon.clone());
        let id = Ident::new_raw(id, span);
        let id = TokenTree::Ident(id);
        tt.extend([id]);
    }
}

fn write_crate_path<'a, I: IntoIterator<Item = &'a str>>(
    tt: &mut TokenStream,
    span: Span,
    dolar_crate: &TokenStream,
    path: I,
) {
    tt.extend(dolar_crate.clone());
    let colon1 = Punct::new(':', Spacing::Joint);
    let colon2 = Punct::new(':', Spacing::Alone);

    let colon1 = TokenTree::Punct(colon1);
    let colon2 = TokenTree::Punct(colon2);
    let coloncolon = [colon1, colon2];

    for id in path {
        tt.extend(coloncolon.clone());
        let id = Ident::new_raw(id, span);
        let id = TokenTree::Ident(id);
        tt.extend([id]);
    }
}

fn write_error(e: Error) -> TokenStream {
    let mut ts = TokenStream::new();
    write_global_path(&mut ts, e.span, ["core", "compile_error"]);
    let mut bang = Punct::new('!', Spacing::Alone);
    bang.set_span(e.span);
    let mut inner = TokenStream::new();
    let st = Literal::string(&e.text);
    inner.extend([TokenTree::Literal(st)]);
    ts
}

#[proc_macro]
pub fn mir_impl(tt: TokenStream) -> TokenStream {
    let mut iter = tt.into_iter();

    let wrapped_dollar_crate = iter.next().unwrap();

    let dollar_crate = match wrapped_dollar_crate {
        TokenTree::Group(g) => g.stream(),
        _ => panic!("Invalid mir_impl invocation"),
    };

    todo!()
}

#[proc_macro]
pub fn mir_type_impl(tt: TokenStream) -> TokenStream {
    let mut iter = tt.into_iter();

    let wrapped_dollar_crate = iter.next().unwrap();

    let dollar_crate = match wrapped_dollar_crate {
        TokenTree::Group(g) => g.stream(),
        _ => panic!("Invalid mir_impl invocation"),
    };

    let mut tree = iter.peekmore();
    match parse::do_type(&mut tree, &dollar_crate) {
        Ok(ts) => ts,
        Err(e) => write_error(e),
    }
}
