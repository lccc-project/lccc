use parse::Error;
use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use xlang_frontend::iter::{PeekMoreIterator, Peekmore};

// mod ast;
mod parse;
mod tt;

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
    let mut st = Literal::string(&e.text);
    st.set_span(e.span);
    inner.extend([TokenTree::Literal(st)]);

    ts.extend([
        TokenTree::Punct(bang),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, inner)),
        TokenTree::Punct(Punct::new(';', Spacing::Alone)),
    ]);

    let mut outer = TokenStream::new();
    outer.extend([TokenTree::Group(Group::new(Delimiter::Brace, ts))]);
    outer
}

fn eval_mir<I: Iterator<Item = TokenTree>>(
    mut it: I,
    entry: fn(&mut PeekMoreIterator<I>, &TokenStream) -> parse::Result<TokenStream>,
) -> TokenStream {
    let wrapped_dollar_crate = it.next().expect("You must provide the wrapped `$crate`");

    let dollar_crate = match wrapped_dollar_crate {
        TokenTree::Group(g) => g.stream(),
        _ => panic!("Invalid mir_impl invocation"),
    };

    let mut tree = it.peekmore();
    match entry(&mut tree, &dollar_crate) {
        Ok(ts) => {
            if let Some(tok) = tree.peek_next() {
                write_error(Error {
                    text: format!("Unexpected garbage after input `{}`", tok),
                    span: tok.span(),
                })
            } else {
                ts
            }
        }
        Err(e) => write_error(e),
    }
}

#[proc_macro]
pub fn mir_impl(tt: TokenStream) -> TokenStream {
    eval_mir(tt.into_iter(), parse::do_mir_fnbody)
}

#[proc_macro]
pub fn mir_type_impl(tt: TokenStream) -> TokenStream {
    eval_mir(tt.into_iter(), parse::do_type)
}

#[proc_macro]
pub fn mir_expr_impl(tt: TokenStream) -> TokenStream {
    eval_mir(tt.into_iter(), parse::do_expr)
}

#[proc_macro]
pub fn mir_fnty_impl(tt: TokenStream) -> TokenStream {
    eval_mir(tt.into_iter(), parse::do_fnty)
}

#[proc_macro]
pub fn mir_term_impl(tt: TokenStream) -> TokenStream {
    eval_mir(tt.into_iter(), parse::do_terminator)
}

#[proc_macro]
pub fn mir_stmt_impl(tt: TokenStream) -> TokenStream {
    eval_mir(tt.into_iter(), parse::do_statement)
}

#[proc_macro]
pub fn mir_basic_block_impl(tt: TokenStream) -> TokenStream {
    eval_mir(tt.into_iter(), parse::do_basic_block)
}
