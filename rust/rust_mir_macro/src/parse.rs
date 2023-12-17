use proc_macro::{
    token_stream, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
};

use xlang_frontend::{
    iter::{with_rewinder_accept_on_continue, IntoRewinder, PeekMoreIterator, Peekmore},
    parse::{do_alternation, take_left, take_right},
};

use crate::write_crate_path;

pub struct Error {
    pub text: String,
    pub span: Span,
}

pub type Result<T> = core::result::Result<T, Error>;

pub fn do_punct<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    punct: &str,
) -> Result<Span> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let mut tok = String::new();
        let mut span = None;
        while let Some(TokenTree::Punct(c)) = tree.peek_next() {
            tok.push(c.as_char());
            let _ = span.get_or_insert(c.span());
            if c.spacing() != Spacing::Joint {
                break;
            }
        }

        if tok != punct {
            return Err(Error {
                text: format!("Expected {}", punct),
                span: span.unwrap_or_else(Span::call_site),
            });
        }

        return Ok(span.unwrap());
    })
}

pub fn do_literal<I: Iterator<Item = TokenTree>>(
    tree: &mut PeekMoreIterator<I>,
) -> Result<Literal> {
    with_rewinder_accept_on_continue(tree, |tree| match tree.peek_next() {
        Some(TokenTree::Literal(lit)) => Ok(lit.clone()),
        Some(tt) => Err(Error {
            text: format!("Expected a literal, got {}", tt),
            span: tt.span(),
        }),
        None => Err(Error {
            text: format!("Expected a literal, but got EOF"),
            span: Span::call_site(),
        }),
    })
}

pub fn do_ident<I: Iterator<Item = TokenTree>>(tree: &mut PeekMoreIterator<I>) -> Result<Ident> {
    with_rewinder_accept_on_continue(tree, |tree| match tree.peek_next() {
        Some(TokenTree::Ident(id)) => Ok(id.clone()),
        Some(tt) => Err(Error {
            text: format!("Expected identifier, got {}", tt),
            span: tt.span(),
        }),
        None => Err(Error {
            text: format!("Expected an identifier, but got EOF"),
            span: Span::call_site(),
        }),
    })
}

pub fn do_type<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    do_alternation(
        tokens,
        [
            do_user_type,
            do_int_type,
            do_float_type,
            do_primitive_type,
            do_never,
        ],
        take_right,
        dollar_crate,
    )
}

pub fn do_interpolation<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
) -> Result<Ident> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        do_punct(tree, "$")?;

        do_ident(tree)
    })
}

fn write_interpolation_for_type<'a, I: IntoIterator<Item = &'a str>>(
    ty_path: I,
    dollar_crate: &TokenStream,
    interp: Ident,
) -> TokenStream {
    let mut inner_stream = TokenStream::new();

    let idspan = dollar_crate.clone().into_iter().next().unwrap().span();

    let letkw = TokenTree::Ident(Ident::new("let", idspan));

    let id = TokenTree::Ident(Ident::new("__interp", idspan));
    let id2 = id.clone();

    let colon = TokenTree::Punct(Punct::new(':', Spacing::Alone));

    inner_stream.extend([letkw, id, colon]);

    write_crate_path(&mut inner_stream, idspan, dollar_crate, ty_path);

    let eq = TokenTree::Punct(Punct::new('=', Spacing::Alone));

    let interp = TokenTree::Ident(interp);
    let semi = TokenTree::Punct(Punct::new(';', Spacing::Alone));

    inner_stream.extend([eq, interp, semi, id2]);

    let mut stream = TokenStream::new();

    let group = Group::new(proc_macro::Delimiter::Bracket, inner_stream);

    stream.extend([TokenTree::Group(group)]);

    let group = Group::new(
        proc_macro::Delimiter::Parenthesis,
        core::mem::take(&mut stream),
    );

    stream.extend([TokenTree::Group(group)]);

    stream
}

pub fn do_defid<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let span = do_punct(tree, "#")?;
        let mut token_stream = TokenStream::new();
        match do_literal(tree) {
            Ok(lit) => {
                write_crate_path(
                    &mut token_stream,
                    lit.span(),
                    dollar_crate,
                    ["sema", "DefId", "__new_unchecked"],
                );
                let mut group = TokenStream::new();
                group.extend([TokenTree::Literal(lit)]);
                let group = Group::new(proc_macro::Delimiter::Parenthesis, group);
                token_stream.extend([TokenTree::Group(group)]);
                Ok(token_stream)
            }
            Err(_) => match do_interpolation(tree) {
                Ok(interp) => Ok(write_interpolation_for_type(
                    ["sema", "DefId"],
                    dollar_crate,
                    interp,
                )),
                Err(d) => Err(d),
            },
        }
    })
}

pub fn do_type_interpolation<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let interp = do_interpolation(tokens)?;
    Ok(write_interpolation_for_type(
        ["sema", "ty", "Type"],
        dollar_crate,
        interp,
    ))
}

pub fn do_user_type<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let defid = do_defid(tokens, dollar_crate)?;
    let mut token_stream = TokenStream::new();
    write_crate_path(
        &mut token_stream,
        Span::call_site(),
        dollar_crate,
        ["sema", "ty", "Type", "UserType"],
    );

    let group = Group::new(proc_macro::Delimiter::Parenthesis, defid);

    let group = TokenTree::Group(group);
    token_stream.extend([group]);

    Ok(token_stream)
}

pub fn do_int_type<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let id = do_ident(tokens)?;
    let span = id.span();
    let id = id.to_string();
    let id = id.trim_start_matches("r#");
    if !(id.starts_with("i") || id.starts_with("u")) {
        return Err(Error {
            text: format!("Expected an integer type, got {}", id),
            span,
        });
    }
    let mut token_stream = TokenStream::new();
    write_crate_path(
        &mut token_stream,
        Span::call_site(),
        dollar_crate,
        ["sema", "ty", "Type", "Int"],
    );
    let mut inner = TokenStream::new();
    write_crate_path(
        &mut inner,
        Span::call_site(),
        dollar_crate,
        ["sema", "ty", "IntType", id],
    );
    let group = Group::new(proc_macro::Delimiter::Parenthesis, inner);
    token_stream.extend([TokenTree::Group(group)]);

    Ok(token_stream)
}

pub fn do_float_type<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let id = do_ident(tokens)?;
    let span = id.span();
    let id = id.to_string();
    let id = id.trim_start_matches("r#");
    if !id.starts_with("f") {
        return Err(Error {
            text: format!("Expected an integer type, got {}", id),
            span,
        });
    }
    let mut token_stream = TokenStream::new();
    write_crate_path(
        &mut token_stream,
        span,
        dollar_crate,
        ["sema", "ty", "Type", "Float"],
    );
    let mut inner = TokenStream::new();
    write_crate_path(
        &mut inner,
        span,
        dollar_crate,
        ["sema", "ty", "FloatWidth", id],
    );
    let group = Group::new(proc_macro::Delimiter::Parenthesis, inner);
    token_stream.extend([TokenTree::Group(group)]);

    Ok(token_stream)
}

pub fn do_primitive_type<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let id = do_ident(tokens)?;
    let span = id.span();
    let id = id.to_string();
    let last = match &*id {
        "bool" => "Bool",
        "str" => "Str",
        "char" => "Char",
        x => {
            return Err(Error {
                text: format!("Expected a type, got {}", x),
                span,
            })
        }
    };

    let mut token_stream = TokenStream::new();
    write_crate_path(
        &mut token_stream,
        span,
        dollar_crate,
        ["sema", "ty", "Type", last],
    );
    Ok(token_stream)
}

pub fn do_never<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let span = do_punct(tokens, "!")?;
    let mut token_stream = TokenStream::new();
    write_crate_path(
        &mut token_stream,
        span,
        dollar_crate,
        ["sema", "ty", "Type", "Never"],
    );
    Ok(token_stream)
}
