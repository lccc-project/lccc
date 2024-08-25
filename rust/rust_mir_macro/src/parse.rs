use proc_macro::{
    token_stream, Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
};

use xlang_frontend::{
    iter::{with_rewinder_accept_on_continue, IntoRewinder, PeekMoreIterator, Peekmore},
    parse::{do_alternation, take_left, take_right},
};

use crate::{tt::*, write_crate_path, write_global_path};

#[derive(Debug)]
pub struct Error {
    pub text: String,
    pub span: Span,
}

pub type Result<T> = core::result::Result<T, Error>;

pub fn do_eof<I: Iterator<Item = TokenTree>>(tokens: &mut PeekMoreIterator<I>) -> Result<()> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        if let Some(tok) = tree.peek_next() {
            Err(Error {
                text: format!("Expected EOF, got `{}`", tok),
                span: tok.span(),
            })
        } else {
            Ok(())
        }
    })
}

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
            if tok == punct {
                return Ok(span.unwrap());
            }
            if c.spacing() != Spacing::Joint {
                break;
            }
        }

        return Err(Error {
            text: format!("Expected {}, got `{}`", punct, tok),
            span: span.unwrap_or_else(Span::call_site),
        });
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

pub fn do_unsuffixed_u32_literal<I: Iterator<Item = TokenTree>>(
    tree: &mut PeekMoreIterator<I>,
) -> Result<Literal> {
    with_rewinder_accept_on_continue(tree, |tree| match tree.peek_next() {
        Some(TokenTree::Literal(lit)) => {
            let span = lit.span();
            let lit = lit.to_string();

            let val = lit.parse::<u32>().map_err(|_| Error {
                text: format!("Expected an integer literal, got {}", lit),
                span,
            })?;

            let mut lit = Literal::u32_suffixed(val);
            lit.set_span(span);
            Ok(lit)
        }
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

pub fn do_keyword<I: Iterator<Item = TokenTree>>(
    tree: &mut PeekMoreIterator<I>,
    kw: &str,
) -> Result<Span> {
    with_rewinder_accept_on_continue(tree, |tree| match tree.peek_next() {
        Some(TokenTree::Ident(id)) if id.to_string() == kw => Ok(id.span()),
        Some(tt) => Err(Error {
            text: format!("Expected `{}`, got {}", kw, tt),
            span: tt.span(),
        }),
        None => Err(Error {
            text: format!("Expected an `{}`, but got EOF", kw),
            span: Span::call_site(),
        }),
    })
}

pub fn do_keywords<'a, I: Iterator<Item = TokenTree>, K: IntoIterator<Item = &'a str>>(
    tree: &mut PeekMoreIterator<I>,
    kws: K,
) -> Result<(&'a str, Span)> {
    let mut err = None;
    for kw in kws {
        match do_keyword(tree, kw) {
            Ok(span) => return Ok((kw, span)),
            Err(e) => err = Some(e),
        }
    }

    Err(err.expect("Required at least one keyword in `do_keywords`"))
}

pub fn do_group<I: Iterator<Item = TokenTree>>(
    tree: &mut PeekMoreIterator<I>,
    gtype: Option<Delimiter>,
) -> Result<Group> {
    with_rewinder_accept_on_continue(tree, |tree| match tree.peek_next() {
        Some(TokenTree::Group(g)) if gtype.is_none() || Some(g.delimiter()) == gtype => {
            Ok(g.clone())
        }
        Some(tt) => Err(Error {
            text: format!("Expected a group, got {}", tt),
            span: tt.span(),
        }),
        None => Err(Error {
            text: format!("Expected an a group, but got EOF"),
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
            do_type_interpolation,
            do_type_fnptr,
            do_type_pointer,
            do_type_var,
        ],
        take_right,
        dollar_crate,
    )
}

pub fn do_interpolation<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
) -> Result<TokenTree> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        do_punct(tree, "<")?;

        let tt = match do_group(tree, Some(Delimiter::Brace)) {
            Ok(g) => TokenTree::Group(g),
            Err(_) => TokenTree::Ident(do_ident(tree)?),
        };

        do_punct(tree, ">")?;
        Ok(tt)
    })
}

fn write_option(inner: Option<TokenStream>, span: Span) -> TokenStream {
    let mut ts = TokenStream::new();
    match inner {
        Some(inner) => {
            write_global_path(&mut ts, span, ["core", "option", "Option", "Some"]);
            ts.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, inner))]);
        }
        None => write_global_path(&mut ts, span, ["core", "option", "Option", "None"]),
    }
    ts
}

fn write_boxed(inner: TokenStream, span: Span) -> TokenStream {
    let mut output = TokenStream::new();
    let g = Group::new(Delimiter::Parenthesis, inner);
    write_global_path(
        &mut output,
        Span::call_site(),
        ["std", "boxed", "Box", "new"],
    );
    output.extend([TokenTree::Group(g)]);
    output
}

fn write_spanned(
    inner: TokenStream,
    span: Span,
    is_boxed: bool,
    dollar_crate: &TokenStream,
) -> TokenStream {
    let mut output = TokenStream::new();
    write_crate_path(&mut output, span, dollar_crate, ["span", "synthetic"]);

    output.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, inner))]);

    if is_boxed {
        let g = Group::new(Delimiter::Parenthesis, core::mem::take(&mut output));
        write_global_path(
            &mut output,
            Span::call_site(),
            ["std", "boxed", "Box", "new"],
        );
        output.extend([TokenTree::Group(g)])
    }

    output
}

fn write_interpolation_for_type<'a, I: IntoIterator<Item = &'a str>>(
    ty_crate_path: I,
    dollar_crate: &TokenStream,
    interp: TokenTree,
) -> TokenStream {
    let mut inner_stream = TokenStream::new();

    let idspan = dollar_crate.clone().into_iter().next().unwrap().span();

    let letkw = TokenTree::Ident(Ident::new("let", idspan));

    let id = TokenTree::Ident(Ident::new("__interp", idspan));
    let id2 = id.clone();

    let colon = TokenTree::Punct(Punct::new(':', Spacing::Alone));

    inner_stream.extend([letkw, id, colon]);

    write_crate_path(&mut inner_stream, idspan, dollar_crate, ty_crate_path);

    let eq = TokenTree::Punct(Punct::new('=', Spacing::Alone));

    let semi = TokenTree::Punct(Punct::new(';', Spacing::Alone));

    inner_stream.extend([eq, interp, semi, id2]);

    let mut stream = TokenStream::new();

    let group = Group::new(proc_macro::Delimiter::Brace, inner_stream);

    stream.extend([TokenTree::Group(group)]);

    let group = Group::new(
        proc_macro::Delimiter::Parenthesis,
        core::mem::take(&mut stream),
    );

    stream.extend([TokenTree::Group(group)]);

    stream
}

fn write_constructor<'a, I: IntoIterator<Item = (&'a str, TokenStream)>>(
    output: &mut TokenStream,
    span: Span,
    fields: I,
) {
    let mut stream = TokenStream::new();

    let colon = TokenTree::Punct(Punct::new(':', Spacing::Alone));
    let comma = TokenTree::Punct(Punct::new(',', Spacing::Alone));

    for (name, init) in fields {
        stream.extend([TokenTree::Ident(Ident::new(name, span)), colon.clone()]);
        stream.extend(init);
        stream.extend([comma.clone()]);
    }

    output.extend([TokenTree::Group(Group::new(Delimiter::Brace, stream))]);
}

fn write_intern(x: &str, span: Span, dollar_crate: &TokenStream) -> TokenStream {
    let mut ts = TokenStream::new();

    write_crate_path(
        &mut ts,
        span,
        dollar_crate,
        ["interning", "Symbol", "intern"],
    );

    let lit = Literal::string(x);
    let mut inner = TokenStream::new();
    inner.extend([TokenTree::Literal(lit)]);

    ts.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, inner))]);
    ts
}

pub fn do_defid<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        do_punct(tree, "#")?;
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
    with_rewinder_accept_on_continue(tokens, |tokens| {
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
    })
}

pub fn do_float_type<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tokens| {
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
    })
}

pub fn do_primitive_type<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tokens| {
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
    })
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

pub fn do_type_fnptr<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let inner = do_fnty_no_interp(tokens, dollar_crate)?;

    let mut token_stream = TokenStream::new();
    write_crate_path(
        &mut token_stream,
        Span::call_site(),
        dollar_crate,
        ["sema", "ty", "Type", "FnPtr"],
    );

    let inner = write_boxed(inner, Span::call_site());

    token_stream.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, inner))]);

    Ok(token_stream)
}

pub fn do_fnty<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    do_alternation(
        tokens,
        [do_fnty_no_interp, do_fnty_interpolation],
        take_right,
        dollar_crate,
    )
}

pub fn do_fnty_interpolation<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let interp = do_interpolation(tokens)?;
    Ok(write_interpolation_for_type(
        ["sema", "ty", "FnType"],
        dollar_crate,
        interp,
    ))
}

pub fn do_fnty_no_interp<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| -> Result<_> {
        let mut token_stream = TokenStream::new();
        let asyncness = {
            let mut ts = TokenStream::new();
            write_crate_path(
                &mut ts,
                Span::call_site(),
                dollar_crate,
                ["sema", "ty", "AsyncType", "Normal"],
            );

            write_spanned(ts, Span::call_site(), false, dollar_crate)
        };
        let constness = {
            let mut ts = TokenStream::new();
            match do_keyword(tree, "const") {
                Ok(span) => write_crate_path(
                    &mut ts,
                    span,
                    dollar_crate,
                    ["sema", "ty", "Mutability", "Const"],
                ),
                Err(_) => write_crate_path(
                    &mut ts,
                    Span::call_site(),
                    dollar_crate,
                    ["sema", "ty", "Mutability", "Const"],
                ),
            }
            write_spanned(ts, Span::call_site(), false, dollar_crate)
        };

        let safety = {
            let mut ts = TokenStream::new();
            match do_keyword(tree, "unsafe") {
                Ok(span) => write_crate_path(
                    &mut ts,
                    span,
                    dollar_crate,
                    ["sema", "ty", "Safety", "Unsafe"],
                ),
                Err(e) => write_crate_path(
                    &mut ts,
                    Span::call_site(),
                    dollar_crate,
                    ["sema", "ty", "Safety", "Safe"],
                ),
            }
            write_spanned(ts, Span::call_site(), false, dollar_crate)
        };

        let tag = {
            let mut ts = TokenStream::new();

            match do_keyword(tree, "extern") {
                Ok(span) => match do_literal(tree) {
                    Ok(lit) => {
                        write_crate_path(
                            &mut ts,
                            span,
                            dollar_crate,
                            ["sema", "ty", "__parse_tag"],
                        );

                        let kw_extern = Ident::new("extern", span);
                        let mut inner_stream = TokenStream::new();
                        inner_stream.extend([TokenTree::Ident(kw_extern), TokenTree::Literal(lit)]);

                        ts.extend([
                            TokenTree::Punct(Punct::new('!', Spacing::Alone)),
                            TokenTree::Group(Group::new(Delimiter::Parenthesis, inner_stream)),
                        ])
                    }
                    Err(e) => match do_interpolation(tree) {
                        Ok(interp) => {
                            ts = write_interpolation_for_type(
                                ["sema", "ty", "AbiTag"],
                                dollar_crate,
                                interp,
                            )
                        }
                        Err(_) => Err(e)?,
                    },
                },
                Err(_) => write_crate_path(
                    &mut ts,
                    Span::call_site(),
                    dollar_crate,
                    ["sema", "ty", "AbiTag", "Rust"],
                ),
            }

            write_spanned(ts, Span::call_site(), false, dollar_crate)
        };

        let span = do_keyword(tree, "fn")?;

        let inner = do_group(tree, Some(Delimiter::Parenthesis))?;

        let mut inner_tree = inner.stream().into_iter().peekmore();

        let mut params = TokenStream::new();

        let mut iscvarargs = Ident::new("false", Span::call_site()); // string because it needs to become an Ident

        loop {
            if do_eof(&mut inner_tree).is_ok() {
                break;
            }

            if let Ok(span) = do_punct(&mut inner_tree, "...") {
                iscvarargs = Ident::new("true", span);
                do_eof(&mut inner_tree)?;
                break;
            }

            params.extend(write_spanned(
                do_type(&mut inner_tree, dollar_crate)?,
                span,
                false,
                dollar_crate,
            ));
            match do_punct(&mut inner_tree, ",") {
                Ok(_) => params.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]),
                Err(e) => {
                    do_eof(&mut inner_tree)?;
                    break;
                }
            }
        }
        let iscvarargs_span = iscvarargs.span();
        let iscvarargs = core::iter::once(TokenTree::Ident(iscvarargs)).collect::<TokenStream>();
        let iscvarargs = write_spanned(iscvarargs, iscvarargs_span, false, dollar_crate);

        let paramtys = {
            let mut token_stream = TokenStream::new();
            write_global_path(&mut token_stream, span, ["std", "vec"]);
            let params = TokenTree::Group(Group::new(Delimiter::Bracket, params));

            token_stream.extend([TokenTree::Punct(Punct::new('!', Spacing::Alone)), params]);
            token_stream
        };

        do_punct(tree, "->")?;

        let retty = do_type(tree, dollar_crate)?;

        let retty = write_spanned(retty, span, true, dollar_crate);

        write_crate_path(
            &mut token_stream,
            span,
            dollar_crate,
            ["sema", "ty", "FnType"],
        );

        let mut inner_stream = TokenStream::new();

        let colon = TokenTree::Punct(Punct::new(':', Spacing::Alone));
        let comma = TokenTree::Punct(Punct::new(',', Spacing::Alone));

        inner_stream.extend([TokenTree::Ident(Ident::new("safety", span)), colon.clone()]);
        inner_stream.extend(safety);
        inner_stream.extend([
            comma.clone(),
            TokenTree::Ident(Ident::new("constness", span)),
            colon.clone(),
        ]);
        inner_stream.extend(constness);
        inner_stream.extend([
            comma.clone(),
            TokenTree::Ident(Ident::new("asyncness", span)),
            colon.clone(),
        ]);
        inner_stream.extend(asyncness);
        inner_stream.extend([
            comma.clone(),
            TokenTree::Ident(Ident::new("tag", span)),
            colon.clone(),
        ]);
        inner_stream.extend(tag);
        inner_stream.extend([
            comma.clone(),
            TokenTree::Ident(Ident::new("retty", span)),
            colon.clone(),
        ]);
        inner_stream.extend(retty);
        inner_stream.extend([
            comma.clone(),
            TokenTree::Ident(Ident::new("paramtys", span)),
            colon.clone(),
        ]);
        inner_stream.extend(paramtys);
        inner_stream.extend([
            comma,
            TokenTree::Ident(Ident::new("iscvarargs", span)),
            colon,
        ]);
        inner_stream.extend(iscvarargs);

        token_stream.extend([TokenTree::Group(Group::new(Delimiter::Brace, inner_stream))]);

        Ok(token_stream)
    })
}

pub fn do_type_pointer<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let base_span = do_punct(tree, "*")?;

        let (mt, span) = match do_keywords(tree, ["mut", "const"])? {
            ("mut", span) => {
                let mut ts = TokenStream::new();
                write_crate_path(
                    &mut ts,
                    span,
                    dollar_crate,
                    ["sema", "ty", "Mutability", "Mut"],
                );
                (ts, span)
            }
            ("const", span) => {
                let mut ts = TokenStream::new();
                write_crate_path(
                    &mut ts,
                    span,
                    dollar_crate,
                    ["sema", "ty", "Mutability", "Mut"],
                );
                (ts, span)
            }
            _ => unreachable!(),
        };

        let inner_ty = do_type(tree, dollar_crate)?;

        let mut stream = TokenStream::new();
        write_crate_path(
            &mut stream,
            base_span,
            dollar_crate,
            ["sema", "ty", "Type", "Pointer"],
        );

        let mut inner_stream = write_spanned(mt, span, false, dollar_crate);
        inner_stream.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
        inner_stream.extend(write_spanned(inner_ty, base_span, true, dollar_crate));

        stream.extend([TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            inner_stream,
        ))]);
        Ok(stream)
    })
}

pub fn do_type_var<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let base_span = do_punct(tree, "%")?;

        let lit = do_unsuffixed_u32_literal(tree)?;

        let mut inner_stream = TokenStream::new();
        inner_stream.extend([TokenTree::Literal(lit)]);

        let mut stream = TokenStream::new();
        write_crate_path(
            &mut stream,
            base_span,
            dollar_crate,
            ["sema", "ty", "Type", "Param"],
        );

        let mut param_stream = TokenStream::new();
        write_crate_path(
            &mut param_stream,
            base_span,
            dollar_crate,
            ["sema", "generics", "ParamId", "__new_unchecked"],
        );

        param_stream.extend([TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            inner_stream,
        ))]);

        stream.extend([TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            param_stream,
        ))]);

        Ok(stream)
    })
}

pub fn do_expr<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    do_alternation(
        tokens,
        [
            do_expr_interpolation,
            do_expr_unreachable,
            do_expr_uninit,
            do_expr_const_or_constructor,
            do_expr_const_lit,
            do_expr_ssa_var,
            do_expr_read,
            do_expr_tuple,
            do_expr_get_symbol,
            do_expr_get_subobject,
            do_expr_project_field,
        ],
        take_right,
        dollar_crate,
    )
}

pub fn do_field_name<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    do_alternation(
        tokens,
        [
            do_field_name_interpolation,
            do_field_name_special,
            do_field_name_nested,
            do_field_name_token,
        ],
        take_right,
        dollar_crate,
    )
}

pub fn do_field_name_interpolation<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let interp = do_interpolation(tokens)?;

    Ok(write_interpolation_for_type(
        ["sema", "ty", "FieldName"],
        dollar_crate,
        interp,
    ))
}

pub fn do_field_name_special<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let group = do_group(tokens, Some(Delimiter::Brace))?;
    let mut inner_tree = group.stream().into_iter().peekmore();
    let inner_group = do_group(&mut inner_tree, Some(Delimiter::Brace))?;
    let mut inner_tree = group.stream().into_iter().peekmore();

    let mut res = TokenStream::new();

    match do_keywords(&mut inner_tree, ["discriminant", "data", "metadata"])? {
        ("discriminant", span) => {
            write_crate_path(
                &mut res,
                span,
                dollar_crate,
                ["sema", "ty", "FieldName", "EnumDiscriminant"],
            );
            Ok(res)
        }
        ("data", span) => {
            write_crate_path(
                &mut res,
                span,
                dollar_crate,
                ["sema", "ty", "FieldName", "FatPtrPart"],
            );
            let mut inner = TokenStream::new();
            write_crate_path(
                &mut res,
                span,
                dollar_crate,
                ["sema", "ty", "FatPtrPart", "Payload"],
            );
            res.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, inner))]);
            Ok(res)
        }
        ("metadata", span) => {
            write_crate_path(
                &mut res,
                span,
                dollar_crate,
                ["sema", "ty", "FieldName", "FatPtrPart"],
            );
            let mut inner = TokenStream::new();
            write_crate_path(
                &mut res,
                span,
                dollar_crate,
                ["sema", "ty", "FatPtrPart", "Metadata"],
            );
            res.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, inner))]);
            Ok(res)
        }
        _ => unreachable!(),
    }
}

pub fn do_field_name_nested<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let defid = do_defid(tree, dollar_crate)?;

        let punct_span = do_punct(tree, "::")?;

        let field_name = do_field_name_token_as_symbol(tree, dollar_crate)?.0;

        let mut res = TokenStream::new();
        write_crate_path(
            &mut res,
            punct_span,
            dollar_crate,
            ["sema", "ty", "FieldName", "VariantSubfield"],
        );
        let mut inner = TokenStream::new();
        inner.extend(defid);
        inner.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
        inner.extend(field_name);

        res.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, inner))]);
        Ok(res)
    })
}

pub fn do_field_name_token<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let (sym, span) = do_field_name_token_as_symbol(tokens, dollar_crate)?;
    let mut res = TokenStream::new();
    write_crate_path(
        &mut res,
        span,
        dollar_crate,
        ["sema", "ty", "FieldName", "Field"],
    );

    res.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, sym))]);

    Ok(res)
}

pub fn do_field_name_token_as_symbol<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<(TokenStream, Span)> {
    match do_ident(tokens) {
        Ok(ident) => {
            let st = ident.to_string();

            let st = st.strip_prefix("r#").unwrap_or(&st);

            Ok((write_intern(st, ident.span(), dollar_crate), ident.span()))
        }
        Err(_) => match do_literal(tokens) {
            Ok(lit) => {
                let st = lit.to_string();

                if st.contains(|c: char| c.is_ascii_digit()) {
                    Err(Error {
                        text: format!("Expected an integer literal, got {st}"),
                        span: lit.span(),
                    })
                } else {
                    Ok((write_intern(&st, lit.span(), dollar_crate), lit.span()))
                }
            }
            Err(e) => Err(e),
        },
    }
}

pub fn do_expr_project_field<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let project_span = do_punct(tree, "&")?;

        let group = do_group(tree, Some(Delimiter::Parenthesis))?;

        let mut inner_tree = group.stream().into_iter().peekmore();

        do_punct(tree, "*")?;

        let expr = do_expr(&mut inner_tree, dollar_crate)?;

        let span = do_punct(tree, ".")?;

        let field = do_field_name(tree, dollar_crate)?;

        let boxed_expr = write_spanned(expr, span, true, dollar_crate);

        let mut res = TokenStream::new();
        write_crate_path(
            &mut res,
            project_span,
            dollar_crate,
            ["sema", "mir", "MirExpr", "FieldProject"],
        );

        let mut token = TokenStream::new();
        token.extend(boxed_expr);
        token.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
        token.extend(field);

        res.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, token))]);

        Ok(res)
    })
}

pub fn do_expr_get_subobject<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let expr = do_expr(tree, dollar_crate)?;

        let span = do_punct(tree, ".")?;

        let field = do_field_name(tree, dollar_crate)?;

        let boxed_expr = write_spanned(expr, span, true, dollar_crate);

        let mut res = TokenStream::new();
        write_crate_path(
            &mut res,
            span,
            dollar_crate,
            ["sema", "mir", "MirExpr", "GetSubobject"],
        );

        let mut token = TokenStream::new();
        token.extend(boxed_expr);
        token.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
        token.extend(field);

        res.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, token))]);

        Ok(res)
    })
}

pub fn do_expr_interpolation<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let interp = do_interpolation(tokens)?;

    Ok(write_interpolation_for_type(
        ["sema", "mir", "MirExpr"],
        dollar_crate,
        interp,
    ))
}

pub fn do_expr_unreachable<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let idspan = do_keyword(tokens, "unreachable")?;
    let mut token_stream = TokenStream::new();
    write_crate_path(
        &mut token_stream,
        idspan,
        dollar_crate,
        ["sema", "mir", "MirExpr", "Unreachable"],
    );
    Ok(token_stream)
}

pub fn do_expr_uninit<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let idspan = do_keyword(tokens, "uninit")?;
    let ty = do_type(tokens, dollar_crate)?;
    let mut token_stream = TokenStream::new();
    write_crate_path(
        &mut token_stream,
        idspan,
        dollar_crate,
        ["sema", "mir", "MirExpr", "Uninit"],
    );
    token_stream.extend([TokenTree::Group(Group::new(
        proc_macro::Delimiter::Parenthesis,
        ty,
    ))]);
    Ok(token_stream)
}

pub fn do_expr_const_or_constructor<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let defid = do_defid(tree, dollar_crate)?;
        let mut token_stream = TokenStream::new();

        if let Ok(g) = do_group(tree, Some(Delimiter::Brace)) {
            let mut inner_tree = g.stream().into_iter().peekmore();
            todo!("constructor")
        } else {
            let mut inner_stream = defid;
            inner_stream.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);

            write_crate_path(
                &mut inner_stream,
                Span::call_site(),
                dollar_crate,
                ["sema", "generics", "GenericArgs", "default"],
            );

            inner_stream.extend([TokenTree::Group(Group::new(
                proc_macro::Delimiter::Parenthesis,
                TokenStream::new(),
            ))]);

            write_crate_path(
                &mut token_stream,
                Span::call_site(),
                dollar_crate,
                ["sema", "mir", "MirExpr", "Const"],
            );
            token_stream.extend([TokenTree::Group(Group::new(
                proc_macro::Delimiter::Parenthesis,
                inner_stream,
            ))]);
        }

        Ok(token_stream)
    })
}

pub fn do_expr_const_lit<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let lit = do_literal(tree)?;

        let span = lit.span();
        let lit = lit.to_string();

        if lit.starts_with(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) {
            let pos = lit.rfind(['u', 'i']).ok_or_else(|| Error {
                text: format!("Expected an integer literal (with a suffix), got `{}`", lit),
                span,
            })?;

            let (lit, suffix) = lit.split_at(pos);
            let lit = lit.trim_end_matches('_');

            let lit_val = lit.parse::<u128>().unwrap();
            let lit = Literal::u128_suffixed(lit_val);

            let mut inner_stream = TokenStream::new();

            write_crate_path(
                &mut inner_stream,
                span,
                dollar_crate,
                ["sema", "ty", "IntType", suffix],
            );
            inner_stream.extend([
                TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                TokenTree::Literal(lit),
            ]);

            let mut stream = TokenStream::new();

            write_crate_path(
                &mut stream,
                span,
                dollar_crate,
                ["sema", "mir", "MirExpr", "ConstInt"],
            );

            stream.extend([TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                inner_stream,
            ))]);

            Ok(stream)
        } else {
            todo!("String/Char Literal")
        }
    })
}

pub fn do_expr_ssa_var<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let var = do_ssa_var(tokens, dollar_crate)?;
    let mut expr = TokenStream::new();

    write_crate_path(
        &mut expr,
        Span::call_site(),
        dollar_crate,
        ["sema", "mir", "MirExpr", "Var"],
    );

    expr.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, var))]);

    Ok(expr)
}

pub fn do_ssa_var<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let id = do_ident(tree)?;
        let span = id.span();
        let id = id.to_string();
        if let Some(rest) = id.strip_prefix('_') {
            let val = rest.parse::<u32>().map_err(|_| Error {
                text: format!("Expected a Ssa var, got `{}`", id),
                span,
            })?;

            let tok = Literal::u32_suffixed(val);

            let inner_tree = core::iter::once(TokenTree::Literal(tok)).collect::<TokenStream>();

            let mut ssa_var = TokenStream::new();
            write_crate_path(
                &mut ssa_var,
                span,
                dollar_crate,
                ["sema", "mir", "SsaVarId", "__new_unchecked"],
            );
            ssa_var.extend([TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                inner_tree,
            ))]);

            Ok(ssa_var)
        } else {
            Err(Error {
                text: format!("Expected a Ssa var, got `{}`", id),
                span,
            })
        }
    })
}

pub fn do_expr_read<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let base_span = do_keyword(tree, "read")?;
        let group = do_group(tree, Some(Delimiter::Parenthesis))?;

        let mut inner_tree = group.stream().into_iter().peekmore();

        let inner_base_span = do_punct(&mut inner_tree, "*")?;

        let inner_expr = do_expr(&mut inner_tree, dollar_crate)?;

        let mut ts = TokenStream::new();
        write_crate_path(
            &mut ts,
            base_span,
            dollar_crate,
            ["sema", "mir", "MirExpr", "Read"],
        );
        let inner_stream = write_spanned(inner_expr, inner_base_span, true, dollar_crate);
        ts.extend([TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            inner_stream,
        ))]);
        Ok(ts)
    })
}

pub fn do_expr_tuple<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let group = do_group(tree, Some(Delimiter::Parenthesis))?;
        let mut inner_tree = group.stream().into_iter().peekmore();
        let base_span = group.span();

        let mut inner_stream = TokenStream::new();

        loop {
            match do_eof(&mut inner_tree) {
                Ok(()) => break,
                Err(_) => {
                    let expr = do_expr(&mut inner_tree, dollar_crate)?;
                    inner_stream.extend(write_spanned(expr, base_span, false, dollar_crate));
                }
            }
            match do_punct(&mut inner_tree, ",") {
                Ok(_) => inner_stream.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]),
                Err(_) => {
                    do_eof(&mut inner_tree)?;
                    break;
                }
            }
        }

        let mut exprs_stream = TokenStream::new();
        write_global_path(&mut exprs_stream, base_span, ["std", "vec"]);
        exprs_stream.extend([TokenTree::Group(Group::new(
            Delimiter::Bracket,
            inner_stream,
        ))]);
        let mut ts = TokenStream::new();
        write_crate_path(
            &mut ts,
            base_span,
            dollar_crate,
            ["sema", "mir", "MirExpr", "Tuple"],
        );
        ts.extend([TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            exprs_stream,
        ))]);

        Ok(ts)
    })
}

pub fn do_expr_get_symbol<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let base_span = do_keyword(tree, "get_symbol")?;

        let group = do_group(tree, Some(Delimiter::Parenthesis))?;

        let mut inner_tree = group.stream().into_iter().peekmore();

        let defid = do_defid(&mut inner_tree, dollar_crate)?;

        let mut ts = TokenStream::new();
        write_crate_path(
            &mut ts,
            base_span,
            dollar_crate,
            ["sema", "mir", "MirExpr", "GetSymbol"],
        );
        ts.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, defid))]);
        Ok(ts)
    })
}

pub fn do_terminator<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    do_alternation(
        tokens,
        [
            do_term_jump,
            do_term_return,
            do_term_unreachable,
            do_term_call,
            do_term_tailcall,
            do_term_drop,
        ],
        take_right,
        dollar_crate,
    )
}

pub fn do_statement<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    do_alternation(
        tokens,
        [do_stmt_store_dead, do_stmt_write, do_stmt_let],
        take_right,
        dollar_crate,
    )
}

pub fn do_stmt_store_dead<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let span = do_keyword(tree, "store")?;
        do_keyword(tree, "dead")?;

        let var = do_ssa_var(tree, dollar_crate)?;

        let mut output = TokenStream::new();

        write_crate_path(
            &mut output,
            span,
            dollar_crate,
            ["sema", "mir", "MirStatement", "StoreDead"],
        );

        output.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, var))]);

        Ok(output)
    })
}

pub fn do_stmt_write<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let base_span = do_keyword(tree, "write")?;

        let group = do_group(tree, Some(Delimiter::Parenthesis))?;

        let inner_span = do_punct(tree, "*")?;

        let ptr_expr = do_expr(tree, dollar_crate)?;

        do_punct(tree, ",")?;

        let val_expr = do_expr(tree, dollar_crate)?;

        let mut ts = TokenStream::new();
        write_crate_path(
            &mut ts,
            base_span,
            dollar_crate,
            ["sema", "mir", "MirStatement", "Write"],
        );

        let mut inner_stream = write_spanned(ptr_expr, inner_span, false, dollar_crate);
        inner_stream.extend(
            [TokenTree::Punct(Punct::new(',', Spacing::Joint))]
                .into_iter()
                .chain(write_spanned(val_expr, inner_span, false, dollar_crate)),
        );

        ts.extend([TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            inner_stream,
        ))]);
        Ok(ts)
    })
}

pub fn do_stmt_let<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let span = do_keyword(tokens, "let")?;

    let var = do_ssa_var(tokens, dollar_crate)?;

    let colon_span = do_punct(tokens, ":")?;

    let ty = do_type(tokens, dollar_crate)?;

    let eq_span = do_punct(tokens, "=")?;

    let init = do_expr(tokens, dollar_crate)?;

    let mut res = TokenStream::new();

    write_crate_path(
        &mut res,
        span,
        dollar_crate,
        ["sema", "mir", "MirStatement", "Declare"],
    );
    let mut inner = TokenStream::new();

    inner.extend([
        TokenTree::Ident(Ident::new("var", span)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
    ]);
    inner.extend(write_spanned(var, span, false, dollar_crate));
    inner.extend([
        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
        TokenTree::Ident(Ident::new("ty", colon_span)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
    ]);
    inner.extend(write_spanned(ty, colon_span, false, dollar_crate));
    inner.extend([
        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
        TokenTree::Ident(Ident::new("init", eq_span)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
    ]);
    inner.extend(write_spanned(init, eq_span, false, dollar_crate));

    res.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, inner))]);

    Ok(res)
}

pub fn do_basic_block_id<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    do_alternation(
        tokens,
        [do_basic_block_id_interp, do_basic_block_id_no_interp],
        take_right,
        dollar_crate,
    )
}

pub fn do_basic_block_id_interp<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        do_punct(tree, "@")?;

        let interp = do_interpolation(tree)?;

        Ok(write_interpolation_for_type(
            ["sema", "mir", "BasicBlockId"],
            dollar_crate,
            interp,
        ))
    })
}

pub fn do_basic_block_id_no_interp<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let span = do_punct(tree, "@")?;
        let target = do_unsuffixed_u32_literal(tree)?;

        let mut ret = TokenStream::new();

        write_crate_path(
            &mut ret,
            span,
            dollar_crate,
            ["sema", "mir", "BasicBlockId", "__new_unchecked"],
        );

        ret.extend([TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            core::iter::once(TokenTree::Literal(target)).collect(),
        ))]);

        Ok(ret)
    })
}

pub fn do_term_jump<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let span = do_keyword(tree, "jump")?;
        let targ = do_jump_target(tree, dollar_crate)?;
        let mut output = TokenStream::new();

        write_crate_path(
            &mut output,
            span,
            dollar_crate,
            ["sema", "mir", "MirTerminator", "Jump"],
        );

        output.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, targ))]);

        Ok(output)
    })
}

pub fn do_term_unreachable<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    let span = do_keyword(tokens, "unreachable")?;
    let mut output = TokenStream::new();
    write_crate_path(
        &mut output,
        span,
        dollar_crate,
        ["sema", "mir", "MirTerminator", "Unreachable"],
    );
    Ok(output)
}

pub fn do_term_return<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let span = do_keyword(tree, "return")?;
        let expr = do_expr(tree, dollar_crate)?;

        let mut output = TokenStream::new();

        write_crate_path(
            &mut output,
            span,
            dollar_crate,
            ["sema", "mir", "MirTerminator", "Return"],
        );

        output.extend([TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            write_spanned(expr, span, false, dollar_crate),
        ))]);

        Ok(output)
    })
}

pub fn do_term_tailcall<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let span = do_keyword(tree, "tailcall")?;

        let targ = write_spanned(do_expr(tree, dollar_crate)?, span, false, dollar_crate);

        do_punct(tree, ":")?;

        let fnty = do_fnty(tree, dollar_crate)?;

        let fnty = write_boxed(fnty, span);

        let mut params = TokenStream::new();

        let mut inner_tree = do_group(tree, Some(Delimiter::Parenthesis))?
            .stream()
            .into_iter()
            .peekmore();
        loop {
            if do_eof(&mut inner_tree).is_ok() {
                break;
            }

            let param = do_expr(&mut inner_tree, dollar_crate)?;

            params.extend(write_spanned(param, span, false, dollar_crate));
            params.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
            match do_punct(&mut inner_tree, ",") {
                Ok(_) => continue,
                Err(e) => match do_eof(&mut inner_tree) {
                    Ok(_) => break,
                    Err(_) => return Err(e),
                },
            }
        }

        let params = {
            let mut ts = TokenStream::new();
            write_global_path(&mut ts, Span::call_site(), ["std", "vec"]);
            ts.extend([
                TokenTree::Punct(Punct::new('!', Spacing::Alone)),
                TokenTree::Group(Group::new(Delimiter::Bracket, params)),
            ]);
            ts
        };

        let unwind = match do_keyword(tree, "unwind") {
            Ok(_) => Some(do_jump_target(tree, dollar_crate)?),
            Err(_) => None,
        };

        let mut call = TokenStream::new();
        write_crate_path(
            &mut call,
            Span::call_site(),
            dollar_crate,
            ["sema", "mir", "MirTailcallInfo"],
        );

        write_constructor(
            &mut call,
            Span::call_site(),
            [
                ("targ", targ),
                ("fnty", fnty),
                ("params", params),
                ("unwind", write_option(unwind, Span::call_site())),
            ],
        );

        let mut outer = TokenStream::new();
        write_crate_path(
            &mut outer,
            Span::call_site(),
            dollar_crate,
            ["sema", "mir", "MirTerminator", "Tailcall"],
        );
        outer.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, call))]);
        Ok(outer)
    })
}

pub fn do_term_call<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let span = do_keyword(tree, "call")?;

        let retplace = write_spanned(do_ssa_var(tree, dollar_crate)?, span, false, dollar_crate);
        do_punct(tree, "=")?;

        let targ = write_spanned(do_expr(tree, dollar_crate)?, span, false, dollar_crate);

        do_punct(tree, ":")?;

        let fnty = do_fnty(tree, dollar_crate)?;

        let fnty = write_boxed(fnty, span);

        let mut params = TokenStream::new();

        let mut inner_tree = do_group(tree, Some(Delimiter::Parenthesis))?
            .stream()
            .into_iter()
            .peekmore();
        loop {
            if do_eof(&mut inner_tree).is_ok() {
                break;
            }

            let param = write_spanned(
                do_expr(&mut inner_tree, dollar_crate)?,
                span,
                false,
                dollar_crate,
            );

            params.extend(param);
            params.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
            match do_punct(&mut inner_tree, ",") {
                Ok(_) => continue,
                Err(e) => match do_eof(&mut inner_tree) {
                    Ok(_) => break,
                    Err(_) => return Err(e),
                },
            }
        }

        let params = {
            let mut ts = TokenStream::new();
            write_global_path(&mut ts, Span::call_site(), ["std", "vec"]);
            ts.extend([
                TokenTree::Punct(Punct::new('!', Spacing::Alone)),
                TokenTree::Group(Group::new(Delimiter::Bracket, params)),
            ]);
            ts
        };

        do_keyword(tree, "next")?;
        let next = do_jump_target(tree, dollar_crate)?;

        let unwind = match do_keyword(tree, "unwind") {
            Ok(_) => Some(do_jump_target(tree, dollar_crate)?),
            Err(_) => None,
        };

        let mut call = TokenStream::new();
        write_crate_path(
            &mut call,
            Span::call_site(),
            dollar_crate,
            ["sema", "mir", "MirCallInfo"],
        );

        write_constructor(
            &mut call,
            Span::call_site(),
            [
                ("retplace", retplace),
                ("targ", targ),
                ("fnty", fnty),
                ("params", params),
                ("next", next),
                ("unwind", write_option(unwind, Span::call_site())),
            ],
        );

        let mut outer = TokenStream::new();
        write_crate_path(
            &mut outer,
            Span::call_site(),
            dollar_crate,
            ["sema", "mir", "MirTerminator", "Call"],
        );
        outer.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, call))]);
        Ok(outer)
    })
}

pub fn do_jump_target<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let fallthrough = do_keyword(tree, "fallthrough");

        let fallthrough = match fallthrough {
            Ok(span) => TokenStream::from(TokenTree::Ident(Ident::new("true", span))),
            Err(x) => TokenStream::from(TokenTree::Ident(Ident::new("false", x.span))),
        };

        let target = do_basic_block_id(tree, dollar_crate)?;

        let inner_tree = do_group(tree, Some(Delimiter::Bracket))?;

        let mut inner_tree = inner_tree.stream().into_iter().peekmore();

        let mut remaps = TokenStream::new();

        loop {
            if do_eof(&mut inner_tree).is_ok() {
                break;
            }

            let var1 = do_ssa_var(&mut inner_tree, dollar_crate)?;

            do_punct(&mut inner_tree, "=>")?;

            let var2 = do_ssa_var(&mut inner_tree, dollar_crate)?;

            let mut inner_stream = TokenStream::new();
            inner_stream.extend(var1);
            inner_stream.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
            inner_stream.extend(var2);

            remaps.extend([TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                inner_stream,
            ))]);

            match do_punct(&mut inner_tree, ",") {
                Ok(_) => continue,
                Err(e) => match do_eof(&mut inner_tree) {
                    Ok(_) => break,
                    Err(_) => return Err(e),
                },
            }
        }

        let remaps_group = Group::new(Delimiter::Bracket, core::mem::take(&mut remaps));

        write_global_path(&mut remaps, Span::call_site(), ["std", "vec"]);

        remaps.extend([
            TokenTree::Punct(Punct::new('!', Spacing::Alone)),
            TokenTree::Group(remaps_group),
        ]);

        let mut output = TokenStream::new();

        write_crate_path(
            &mut output,
            Span::call_site(),
            dollar_crate,
            ["sema", "mir", "MirJumpInfo"],
        );

        write_constructor(
            &mut output,
            Span::call_site(),
            [
                ("targbb", target),
                ("remaps", remaps),
                ("fallthrough", fallthrough),
            ],
        );

        Ok(output)
    })
}

pub fn do_term_drop<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let span = do_keyword(tree, "drop")?;

        let target = do_expr(tree, dollar_crate)?;

        let opt_flags = match do_keyword(tree, "flags") {
            Ok(span) => Some(do_expr(tree, dollar_crate)?),
            Err(_) => None,
        };

        do_keyword(tree, "next")?;

        let next = do_jump_target(tree, dollar_crate)?;

        let unwind = match do_keyword(tree, "unwind") {
            Ok(_) => Some(do_jump_target(tree, dollar_crate)?),
            Err(_) => None,
        };

        let mut res = TokenStream::new();

        write_crate_path(
            &mut res,
            span,
            dollar_crate,
            ["sema", "mir", "MirTerminator", "DropInPlace"],
        );

        let mut inner = TokenStream::new();

        write_crate_path(
            &mut inner,
            span,
            dollar_crate,
            ["sema", "mir", "MirDropInfo"],
        );

        let mut ctor = TokenStream::new();

        ctor.extend(ident("target", span).chain(punct(":")).chain(target));

        ctor.extend(
            punct(",")
                .chain(ident("flags", span))
                .chain(punct(":"))
                .chain(write_option(opt_flags, span)),
        );

        ctor.extend(
            punct(",")
                .chain(ident("next", span))
                .chain(punct(":"))
                .chain(next),
        );

        ctor.extend(
            punct(",")
                .chain(ident("unwind", span))
                .chain(punct(":"))
                .chain(write_option(unwind, span)),
        );

        inner.extend(group(ctor, Delimiter::Brace));
        res.extend(group(inner, Delimiter::Parenthesis));

        Ok(res)
    })
}

pub fn do_basic_block<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let id = do_basic_block_id(tree, dollar_crate)?;

        let span = do_punct(tree, ":")?;

        let mut inner_tree = do_group(tree, Some(Delimiter::Brace))?
            .stream()
            .into_iter()
            .peekmore();

        let mut incoming_tree = do_group(&mut inner_tree, Some(Delimiter::Bracket))?
            .stream()
            .into_iter()
            .peekmore();

        let mut incoming = TokenStream::new();

        loop {
            if do_eof(&mut incoming_tree).is_ok() {
                break;
            }

            let param = do_ssa_var(&mut incoming_tree, dollar_crate)?;

            do_punct(&mut incoming_tree, ":")?;

            let ty = do_type(&mut incoming_tree, dollar_crate)?;

            let mut incoming_var = TokenStream::new();

            incoming_var.extend(param);
            incoming_var.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
            incoming_var.extend(ty);

            incoming.extend([TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                incoming_var,
            ))]);
            incoming.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
            match do_punct(&mut incoming_tree, ",") {
                Ok(_) => continue,
                Err(e) => match do_eof(&mut incoming_tree) {
                    Ok(_) => break,
                    Err(_) => return Err(e),
                },
            }
        }

        let mut stats = TokenStream::new();

        let term = loop {
            match do_statement(&mut inner_tree, dollar_crate) {
                Ok(stmt) => {
                    stats.extend(write_spanned(stmt, span, false, dollar_crate));
                    stats.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);

                    do_punct(&mut inner_tree, ";")?;
                    continue;
                }
                Err(_) => {
                    let term = do_terminator(&mut inner_tree, dollar_crate)?;
                    do_eof(&mut inner_tree)?;
                    break write_spanned(term, span, false, dollar_crate);
                }
            }
        };

        let incoming = {
            let mut ts = TokenStream::new();
            write_global_path(&mut ts, span, ["std", "vec"]);

            ts.extend([
                TokenTree::Punct(Punct::new('!', Spacing::Alone)),
                TokenTree::Group(Group::new(Delimiter::Bracket, incoming)),
            ]);
            ts
        };

        let stats = {
            let mut ts = TokenStream::new();
            write_global_path(&mut ts, span, ["std", "vec"]);

            ts.extend([
                TokenTree::Punct(Punct::new('!', Spacing::Alone)),
                TokenTree::Group(Group::new(Delimiter::Bracket, stats)),
            ]);
            ts
        };

        let mut output = TokenStream::new();

        write_crate_path(
            &mut output,
            span,
            dollar_crate,
            ["sema", "mir", "MirBasicBlock"],
        );
        write_constructor(
            &mut output,
            span,
            [
                ("incoming_vars", incoming),
                ("id", id),
                ("stmts", stats),
                ("term", term),
            ],
        );

        Ok(output)
    })
}

pub fn do_mir_fnbody<I: Iterator<Item = TokenTree>>(
    tokens: &mut PeekMoreIterator<I>,
    dollar_crate: &TokenStream,
) -> Result<TokenStream> {
    with_rewinder_accept_on_continue(tokens, |tree| {
        let idspan = dollar_crate.clone().into_iter().next().unwrap().span();
        let local_items = match do_keyword(tree, "items") {
            Ok(span) => todo!("items"),
            Err(_) => TokenStream::new(),
        };

        let local_items = {
            let mut ts = TokenStream::new();
            write_global_path(&mut ts, idspan, ["std", "vec"]);

            ts.extend([
                TokenTree::Punct(Punct::new('!', Spacing::Alone)),
                TokenTree::Group(Group::new(Delimiter::Bracket, local_items)),
            ]);
            ts
        };

        let vardbg_info = {
            let mut ts = TokenStream::new();
            ts.extend([
                TokenTree::Ident(Ident::new("let", idspan)),
                TokenTree::Ident(Ident::new("mut", idspan)),
                TokenTree::Ident(Ident::new("__vardbg_info", idspan)),
                TokenTree::Punct(Punct::new('=', Spacing::Alone)),
            ]);
            write_global_path(
                &mut ts,
                idspan,
                ["xlang", "abi", "collection", "HashMap", "new"],
            );

            ts.extend([
                TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
                TokenTree::Punct(Punct::new(';', Spacing::Alone)),
            ]);

            match do_keyword(tree, "debug") {
                Ok(span) => todo!("debug"),
                Err(_) => {}
            }

            ts.extend([TokenTree::Ident(Ident::new("__vardbg_info", idspan))]);

            let mut outer = TokenStream::new();
            outer.extend([TokenTree::Group(Group::new(Delimiter::Brace, ts))]);
            outer
        };

        let mut bbs = TokenStream::new();

        loop {
            if do_eof(tree).is_ok() {
                break;
            }

            bbs.extend(do_basic_block(tree, dollar_crate)?);
            bbs.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
        }

        let bbs = {
            let mut ts = TokenStream::new();
            write_global_path(&mut ts, idspan, ["std", "vec"]);

            ts.extend([
                TokenTree::Punct(Punct::new('!', Spacing::Alone)),
                TokenTree::Group(Group::new(Delimiter::Bracket, bbs)),
            ]);
            ts
        };

        let mut out = TokenStream::new();

        write_crate_path(
            &mut out,
            idspan,
            dollar_crate,
            ["sema", "mir", "MirFunctionBody"],
        );
        write_constructor(
            &mut out,
            idspan,
            [
                ("bbs", bbs),
                ("localitems", local_items),
                ("vardbg_info", vardbg_info),
            ],
        );

        Ok(out)
    })
}
