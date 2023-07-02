use core::fmt::{Arguments, Write};
use std::collections::HashMap;

use proc_macro::{Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

fn generate_compile_error(ts: &mut TokenStream, error_msg: Arguments, span: Span) {
    let colon1 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint));
    let colon2 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Alone));
    let core = TokenTree::Ident(Ident::new("core", span));
    let colon3 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint));
    let colon4 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Alone));
    let compile_error = TokenTree::Ident(Ident::new("compile_error", span));
    let bang = TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Alone));

    let mut inner = TokenStream::new();

    inner.extend([TokenTree::Literal(Literal::string(&error_msg.to_string()))]);

    let parens = TokenTree::Group(Group::new(proc_macro::Delimiter::Parenthesis, inner));

    let semi = TokenTree::Punct(Punct::new(';', proc_macro::Spacing::Alone));

    let tokens = [
        colon1,
        colon2,
        core,
        colon3,
        colon4,
        compile_error,
        bang,
        parens,
        semi,
    ];

    ts.extend(tokens);
}

fn generate_nzu64_ctor(ts: &mut TokenStream, val: u64, span: Span) {
    assert_ne!(val, 0);
    let unsafekw = TokenTree::Ident(Ident::new("unsafe", span));

    let mut unsafeinner = TokenStream::new();

    let colon1 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint));
    let colon2 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Alone));
    let core = TokenTree::Ident(Ident::new("core", span));
    let colon3 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint));
    let colon4 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Alone));
    let num = TokenTree::Ident(Ident::new("num", span));
    let colon5 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint));
    let colon6 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Alone));
    let nonzerousize = TokenTree::Ident(Ident::new("NonZeroU64", span));
    let colon7 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint));
    let colon8 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Alone));
    let new_unchecked = TokenTree::Ident(Ident::new("new_unchecked", span));

    let mut new_unchecked_inner = TokenStream::new();
    new_unchecked_inner.extend([TokenTree::Literal(Literal::u64_suffixed(val))]);

    let new_unchecked_params = TokenTree::Group(Group::new(
        proc_macro::Delimiter::Parenthesis,
        new_unchecked_inner,
    ));

    unsafeinner.extend([
        colon1,
        colon2,
        core,
        colon3,
        colon4,
        num,
        colon5,
        colon6,
        nonzerousize,
        colon7,
        colon8,
        new_unchecked,
        new_unchecked_params,
    ]);

    let unsafeblk = TokenTree::Group(Group::new(proc_macro::Delimiter::Brace, unsafeinner));

    ts.extend([unsafekw, unsafeblk]);
}

fn generate_const(ts: &mut TokenStream, name: Ident, id: u64, span: Span) {
    let pubkw = TokenTree::Ident(Ident::new("pub", span));
    let constkw = TokenTree::Ident(Ident::new("const", span));

    let nameid = TokenTree::Ident(name);

    let colon0 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Alone));

    let ty = TokenTree::Ident(Ident::new("Symbol", span));

    let equals = TokenTree::Punct(Punct::new('=', proc_macro::Spacing::Alone));

    let ctorname = TokenTree::Ident(Ident::new("Symbol", span));

    let mut ctorinner = TokenStream::new();

    generate_nzu64_ctor(&mut ctorinner, id, span);

    let ctorparens = TokenTree::Group(Group::new(proc_macro::Delimiter::Parenthesis, ctorinner));

    let semi = TokenTree::Punct(Punct::new(';', proc_macro::Spacing::Alone));

    ts.extend([
        pubkw, constkw, nameid, colon0, ty, equals, ctorname, ctorparens, semi,
    ]);
}

fn generate_const_u64(ts: &mut TokenStream, name: Ident, val: u64, span: Span) {
    let constkw = TokenTree::Ident(Ident::new("const", span));
    let nameid = TokenTree::Ident(name);
    let colon0 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Alone));
    let ty = TokenTree::Ident(Ident::new("u64", span));
    let equals = TokenTree::Punct(Punct::new('=', proc_macro::Spacing::Alone));
    let lit = TokenTree::Literal(Literal::u64_suffixed(val));
    let semi = TokenTree::Punct(Punct::new(';', proc_macro::Spacing::Alone));

    ts.extend([constkw, nameid, colon0, ty, equals, lit, semi]);
}

fn generate_init(ts: &mut TokenStream, val: &str, id: u64, span: Span) {
    let insert_static_symbol = TokenTree::Ident(Ident::new("insert_static_symbol", span));

    let mut call_params = TokenStream::new();

    let blob = TokenTree::Ident(Ident::new("blob", span));

    let strtok = TokenTree::Literal(Literal::string(val));
    let comma = TokenTree::Punct(Punct::new(',', Spacing::Alone));

    call_params.extend([strtok, comma]);

    generate_nzu64_ctor(&mut call_params, id, span);

    let call_parens = TokenTree::Group(Group::new(proc_macro::Delimiter::Parenthesis, call_params));

    let semi = TokenTree::Punct(Punct::new(';', proc_macro::Spacing::Alone));

    ts.extend([insert_static_symbol, call_parens, semi]);
}

fn generate_init_fn(ts: &mut TokenStream, body: TokenStream, span: Span) {
    let fnkw = TokenTree::Ident(Ident::new("fn", span));
    let name = TokenTree::Ident(Ident::new("init_static_symbols", span));

    let mut params = TokenStream::new();

    let blob = TokenTree::Ident(Ident::new("blob", span));
    let colon0 = TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Alone));
    let amp = TokenTree::Punct(Punct::new('&', proc_macro::Spacing::Alone));
    let mutkw = TokenTree::Ident(Ident::new("mut", span));
    let blobty = TokenTree::Ident(Ident::new("InitBlob", span));

    params.extend([blob, colon0, amp, mutkw, blobty]);

    let params_parens = TokenTree::Group(Group::new(proc_macro::Delimiter::Parenthesis, params));

    let braces = TokenTree::Group(Group::new(proc_macro::Delimiter::Brace, body));

    ts.extend([fnkw, name, params_parens, braces]);
}

#[proc_macro]
pub fn gen_sym_map(ts: TokenStream) -> TokenStream {
    let mut output_stream = TokenStream::new();

    let tspan = Span::call_site();

    let mut cur_id = 1u64;

    let mut keys = Vec::new();
    let mut map = HashMap::new();
    let mut iter = ts.into_iter().peekable();
    while let Some(tt) = iter.next() {
        let span = tt.span();
        if let TokenTree::Ident(id) = tt {
            match iter.peek() {
                Some(TokenTree::Punct(p)) if p.as_char() == ':' => {
                    let spacing = p.spacing();
                    iter.next();
                    if spacing == Spacing::Joint {
                        generate_compile_error(
                            &mut output_stream,
                            format_args!("Expected `:` got `::`"),
                            span,
                        );
                        break;
                    }

                    let mut val = match iter.next() {
                        Some(TokenTree::Ident(id)) => id.to_string(),
                        Some(TokenTree::Punct(p)) => {
                            let mut val = p.to_string();
                            if p.spacing() == Spacing::Joint {
                                loop {
                                    match iter.next() {
                                        Some(TokenTree::Punct(p)) => {
                                            write!(val, "{}", p).unwrap();
                                            if p.spacing() == Spacing::Alone {
                                                break;
                                            }
                                        }
                                        Some(TokenTree::Ident(id)) => {
                                            write!(val, "{}", id).unwrap();
                                            break;
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                            }
                            val
                        }
                        Some(TokenTree::Literal(lit)) => lit.to_string(),
                        Some(tt) => {
                            generate_compile_error(
                                &mut output_stream,
                                format_args!(
                                    "Expected a sigil, lifetime, or identifier, got {}",
                                    tt
                                ),
                                tt.span(),
                            );
                            break;
                        }
                        None => {
                            generate_compile_error(
                                &mut output_stream,
                                format_args!(
                                    "Expected a sigil, lifetime, or identifier, got end of input"
                                ),
                                span,
                            );
                            break;
                        }
                    };
                }
                _ => {
                    keys.push(id.clone());
                    let str = id.to_string();
                    map.insert(id.to_string(), str);
                }
            }

            match iter.next() {
                Some(TokenTree::Punct(p)) if p.as_char() == ';' => {}
                Some(tt) => generate_compile_error(
                    &mut output_stream,
                    format_args!("Expected `;`, got {}", tt),
                    tt.span(),
                ),
                None => generate_compile_error(
                    &mut output_stream,
                    format_args!("Expected `;` got end of input"),
                    span,
                ),
            }
        } else {
            generate_compile_error(
                &mut output_stream,
                format_args!("Expected an identifier got {}", tt),
                tt.span(),
            );
            break;
        }
    }

    let mut init_fnbody = TokenStream::new();

    for name in keys {
        let span = name.span();

        let lookid = name.to_string();

        let val = &*map[&lookid];

        let id = cur_id;
        cur_id += 1;

        generate_init(&mut init_fnbody, val, id, span);

        generate_const(&mut output_stream, name, id, span)
    }

    generate_const_u64(
        &mut output_stream,
        Ident::new("INIT_DYN_VAL", tspan),
        cur_id,
        tspan,
    );
    generate_init_fn(&mut output_stream, init_fnbody, tspan);

    output_stream
}
