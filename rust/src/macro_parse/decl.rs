use peekmore::{PeekMore, PeekMoreIterator};
use xlang::abi::collection::HashMap;

use crate::{
    ast,
    interning::Symbol,
    lex::{
        do_group, keyword, punct, Group, GroupType, Lexeme, LexemeBody, LexemeClass, Punctuation,
        Token,
    },
    parse::{
        do_lexeme_class, do_lexeme_classes, do_lexeme_group, do_lexeme_token, do_token_classes,
        IntoRewinder,
    },
    span::{HygieneRef, RustEdition, Span, Spanned},
};

use super::ErrorCode;
pub use super::{Error, Result};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroMatcherKind {
    Tt,
    Ident,
    Literal,
    Lifetime,
    Item,
    Expr,
    Visibility,
    Meta,
    Block,
    Statement,
    Type,
    Path,
    PatternTop,
    PatternNoTop,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct MacroMatcher {
    pub kind: Spanned<MacroMatcherKind>,
    pub id: Spanned<Symbol>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum RepetitionType {
    Optional,
    ZeroOrMore,
    OneOrMore,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RepetitionMatcher {
    pub tokens: Vec<Spanned<MacroInputToken>>,
    pub sep: Option<Spanned<Token>>,
    pub repty: Spanned<RepetitionType>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroInputToken {
    BareToken(Token),
    Group(GroupType, Vec<Spanned<MacroInputToken>>),
    Repetition(RepetitionMatcher),
    Matcher(MacroMatcher),
    Unsatisfiable,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RepetitionExpansion {
    pub tokens: Vec<Spanned<MacroExpansionToken>>,
    pub sep: Option<Spanned<Token>>,
    pub repty: Spanned<RepetitionType>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum InterpolationMode {
    Token,
    Ignore,
    CountReps,
    CountNests,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Interpolation {
    pub mode: InterpolationMode,
    pub ident: Spanned<Symbol>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroExpansionToken {
    BareToken(Token),
    DollarCrate,
    Group(GroupType, Vec<Spanned<MacroExpansionToken>>),
    Repetition(RepetitionExpansion),
    Interpolation(Interpolation),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MacroArm {
    pub matcher: Vec<Spanned<MacroInputToken>>,
    pub expansion: Vec<Spanned<MacroExpansionToken>>,
}

pub fn do_input_group<I: Iterator<Item = Lexeme>>(
    tree: &mut PeekMoreIterator<I>,
) -> Result<Vec<Spanned<MacroInputToken>>> {
    let mut tree = tree.into_rewinder();
    let mut ret = Vec::new();
    loop {
        match do_lexeme_classes(&mut tree, &[punct!($), LexemeClass::Eof]) {
            Ok((_, LexemeClass::Eof)) => break,
            Ok((tok, punct!($))) => {
                let startspan = tok.span;
                match do_lexeme_token(&mut tree, LexemeClass::IdentOrKeyword) {
                    Ok((span, tok)) => {
                        if &tok.body == "crate" {
                            return Err(Error::from_parse_err(
                                crate::parse::Error {
                                    expected: vec![LexemeClass::IdentOrKeyword],
                                    got: keyword!(crate),
                                    span,
                                },
                                ErrorCode::IllFormedMacro,
                            ));
                        }

                        let id = Spanned {
                            span,
                            body: tok.body,
                        };

                        do_lexeme_class(&mut tree, punct!(:))
                            .map_err(|e| Error::from_parse_err(e, ErrorCode::IllFormedMacro))?;

                        match do_token_classes(
                            &mut tree,
                            &[
                                keyword!(block),
                                keyword!(expr),
                                keyword!(ident),
                                keyword!(item),
                                keyword!(lifetime),
                                keyword!(literal),
                                keyword!(meta),
                                keyword!(pat),
                                keyword!(pat_param),
                                keyword!(path),
                                keyword!(stmt),
                                keyword!(tt),
                                keyword!(ty),
                                keyword!(vis),
                            ],
                        ) {
                            Ok((kw, span, _)) => {
                                let kind = match kw {
                                    keyword!(block) => MacroMatcherKind::Block,
                                    keyword!(expr) => MacroMatcherKind::Expr,
                                    keyword!(ident) => MacroMatcherKind::Ident,
                                    keyword!(item) => MacroMatcherKind::Item,
                                    keyword!(lifetime) => MacroMatcherKind::Lifetime,
                                    keyword!(literal) => MacroMatcherKind::Literal,
                                    keyword!(meta) => MacroMatcherKind::Meta,
                                    keyword!(pat)
                                        if matches!(
                                            span.hygiene.edition(),
                                            Some(RustEdition::Rust2015 | RustEdition::Rust2018)
                                        ) =>
                                    {
                                        MacroMatcherKind::PatternNoTop
                                    }
                                    keyword!(pat) => MacroMatcherKind::PatternTop,
                                    keyword!(pat_param) => MacroMatcherKind::PatternNoTop,
                                    keyword!(stmt) => MacroMatcherKind::Statement,
                                    keyword!(tt) => MacroMatcherKind::Tt,
                                    keyword!(ty) => MacroMatcherKind::Type,
                                    keyword!(vis) => MacroMatcherKind::Visibility,
                                    _ => unreachable!(),
                                };
                                let matcher = MacroMatcher {
                                    id,
                                    kind: Spanned { span, body: kind },
                                };
                                let body = MacroInputToken::Matcher(matcher);
                                let span = Span::between(startspan, span);
                                ret.push(Spanned { span, body });
                            }
                            Err(e) => {
                                return Err(Error::from_parse_err(e, ErrorCode::IllFormedMacro))
                            }
                        }
                    }
                    Err(e) => match do_lexeme_group(&mut tree, Some(GroupType::Parens)) {
                        Ok((group, span)) => {
                            let mut inner_tree = group.body.into_iter().peekmore();
                            let tokens = do_input_group(&mut inner_tree)?;

                            let sep = do_token_classes(
                                &mut tree,
                                &[
                                    LexemeClass::Lifetime,
                                    LexemeClass::IdentOrKeyword,
                                    LexemeClass::String,
                                    LexemeClass::Number,
                                    LexemeClass::Character,
                                    punct!(-),
                                    punct!(/),
                                    punct!(%),
                                    punct!(^),
                                    punct!(!),
                                    punct!(&),
                                    punct!(|),
                                    punct!(&&),
                                    punct!(||),
                                    punct!(<<),
                                    punct!(>>),
                                    punct!(+=),
                                    punct!(-=),
                                    punct!(*=),
                                    punct!(/=),
                                    punct!(%=),
                                    punct!(^=),
                                    punct!(&=),
                                    punct!(|=),
                                    punct!(<<=),
                                    punct!(>>=),
                                    punct!(=),
                                    punct!(==),
                                    punct!(!=),
                                    punct!(>),
                                    punct!(<),
                                    punct!(>=),
                                    punct!(<=),
                                    punct!(@),
                                    punct!(.),
                                    punct!(..),
                                    punct!(...),
                                    punct!(..=),
                                    punct!(,),
                                    punct!(;),
                                    punct!(:),
                                    punct!(::),
                                    punct!(->),
                                    punct!(=>),
                                    punct!(#),
                                    punct!(~),
                                ],
                            )
                            .ok()
                            .map(|(_, span, body)| Spanned { span, body });

                            let repty = match do_token_classes(
                                &mut tree,
                                &[punct!(?), punct!(*), punct!(+)],
                            ) {
                                Ok((punct!(?), span, _)) => Spanned {
                                    span,
                                    body: RepetitionType::Optional,
                                },
                                Ok((punct!(*), span, _)) => Spanned {
                                    span,
                                    body: RepetitionType::ZeroOrMore,
                                },
                                Ok((punct!(+), span, _)) => Spanned {
                                    span,
                                    body: RepetitionType::OneOrMore,
                                },
                                Ok(_) => unreachable!(),
                                Err(e) => {
                                    return Err(Error::from_parse_err(e, ErrorCode::IllFormedMacro))
                                }
                            };

                            let endspan = repty.span;

                            let body = RepetitionMatcher { tokens, sep, repty };
                            let span = Span::between(startspan, endspan);

                            let body = MacroInputToken::Repetition(body);
                            ret.push(Spanned { span, body });
                        }
                        Err(d) => {
                            return Err(Error::from_parse_err(e | d, ErrorCode::IllFormedMacro))
                        }
                    },
                }
            }
            Ok(_) => unreachable!(),
            Err(_) => match do_lexeme_group(&mut tree, None) {
                Ok((group, span)) => {
                    let mut inner_tree = group.body.into_iter().peekmore();
                    let tokens = do_input_group(&mut inner_tree)?;
                    ret.push(Spanned {
                        span,
                        body: MacroInputToken::Group(group.ty, tokens),
                    });
                }
                Err(_) => match tree.peek_next() {
                    Some(Lexeme {
                        span,
                        body: LexemeBody::Token(tok),
                    }) => {
                        let tok = tok.clone();
                        let span = *span;
                        ret.push(Spanned {
                            span,
                            body: MacroInputToken::BareToken(tok),
                        });
                    }
                    Some(Lexeme {
                        span,
                        body: LexemeBody::AstFrag(_),
                    }) => {
                        let span = *span;
                        ret.push(Spanned {
                            span,
                            body: MacroInputToken::Unsatisfiable,
                        })
                    }
                    _ => unreachable!(),
                },
            },
        }
    }
    tree.accept();
    Ok(ret)
}

pub fn do_expansion_group<I: Iterator<Item = Lexeme>>(
    tree: &mut PeekMoreIterator<I>,
) -> Result<Vec<Spanned<MacroExpansionToken>>> {
    let mut tree = tree.into_rewinder();
    let mut ret = Vec::new();
    loop {
        match do_lexeme_classes(&mut tree, &[punct!($), LexemeClass::Eof]) {
            Ok((_, LexemeClass::Eof)) => break,
            Ok((lex, punct!($))) => {
                let startspan = lex.span;
                match do_lexeme_token(&mut tree, LexemeClass::IdentOrKeyword) {
                    Ok((span, tok)) => {
                        if &tok.body == "crate" {
                            let span = Span::between(startspan, span);
                            ret.push(Spanned {
                                span,
                                body: MacroExpansionToken::DollarCrate,
                            });
                        } else {
                            let id = Spanned {
                                span,
                                body: tok.body,
                            };
                            let span = Span::between(startspan, span);
                            todo!()
                        }
                    }
                    Err(e) => match do_lexeme_group(&mut tree, Some(GroupType::Parens)) {
                        Ok((g, _)) => {
                            let mut inner_tree = g.body.into_iter().peekmore();
                            let tokens = do_expansion_group(&mut inner_tree)?;

                            let sep = do_token_classes(
                                &mut tree,
                                &[
                                    LexemeClass::Lifetime,
                                    LexemeClass::IdentOrKeyword,
                                    LexemeClass::String,
                                    LexemeClass::Number,
                                    LexemeClass::Character,
                                    punct!(-),
                                    punct!(/),
                                    punct!(%),
                                    punct!(^),
                                    punct!(!),
                                    punct!(&),
                                    punct!(|),
                                    punct!(&&),
                                    punct!(||),
                                    punct!(<<),
                                    punct!(>>),
                                    punct!(+=),
                                    punct!(-=),
                                    punct!(*=),
                                    punct!(/=),
                                    punct!(%=),
                                    punct!(^=),
                                    punct!(&=),
                                    punct!(|=),
                                    punct!(<<=),
                                    punct!(>>=),
                                    punct!(=),
                                    punct!(==),
                                    punct!(!=),
                                    punct!(>),
                                    punct!(<),
                                    punct!(>=),
                                    punct!(<=),
                                    punct!(@),
                                    punct!(.),
                                    punct!(..),
                                    punct!(...),
                                    punct!(..=),
                                    punct!(,),
                                    punct!(;),
                                    punct!(:),
                                    punct!(::),
                                    punct!(->),
                                    punct!(=>),
                                    punct!(#),
                                    punct!(~),
                                ],
                            )
                            .ok()
                            .map(|(_, span, body)| Spanned { span, body });

                            let repty = match do_token_classes(
                                &mut tree,
                                &[punct!(?), punct!(*), punct!(+)],
                            ) {
                                Ok((punct!(?), span, _)) => Spanned {
                                    span,
                                    body: RepetitionType::Optional,
                                },
                                Ok((punct!(*), span, _)) => Spanned {
                                    span,
                                    body: RepetitionType::ZeroOrMore,
                                },
                                Ok((punct!(+), span, _)) => Spanned {
                                    span,
                                    body: RepetitionType::OneOrMore,
                                },
                                Ok(_) => unreachable!(),
                                Err(e) => {
                                    return Err(Error::from_parse_err(e, ErrorCode::IllFormedMacro))
                                }
                            };

                            let endspan = repty.span;
                            let span = Span::between(startspan, endspan);

                            let body = RepetitionExpansion { tokens, sep, repty };
                            let body = MacroExpansionToken::Repetition(body);
                            ret.push(Spanned { body, span })
                        }
                        Err(d) => match do_lexeme_group(&mut tree, Some(GroupType::Braces)) {
                            Ok((group, endspan)) => todo!("Special Interpolation"),
                            Err(f) => {
                                return Err(Error::from_parse_err(
                                    e | d | f,
                                    ErrorCode::IllFormedMacro,
                                ))
                            }
                        },
                    },
                }
            }
            Ok(_) => unreachable!(),
            Err(_) => match do_lexeme_group(&mut tree, None) {
                Ok((group, span)) => {
                    let mut inner_tree = group.body.into_iter().peekmore();
                    let body = do_expansion_group(&mut inner_tree)?;
                    let body = MacroExpansionToken::Group(group.ty, body);
                    ret.push(Spanned { span, body })
                }
                Err(_) => match tree.peek_next() {
                    Some(Lexeme {
                        span,
                        body: LexemeBody::Token(tok),
                    }) => {
                        let tok = tok.clone();
                        let span = *span;
                        ret.push(Spanned {
                            span,
                            body: MacroExpansionToken::BareToken(tok),
                        });
                    }
                    Some(Lexeme {
                        span,
                        body: LexemeBody::AstFrag(_),
                    }) => todo!(),
                    _ => unreachable!(),
                },
            },
        }
    }
    tree.accept();
    Ok(ret)
}

pub fn do_macro_arm<I: Iterator<Item = Lexeme>>(
    tree: &mut PeekMoreIterator<I>,
) -> Result<Spanned<MacroArm>> {
    let mut tree = tree.into_rewinder();
    let (startspan, matcher) = match do_lexeme_group(&mut tree, None) {
        Ok((group, span)) => {
            let mut inner_tree = group.body.into_iter().peekmore();

            (span, do_input_group(&mut inner_tree)?)
        }
        Err(e) => Err(Error::from_parse_err(e, ErrorCode::IllFormedMacro))?,
    };

    do_lexeme_class(&mut tree, punct!(=>))
        .map_err(|e| Error::from_parse_err(e, ErrorCode::IllFormedMacro))?;

    let (endspan, expansion) = match do_lexeme_group(&mut tree, None) {
        Ok((group, span)) => {
            let mut inner_tree = group.body.into_iter().peekmore();

            (span, do_expansion_group(&mut inner_tree)?)
        }
        Err(e) => Err(Error::from_parse_err(e, ErrorCode::IllFormedMacro))?,
    };

    let span = Span::between(startspan, endspan);
    let body = MacroArm { matcher, expansion };

    Ok(Spanned { body, span })
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct GroupInfo {
    pub sep: Option<Spanned<Token>>,
    pub repty: RepetitionType,
    pub containing_group: Option<u32>,
    pub nesting_level: u32,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct FragmentInfo {
    pub diag_name: Spanned<Symbol>,
    pub frag_ty: MacroMatcherKind,
    pub containing_group: Option<u32>,
    pub nesting_level: u32,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct MacroInputFragments {
    pub groups: Vec<GroupInfo>,
    pub fragments: Vec<FragmentInfo>,
    pub fragment_name_lookup: HashMap<Symbol, u32>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CompiledMacroInput {
    pub input_fragments: MacroInputFragments,
    pub matcher: Vec<Spanned<CompiledMacroMatcher>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CompiledMacroMatcher {
    BareToken(Token),
    Group(GroupType, Vec<Spanned<CompiledMacroMatcher>>),
    Repetition(u32, Vec<Spanned<CompiledMacroMatcher>>),
    Matcher(u32),
    Unsatisfiable,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExpansionGroupInfo {
    pub nesting_level: u32,
    pub contained_interpolations: Vec<u32>, // all at a given `nesting_level` must repeat the same number of times.
    pub sep: Option<Spanned<Token>>,
    pub repty: RepetitionType,
    pub containing_group: Option<u32>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExpansionInterpolationInfo {
    pub nesting_level: u32,
    pub containing_group: Option<u32>,
    pub mode: InterpolationMode,
    pub fragment_spec: u32,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct MacroExpansionFragments {
    pub groups: Vec<ExpansionGroupInfo>,
    pub interpolations: Vec<ExpansionInterpolationInfo>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CompiledExpansionToken {
    BareToken(Token),
    Group(GroupType, Vec<Spanned<CompiledExpansionToken>>),
    Repetition(u32, Vec<Spanned<CompiledExpansionToken>>),
    Interpolation(u32),
    DollarCrate,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CompiledMacroExpansion {
    pub hygiene: HygieneRef,
    pub fragments: MacroExpansionFragments,
    pub expansion: Vec<Spanned<CompiledExpansionToken>>,
}

pub fn compile_input_token(
    tok: Spanned<MacroInputToken>,
    input_fragments: &mut MacroInputFragments,
    containing_group: Option<u32>,
    nesting_level: u32,
) -> Result<Spanned<CompiledMacroMatcher>> {
    let Spanned { body, span } = tok;
    match body {
        MacroInputToken::BareToken(tok) => Ok(Spanned {
            body: CompiledMacroMatcher::BareToken(tok),
            span,
        }),
        MacroInputToken::Group(gty, inputs) => {
            let matchers = inputs
                .into_iter()
                .map(|tok| {
                    compile_input_token(tok, input_fragments, containing_group, nesting_level)
                })
                .collect::<Result<Vec<_>>>()?;

            Ok(Spanned {
                body: CompiledMacroMatcher::Group(gty, matchers),
                span,
            })
        }
        MacroInputToken::Repetition(rep) => {
            let next_group = input_fragments.groups.len() as u32;
            let group = GroupInfo {
                sep: rep.sep,
                repty: rep.repty.body,
                containing_group,
                nesting_level,
            };
            input_fragments.groups.push(group);
            let matchers = rep
                .tokens
                .into_iter()
                .map(|tok| {
                    compile_input_token(tok, input_fragments, Some(next_group), nesting_level + 1)
                })
                .collect::<Result<Vec<_>>>()?;

            Ok(Spanned {
                body: CompiledMacroMatcher::Repetition(next_group, matchers),
                span,
            })
        }
        MacroInputToken::Matcher(matcher) => {
            let name = matcher.id;

            let next_frag = input_fragments.fragments.len() as u32;

            if let Some(existing) = input_fragments
                .fragment_name_lookup
                .insert(name.body, next_frag)
            {
                return Err(Error {
                    parse_err: None,
                    text: Some(format!("Attempted redefinition of fragment {}", name.body)),
                    span,
                    code: ErrorCode::MacroCompilationError,
                });
            }

            let fragment = FragmentInfo {
                diag_name: name,
                frag_ty: matcher.kind.body,
                containing_group,
                nesting_level,
            };

            input_fragments.fragments.push(fragment);

            Ok(Spanned {
                body: CompiledMacroMatcher::Matcher(next_frag),
                span,
            })
        }
        MacroInputToken::Unsatisfiable => Ok(Spanned {
            body: CompiledMacroMatcher::Unsatisfiable,
            span,
        }),
    }
}

pub fn compile_expansion_token(
    tok: Spanned<MacroExpansionToken>,
    expansion_fragments: &mut MacroExpansionFragments,
    containing_group: Option<u32>,
    nesting_level: u32,
    contained_interpolations: &mut Vec<u32>,
    input_fragments: &MacroInputFragments,
) -> Result<Spanned<CompiledExpansionToken>> {
    let Spanned { body, span } = tok;

    match body {
        MacroExpansionToken::BareToken(tok) => Ok(Spanned {
            body: CompiledExpansionToken::BareToken(tok),
            span,
        }),
        MacroExpansionToken::DollarCrate => Ok(Spanned {
            body: CompiledExpansionToken::DollarCrate,
            span,
        }),
        MacroExpansionToken::Group(gty, inner) => {
            let body = inner
                .into_iter()
                .map(|s| {
                    compile_expansion_token(
                        s,
                        expansion_fragments,
                        containing_group,
                        nesting_level,
                        contained_interpolations,
                        input_fragments,
                    )
                })
                .collect::<Result<Vec<_>>>()?;

            Ok(Spanned {
                body: CompiledExpansionToken::Group(gty, body),
                span,
            })
        }
        MacroExpansionToken::Repetition(rep) => {
            let mut group_info = ExpansionGroupInfo {
                nesting_level,
                contained_interpolations: Vec::new(),
                sep: rep.sep,
                repty: rep.repty.body,
                containing_group,
            };
            let nextgroup = expansion_fragments.groups.len() as u32;
            expansion_fragments.groups.push(group_info);
            let mut next_interpolations = Vec::new();

            let body = rep
                .tokens
                .into_iter()
                .map(|s| {
                    compile_expansion_token(
                        s,
                        expansion_fragments,
                        Some(nextgroup),
                        nesting_level + 1,
                        &mut next_interpolations,
                        input_fragments,
                    )
                })
                .collect::<Result<Vec<_>>>()?;

            contained_interpolations.extend_from_slice(&next_interpolations);

            expansion_fragments.groups[nextgroup as usize].contained_interpolations =
                next_interpolations;

            Ok(Spanned {
                body: CompiledExpansionToken::Repetition(nextgroup, body),
                span,
            })
        }
        MacroExpansionToken::Interpolation(int) => {
            let fragment_spec = input_fragments
                .fragment_name_lookup
                .get(&int.ident.body)
                .copied()
                .ok_or_else(|| Error {
                    parse_err: None,
                    text: Some(format!(
                        "Matcher fragment `{}` does not exist",
                        int.ident.body
                    )),
                    span: int.ident.span,
                    code: ErrorCode::MacroCompilationError,
                })?;
            let mut interp_info = ExpansionInterpolationInfo {
                nesting_level,
                containing_group,
                mode: int.mode,
                fragment_spec,
            };

            let next_interp = expansion_fragments.interpolations.len() as u32;
            expansion_fragments.interpolations.push(interp_info);

            Ok(Spanned {
                body: CompiledExpansionToken::Interpolation(next_interp),
                span,
            })
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CompiledMacroArm {
    pub input: CompiledMacroInput,
    pub expansion: CompiledMacroExpansion,
}

pub fn compile_macro_arm(x: MacroArm, hygiene: HygieneRef) -> Result<CompiledMacroArm> {
    let mut input_fragments = MacroInputFragments::default();
    let mut expansion_fragments = MacroExpansionFragments::default();

    let matcher = x
        .matcher
        .into_iter()
        .map(|tok| compile_input_token(tok, &mut input_fragments, None, 0))
        .collect::<Result<Vec<_>>>()?;

    let mut interp_collector = Vec::new();
    let expansion = x
        .expansion
        .into_iter()
        .map(|tok| {
            compile_expansion_token(
                tok,
                &mut expansion_fragments,
                None,
                0,
                &mut interp_collector,
                &input_fragments,
            )
        })
        .collect::<Result<Vec<_>>>()?;

    let input = CompiledMacroInput {
        input_fragments,
        matcher,
    };
    let expansion = CompiledMacroExpansion {
        expansion,
        fragments: expansion_fragments,
        hygiene,
    };

    Ok(CompiledMacroArm { input, expansion })
}
