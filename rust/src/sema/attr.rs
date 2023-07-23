use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    ast::{AttrInput, Literal, LiteralKind, SimplePath, SimplePathSegment},
    interning::Symbol,
    lang::LangItem,
    lex::{self, do_lexeme_str, Lexeme, LexemeClass},
    parse::{self, do_lexeme_class, do_lexeme_group, do_literal, IntoRewinder},
    span::Span,
};

use super::{
    ty::{self, IntType},
    DefId, SemaHint, Spanned,
};
use crate::ast;

use super::{Error, ErrorCategory, Result};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Attr {
    Lang(Spanned<LangItem>),
    Repr(Spanned<Repr>),
    Inline(Option<Spanned<InlineFrequency>>),
    NoStd,
    NoCore,
    NoMain,
    NoMangle,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Repr {
    pub base: Spanned<ReprBase>,
    pub alignment: Option<Spanned<AlignmentSpec>>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ReprBase {
    Transparent,
    Rust,
    C,
    LCRust(Option<u16>),
    Precise(IntType),
}

impl core::fmt::Display for ReprBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReprBase::Transparent => f.write_str("transparent"),
            ReprBase::Rust => f.write_str("Rust"),
            ReprBase::C => f.write_str("C"),
            ReprBase::LCRust(None) => f.write_str("lcrust"),
            ReprBase::LCRust(Some(ver)) => f.write_fmt(format_args!("lcrust_v{}", ver)),
            ReprBase::Precise(ity) => ity.fmt(f),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum AlignmentSpec {
    Packed(u64),
    Aligned(u64),
}

impl core::fmt::Display for AlignmentSpec {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            AlignmentSpec::Packed(align) => f.write_fmt(format_args!("packed({})", align)),
            AlignmentSpec::Aligned(align) => f.write_fmt(format_args!("align({})", align)),
        }
    }
}

impl core::fmt::Display for Repr {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.base.body.fmt(f)?;
        if let Some(align) = &self.alignment {
            f.write_str(", ")?;
            align.body.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum InlineFrequency {
    Never,
    Always,
}

impl core::fmt::Display for InlineFrequency {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Never => f.write_str("never"),
            Self::Always => f.write_str("always"),
        }
    }
}

impl core::fmt::Display for Attr {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Lang(lang) => f.write_fmt(format_args!("#[lang = \"{}\"]", lang.name())),
            Self::Repr(repr) => f.write_fmt(format_args!("#[repr({})]", repr.body)),
            Self::Inline(None) => f.write_str("#[inline]"),
            Self::Inline(Some(freq)) => f.write_fmt(format_args!("#[inline({})]", freq.body)),
            Self::NoCore => f.write_str("#[no_core]"),
            Self::NoStd => f.write_str("#[no_std]"),
            Self::NoMain => f.write_str("#[no_main]"),
            Self::NoMangle => f.write_str("#[no_mangle]"),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MetaContent {
    MetaPath(Spanned<SimplePath>),
    MetaGroup(Spanned<SimplePath>, Spanned<Vec<Spanned<MetaContent>>>),
    MetaKeyValue(Spanned<SimplePath>, Spanned<Literal>),
    MetaLiteral(Spanned<Literal>),
}

impl core::fmt::Display for MetaContent {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            MetaContent::MetaPath(id) => id.body.fmt(f),
            MetaContent::MetaGroup(id, inner) => {
                id.body.fmt(f)?;
                f.write_str("(")?;
                let mut sep = "";

                for meta in &inner.body {
                    f.write_str(sep)?;
                    sep = ", ";
                    meta.body.fmt(f)?;
                }
                f.write_str(")")
            }
            MetaContent::MetaKeyValue(id, lit) => {
                id.body.fmt(f)?;
                f.write_str(" = ")?;
                lit.fmt(f)
            }
            MetaContent::MetaLiteral(lit) => lit.body.fmt(f),
        }
    }
}

fn parse_error_into_sema_error(
    e: parse::Error,
    at_item: DefId,
    containing_item: DefId,
    hints: Vec<SemaHint>,
) -> Error {
    Error {
        span: e.span,
        text: format!("Incorrect builtin attribute: {}", e),
        category: ErrorCategory::InvalidAttr,
        at_item,
        containing_item,
        relevant_item: at_item,
        hints,
    }
}

fn do_meta_content<I: Iterator<Item = Lexeme>>(
    tree: &mut PeekMoreIterator<I>,
) -> parse::Result<Spanned<MetaContent>> {
    let id = parse::do_simple_path(tree)?;
    let idspan = id.span;
    let mut rewinder = tree.into_rewinder();

    match do_lexeme_group(&mut rewinder, Some(lex::GroupType::Parens)) {
        Ok((g, gspan)) => {
            rewinder.accept();
            let mut inner_tree = g.body.into_iter().peekmore();
            let mut built = Vec::new();
            loop {
                if LexemeClass::of(inner_tree.peek()).is(&LexemeClass::Eof) {
                    break;
                }

                let inner = match do_meta_content(&mut inner_tree) {
                    Ok(content) => content,
                    Err(e) => match do_literal(&mut inner_tree) {
                        Ok(lit) => Spanned {
                            body: MetaContent::MetaLiteral(lit),
                            span: lit.span,
                        },
                        Err(d) => return Err(e | d),
                    },
                };

                built.push(inner);

                match parse::do_lexeme_classes(
                    &mut inner_tree,
                    &[
                        LexemeClass::Punctuation(Symbol::intern(",")),
                        LexemeClass::Eof,
                    ],
                )? {
                    (_, LexemeClass::Eof) => break,
                    _ => continue,
                }
            }

            Ok(Spanned {
                span: Span::between(idspan, gspan),
                body: MetaContent::MetaGroup(
                    id,
                    Spanned {
                        body: built,
                        span: gspan,
                    },
                ),
            })
        }
        Err(_) => {
            match do_lexeme_class(&mut rewinder, LexemeClass::Punctuation(Symbol::intern("="))) {
                Ok(_) => {
                    rewinder.accept();

                    let lit = do_literal(tree)?;

                    Ok(Spanned {
                        span: Span::between(idspan, lit.span),
                        body: MetaContent::MetaKeyValue(id, lit),
                    })
                }
                Err(_) => Ok(Spanned {
                    span: idspan,
                    body: MetaContent::MetaPath(id),
                }),
            }
        }
    }
}

fn into_meta_content(m: &Spanned<ast::Attr>) -> parse::Result<Spanned<MetaContent>> {
    m.try_copy_span(|meta| {
        let id = meta.name.clone();

        match &meta.input {
            None => Ok(MetaContent::MetaPath(id)),
            Some(AttrInput::DelimTokenTree(inner)) => {
                let mut inner_tree = inner.iter().cloned().peekmore();
                let mut built = Vec::new();

                loop {
                    if LexemeClass::of(inner_tree.peek()).is(&LexemeClass::Eof) {
                        break;
                    }

                    let inner = match do_meta_content(&mut inner_tree) {
                        Ok(content) => content,
                        Err(e) => match do_literal(&mut inner_tree) {
                            Ok(lit) => Spanned {
                                body: MetaContent::MetaLiteral(lit),
                                span: lit.span,
                            },
                            Err(d) => return Err(e | d),
                        },
                    };

                    built.push(inner);

                    match parse::do_lexeme_classes(
                        &mut inner_tree,
                        &[
                            LexemeClass::Punctuation(Symbol::intern(",")),
                            LexemeClass::Eof,
                        ],
                    )? {
                        (_, LexemeClass::Eof) => break,
                        _ => continue,
                    }
                }
                Ok(MetaContent::MetaGroup(
                    id,
                    Spanned {
                        body: built,
                        span: inner.span,
                    },
                ))
            }
            Some(AttrInput::MetaValue(val)) => Ok(MetaContent::MetaKeyValue(id, *val)),
        }
    })
}

fn convert_lang_item(
    l: Spanned<Literal>,
    at_item: DefId,
    containing_item: DefId,
    attr_span: Span,
) -> Result<Spanned<LangItem>> {
    l.try_copy_span(|lit| match lit.lit_kind {
        LiteralKind::String(ast::StringType::Default | ast::StringType::Raw(_)) => {
            LangItem::from_name(&lit.val).ok_or_else(|| Error {
                span: l.span,
                text: format!(
                    "Invalid `#[lang]` attribute: Unknown language item {}",
                    lit.val.body
                ),
                category: ErrorCategory::InvalidAttr,
                at_item,
                containing_item,
                relevant_item: at_item,
                hints: vec![SemaHint {
                    text: format!("at this `#[lang]` attribute"),
                    itemref: at_item,
                    refspan: attr_span,
                }],
            })
        }
        _ => Err(Error {
            span: l.span,
            text: format!(
                "Invalid `#[lang]` attribute: Expected a string literal, got {}",
                lit.val.body
            ),
            category: ErrorCategory::InvalidAttr,
            at_item,
            containing_item,
            relevant_item: at_item,
            hints: vec![SemaHint {
                text: format!("at this `#[lang]` attribute"),
                itemref: at_item,
                refspan: attr_span,
            }],
        }),
    })
}

pub fn convert_repr(
    inner: Spanned<Vec<Spanned<MetaContent>>>,
    at_item: DefId,
    containing_item: DefId,
    attr_span: Span,
) -> Result<Spanned<Repr>> {
    let mut base = None::<Spanned<ReprBase>>;
    let mut alignment = None::<Spanned<AlignmentSpec>>;

    for meta in inner.body {
        let span = meta.span;
        match meta.body {
            MetaContent::MetaPath(id) => {
                let has_base = match base {
                    Some(base) => Err(Error {
                        span,
                        text: format!(
                            "Cannot combine `repr({})` with `repr({})`",
                            base.body, id.body
                        ),
                        category: ErrorCategory::InvalidAttr,
                        at_item,
                        containing_item,
                        relevant_item: at_item,
                        hints: vec![],
                    }),
                    None => Ok(()),
                }; // I'd eagerly throw this error here, but `repr(align)` shouldn't get a confusing diangostic like "Cannot combine repr(C) and repr(align)"

                match id {
                    id if matches_simple_path!(id, Rust) => {
                        has_base?;
                        base = Some(Spanned {
                            body: ReprBase::Rust,
                            span: id.span,
                        });
                    }
                    id if matches_simple_path!(id, C) => {
                        has_base?;
                        base = Some(Spanned {
                            body: ReprBase::C,
                            span: id.span,
                        });
                    }
                    id if matches_simple_path!(id, transparent) => {
                        has_base?;
                        base = Some(Spanned {
                            body: ReprBase::Transparent,
                            span: id.span,
                        });
                    }
                    id if matches_simple_path!(id, lcrust) => {
                        has_base?;
                        base = Some(Spanned {
                            body: ReprBase::Transparent,
                            span: id.span,
                        });
                    }
                    id if matches_simple_path!(id, align) => {
                        return Err(Error {
                            span,
                            text: format!("Expected an alignment value for repr(align) attribute"),
                            category: ErrorCategory::InvalidAttr,
                            at_item,
                            containing_item,
                            relevant_item: at_item,
                            hints: vec![SemaHint {
                                text: format!("Add the alignment here, like `align(8)`"),
                                itemref: at_item,
                                refspan: id.span,
                            }],
                        });
                    }
                    id if matches_simple_path!(id, packed) => {
                        if let Some(align) = alignment {
                            return Err(Error {
                                span,
                                text: format!(
                                    "Cannot combine `repr({})` with `repr(packed)`",
                                    align.body
                                ),
                                category: ErrorCategory::InvalidAttr,
                                at_item,
                                containing_item,
                                relevant_item: at_item,
                                hints: vec![],
                            });
                        }

                        alignment = Some(Spanned {
                            body: AlignmentSpec::Packed(1),
                            span: id.span,
                        })
                    }
                    id if !id.from_root => match &*id.segments {
                        [Spanned {
                            body: SimplePathSegment::Identifier(id),
                            span,
                        }] => {
                            let span = *span;
                            if let Some(ver) = id.strip_prefix("lcrust_v") {
                                let ver = ver.parse().map_err(|e| Error {
                                    span,
                                    text: format!("Unknown repr({})", id),
                                    category: ErrorCategory::InvalidAttr,
                                    at_item,
                                    containing_item,
                                    relevant_item: at_item,
                                    hints: vec![],
                                })?;

                                has_base?;

                                base = Some(Spanned {
                                    span,
                                    body: ReprBase::LCRust(Some(ver)),
                                })
                            } else if let Ok(ity) = ty::parse_int_type(
                                Spanned { body: *id, span },
                                at_item,
                                containing_item,
                            ) {
                                has_base?;

                                base = Some(Spanned {
                                    span,
                                    body: ReprBase::Precise(*ity),
                                })
                            } else {
                                return Err(Error {
                                    span,
                                    text: format!("Unknown repr({})", id),
                                    category: ErrorCategory::InvalidAttr,
                                    at_item,
                                    containing_item,
                                    relevant_item: at_item,
                                    hints: vec![],
                                });
                            }
                        }
                        _ => {
                            return Err(Error {
                                span,
                                text: format!("Unknown repr({})", id.body),
                                category: ErrorCategory::InvalidAttr,
                                at_item,
                                containing_item,
                                relevant_item: at_item,
                                hints: vec![],
                            });
                        }
                    },
                    id => {
                        return Err(Error {
                            span,
                            text: format!("Unknown repr({})", id.body),
                            category: ErrorCategory::InvalidAttr,
                            at_item,
                            containing_item,
                            relevant_item: at_item,
                            hints: vec![],
                        });
                    }
                }
            }
            MetaContent::MetaGroup(id, inner) => {
                let has_align = match alignment {
                    Some(align) => Err(Error {
                        span,
                        text: format!("Cannot combine `repr({})` with `repr(packed)`", align.body),
                        category: ErrorCategory::InvalidAttr,
                        at_item,
                        containing_item,
                        relevant_item: at_item,
                        hints: vec![],
                    }),
                    None => Ok(()),
                };

                let valid_inner = match &*inner.body {
                    [Spanned {
                        span,
                        body:
                            MetaContent::MetaLiteral(Spanned {
                                body:
                                    Literal {
                                        val,
                                        lit_kind: LiteralKind::Int(None),
                                    },
                                ..
                            }),
                    }] => {
                        let val = val.parse::<u64>().map_err(|e| Error {
                            span: *span,
                            text: format!(
                                "Invalid alignment value {} (exceeds u64::max())",
                                val.body
                            ),
                            at_item,
                            containing_item,
                            relevant_item: at_item,
                            category: ErrorCategory::InvalidAttr,
                            hints: vec![],
                        });

                        match val {
                            Ok(val) if val.is_power_of_two() => Ok(val),
                            Ok(val) => Err(Error {
                                span: *span,
                                text: format!(
                                    "Invalid alignment value {} (alignment must be a power of two)",
                                    val
                                ),
                                at_item,
                                containing_item,
                                relevant_item: at_item,
                                category: ErrorCategory::InvalidAttr,
                                hints: vec![],
                            }),
                            Err(e) => Err(e),
                        }
                    }
                    _ => Err(Error {
                        span,
                        text: format!("Unexpected value in alignment spec"),
                        at_item,
                        containing_item,
                        relevant_item: at_item,
                        category: ErrorCategory::InvalidAttr,
                        hints: vec![],
                    }),
                };

                match id {
                    id if matches_simple_path!(id, packed) => {
                        has_align?;

                        let val = valid_inner?;
                        alignment = Some(Spanned {
                            body: AlignmentSpec::Packed(val),
                            span,
                        })
                    }
                    id if matches_simple_path!(id, align) => {
                        has_align?;

                        let val = valid_inner?;
                        alignment = Some(Spanned {
                            body: AlignmentSpec::Aligned(val),
                            span,
                        })
                    }
                    id => {
                        return Err(Error {
                            span,
                            text: format!("Unknown repr({})", id.body),
                            category: ErrorCategory::InvalidAttr,
                            at_item,
                            containing_item,
                            relevant_item: at_item,
                            hints: vec![],
                        });
                    }
                }
            }
            m => {
                return Err(Error {
                    span,
                    text: format!("Unknown repr({})", m),
                    category: ErrorCategory::InvalidAttr,
                    at_item,
                    containing_item,
                    relevant_item: at_item,
                    hints: vec![],
                });
            }
        }
    }

    let base = base.unwrap_or(Spanned {
        body: ReprBase::Rust,
        span: inner.span,
    });

    match (base.body, alignment) {
        (ReprBase::Transparent, Some(align)) => Err(Error {
            span: base.span,
            text: format!("Cannot combine repr(transparent) and repr({})", align.body),
            category: ErrorCategory::InvalidAttr,
            at_item,
            containing_item,
            relevant_item: at_item,
            hints: vec![
                SemaHint {
                    text: format!(
                        "Use repr(C,{}) instead if you want a consistent layout",
                        align.body
                    ),
                    itemref: at_item,
                    refspan: inner.span,
                },
                SemaHint {
                    text: format!("Or remove this repr({})", align.body),
                    itemref: at_item,
                    refspan: align.span,
                },
            ],
        }),
        _ => Ok(Spanned {
            span: inner.span,
            body: Repr { base, alignment },
        }),
    }
}

pub fn parse_meta(
    m: &Spanned<ast::Attr>,
    at_item: DefId,
    containing_item: DefId,
) -> Result<Spanned<Attr>> {
    let content = into_meta_content(m)
        .map_err(|e| parse_error_into_sema_error(e, at_item, containing_item, vec![]))?;
    let span = content.span;
    match content.body {
        MetaContent::MetaKeyValue(id, lit) if matches_simple_path!(id, lang) => {
            let lang = convert_lang_item(lit, at_item, containing_item, content.span)?;

            Ok(Spanned {
                body: Attr::Lang(lang),
                span,
            })
        }
        MetaContent::MetaGroup(id, inner) if matches_simple_path!(id, repr) => {
            let repr = convert_repr(inner, at_item, containing_item, content.span)?;

            Ok(Spanned {
                body: Attr::Repr(repr),
                span,
            })
        }
        MetaContent::MetaPath(id) if matches_simple_path!(id, inline) => Ok(Spanned {
            body: Attr::Inline(None),
            span,
        }),
        MetaContent::MetaGroup(id, inner) if matches_simple_path!(id, inline) => {
            let inner = match &*inner.body{
                [Spanned{body: MetaContent::MetaPath(id),span}] => {
                    match id{
                        id if matches_simple_path!(id,always) => Spanned{body: InlineFrequency::Always,span: *span},
                        id if matches_simple_path!(id,never) => Spanned{body: InlineFrequency::Never,span: *span},
                        id => return Err(Error{
                            span: *span,
                            text: format!("Invalid #[inline] attribute: expected either `inline(always)` or `inline(never)`"),
                            category: ErrorCategory::InvalidAttr,
                            at_item,
                            containing_item,
                            relevant_item: at_item,
                            hints: vec![]
                        })
                    }
                }
                _ => {
                    return Err(Error{
                        span: inner.span,
                        text: format!("Invalid #[inline] attribute: expected either `inline(always)` or `inline(never)`"),
                        category: ErrorCategory::InvalidAttr,
                        at_item,
                        containing_item,
                        relevant_item: at_item,
                        hints: vec![]
                    })
                }
            };

            Ok(Spanned {
                body: Attr::Inline(Some(inner)),
                span,
            })
        }
        MetaContent::MetaPath(id) if matches_simple_path!(id, no_core) => Ok(Spanned {
            body: Attr::NoCore,
            span,
        }),
        MetaContent::MetaPath(id) if matches_simple_path!(id, no_std) => Ok(Spanned {
            body: Attr::NoStd,
            span,
        }),
        MetaContent::MetaPath(id) if matches_simple_path!(id, no_main) => Ok(Spanned {
            body: Attr::NoMain,
            span,
        }),
        MetaContent::MetaPath(id) if matches_simple_path!(id, no_mangle) => Ok(Spanned {
            body: Attr::NoMangle,
            span,
        }),
        m => Err(Error {
            span,
            text: format!("Invalid built-in attrbute #[{}]", m),
            at_item,
            containing_item,
            category: ErrorCategory::InvalidAttr,
            relevant_item: at_item,
            hints: vec![],
        }),
    }
}
