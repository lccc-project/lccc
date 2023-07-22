use crate::{ast::LiteralKind, interning::Symbol, lang::LangItem};

use super::{DefId, SemaHint, Spanned};
use crate::ast;

use super::{Error, ErrorCategory, Result};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Attr {
    Lang(LangItem),
}

pub fn parse_meta(
    m: &Spanned<ast::Attr>,
    at_item: DefId,
    containing_item: DefId,
) -> Result<Spanned<Attr>> {
    m.try_copy_span(|meta| {
        if matches_simple_path!(meta.name, lang) {
            match meta.input {
                Some(ast::AttrInput::MetaValue(lit)) => {
                    if let LiteralKind::String(ast::StringType::Default | ast::StringType::Raw(_)) =
                        lit.lit_kind
                    {
                        match LangItem::from_name(&lit.val) {
                            Some(lang) => Ok(Attr::Lang(lang)),
                            None => Err(Error {
                                span: lit.val.span,
                                text: format!(
                                    "Unrecognized lang item \"{}\"",
                                    lit.val.escape_default()
                                ),
                                category: ErrorCategory::InvalidAttr,
                                at_item,
                                containing_item,
                                relevant_item: at_item,
                                hints: vec![SemaHint {
                                    text: format!("at this `#[lang]` attribute"),
                                    itemref: at_item,
                                    refspan: m.span,
                                }],
                            }),
                        }
                    } else {
                        Err(Error {
                            span: lit.span,
                            text: format!(
                                "Incorrect `#[lang]` attribute: Expected a string literal"
                            ),
                            category: ErrorCategory::InvalidAttr,
                            at_item,
                            containing_item,
                            relevant_item: at_item,
                            hints: vec![SemaHint {
                                text: format!("at this `#[lang]` attribute"),
                                itemref: at_item,
                                refspan: m.span,
                            }],
                        })
                    }
                }
                _ => Err(Error {
                    span: m.span,
                    text: format!("Incorrect `#[lang]` attribute expected a key-value pair"),
                    category: ErrorCategory::InvalidAttr,
                    at_item,
                    containing_item,
                    relevant_item: at_item,
                    hints: vec![],
                }),
            }
        } else {
            Err(Error {
                span: meta.name.span,
                text: format!("unrecognized builtin attribute {}", meta.name.body),
                category: ErrorCategory::InvalidAttr,
                at_item,
                containing_item,
                relevant_item: at_item,
                hints: vec![],
            })
        }
    })
}
