use core::ops::{BitOr, BitOrAssign, Deref, DerefMut};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    ast::{
        Attr, AttrInput, Block, CompoundBlock, Expr, ExternBlock, Function, Item, ItemBody,
        ItemValue, Literal, LiteralKind, Mod, Param, Path, PathSegment, Pattern, SimplePath,
        SimplePathSegment, Spanned, Statement, Type, UserType, Visibility,
    },
    interning::Symbol,
    lex::{Group, GroupType, IsEof, Lexeme, LexemeBody, LexemeClass, StringType, Token, TokenType},
    sema::ty::Mutability,
    span::{Pos, Span},
};

#[derive(Debug)]
pub struct Error {
    pub expected: Vec<LexemeClass>,
    pub got: LexemeClass,
    pub span: Span,
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        use core::fmt::Debug; // TODO: Pretty-print Lexeme Class as well
        f.write_str("Expected ")?;

        let mut sep = "";
        for class in &self.expected {
            f.write_str(sep)?;
            sep = ", ";
            class.fmt(f)?;
        }

        f.write_str(" got ")?;

        self.got.fmt(f)
    }
}

impl BitOr for Error {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        self |= rhs;
        self
    }
}

impl BitOrAssign for Error {
    fn bitor_assign(&mut self, mut rhs: Self) {
        if self.span.start > rhs.span.start {
            ()
        } else if self.span.start < rhs.span.start {
            *self = rhs;
        } else if self.span.start == rhs.span.start {
            self.expected.append(&mut rhs.expected);
        } else {
            todo!("{:?}, {:?}", self, rhs); // Uh oh. Multiple files collided.
        }
    }
}

pub type Result<T> = core::result::Result<T, Error>;

pub struct Rewinder<'a, T: Iterator<Item = Lexeme>> {
    inner: &'a mut PeekMoreIterator<T>,
    cursor: usize,
}

impl<T: Iterator<Item = Lexeme>> Rewinder<'_, T> {
    pub fn accept(self) {
        core::mem::forget(self) // Rewind? Oops, I forgor
    }
}

impl<'a, T: Iterator<Item = Lexeme>> From<&'a mut PeekMoreIterator<T>> for Rewinder<'a, T> {
    fn from(inner: &'a mut PeekMoreIterator<T>) -> Self {
        let cursor = inner.cursor();
        Self { inner, cursor }
    }
}

impl<'a, T: Iterator<Item = Lexeme>> Drop for Rewinder<'a, T> {
    fn drop(&mut self) {
        self.inner.move_nth(self.cursor);
    }
}

impl<'a, T: Iterator<Item = Lexeme>> Deref for Rewinder<'a, T> {
    type Target = PeekMoreIterator<T>;

    fn deref(&self) -> &PeekMoreIterator<T> {
        &self.inner
    }
}

impl<'a, T: Iterator<Item = Lexeme>> DerefMut for Rewinder<'a, T> {
    fn deref_mut(&mut self) -> &mut PeekMoreIterator<T> {
        &mut self.inner
    }
}

pub trait IntoRewinder<'a> {
    type Iter: Iterator<Item = Lexeme>;
    fn into_rewinder(self) -> Rewinder<'a, Self::Iter>;
}

impl<'a, T: Iterator<Item = Lexeme>> IntoRewinder<'a> for &'a mut PeekMoreIterator<T> {
    type Iter = T;
    fn into_rewinder(self) -> Rewinder<'a, T> {
        Rewinder::from(self)
    }
}

pub fn do_lexeme_class(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    expected: LexemeClass,
) -> Result<Lexeme> {
    let mut tree = tree.into_rewinder();
    let lexeme = tree.peek();
    let got = LexemeClass::of(lexeme);
    if got.is(&expected) {
        let result = lexeme.unwrap().clone();
        tree.advance_cursor();
        tree.accept();
        Ok(result)
    } else {
        let span = lexeme.map_or_else(
            || Span::new_simple(Pos::default(), Pos::default(), ""),
            |x| x.span,
        );
        Err(Error {
            expected: vec![expected],
            got,
            span,
        })
    }
}

pub fn do_lexeme_classes(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    expected: &[LexemeClass],
) -> Result<(Lexeme, LexemeClass)> {
    let mut tree = tree.into_rewinder();
    let mut error = None;
    for &class in expected {
        let result = do_lexeme_class(&mut tree, class);
        match result {
            Ok(x) => {
                tree.accept();
                return Ok((x, class));
            }
            Err(x) => match &mut error {
                Some(old) => *old |= x,
                old @ None => *old = Some(x),
            },
        }
    }
    Err(error.unwrap())
}

pub fn do_lexeme_group(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    expected: Option<GroupType>,
) -> Result<(Group, Span)> {
    let lexeme = do_lexeme_class(tree, LexemeClass::Group(expected))?;
    match lexeme.body {
        LexemeBody::Group(x) => Ok((x, lexeme.span)),
        _ => unreachable!(),
    }
}

pub fn do_simple_path_segment(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<SimplePathSegment>> {
    let (lexeme, class) = do_lexeme_classes(
        tree,
        &[
            LexemeClass::Identifier,
            LexemeClass::Keyword("super".into()),
            LexemeClass::Keyword("self".into()),
            LexemeClass::Keyword("crate".into()),
            LexemeClass::Punctuation("$".into()),
        ],
    )?;
    let span = lexeme.span;
    match class {
        LexemeClass::Identifier => Ok(Spanned {
            body: SimplePathSegment::Identifier(lexeme.into_text().unwrap()),
            span,
        }),
        LexemeClass::Keyword(x) if x == "super" => Ok(Spanned {
            body: SimplePathSegment::SuperPath,
            span,
        }),
        LexemeClass::Keyword(x) if x == "self" => Ok(Spanned {
            body: SimplePathSegment::SelfPath,
            span,
        }),
        LexemeClass::Keyword(x) if x == "crate" => Ok(Spanned {
            body: SimplePathSegment::CratePath,
            span,
        }),
        LexemeClass::Punctuation(x) if x == "$" => todo!(),
        _ => unreachable!(),
    }
}

pub fn do_simple_path(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<SimplePath>> {
    let mut tree = tree.into_rewinder();
    let (from_root, mut span) =
        match do_lexeme_class(&mut tree, LexemeClass::Punctuation("::".into())) {
            Ok(x) => (true, x.span),
            Err(Error { span, .. }) => (false, span),
        };
    let mut segments = Vec::new();
    loop {
        let lexeme = do_simple_path_segment(&mut tree)?;
        span.end = lexeme.span.end;
        segments.push(lexeme);
        if do_lexeme_class(&mut tree, LexemeClass::Punctuation("::".into())).is_err() {
            break;
        }
    }
    let path = SimplePath {
        from_root,
        segments,
    };
    tree.accept();
    Ok(Spanned { body: path, span })
}

pub fn do_attr_body(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<(Spanned<SimplePath>, Option<AttrInput>)> {
    // TODO: support attr inputs
    let name = do_simple_path(tree)?;
    Ok((name, None))
}

pub fn do_internal_attr(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Attr>> {
    let mut tree = tree.into_rewinder();
    let span_start = do_lexeme_class(&mut tree, LexemeClass::Punctuation("#".into()))?.span;
    do_lexeme_class(&mut tree, LexemeClass::Punctuation("!".into()))?;
    let (group, span_end) = do_lexeme_group(&mut tree, Some(GroupType::Brackets))?;
    let (name, input) = do_attr_body(&mut group.body.into_iter().peekmore())?;
    tree.accept();
    Ok(Spanned {
        body: Attr { name, input },
        span: Span::between(span_start, span_end),
    })
}

pub fn do_external_attr(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Attr>> {
    let mut tree = tree.into_rewinder();
    let span_start = do_lexeme_class(&mut tree, LexemeClass::Punctuation("#".into()))?.span;
    let (group, span_end) = do_lexeme_group(&mut tree, Some(GroupType::Brackets))?;
    let (name, input) = do_attr_body(&mut group.body.into_iter().peekmore())?;
    tree.accept();
    Ok(Spanned {
        body: Attr { name, input },
        span: Span::between(span_start, span_end),
    })
}

pub fn do_visibility(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Visibility>> {
    let mut tree = tree.into_rewinder();
    let span_start = do_lexeme_class(&mut tree, LexemeClass::Keyword("pub".into()))?.span;
    match do_lexeme_group(&mut tree, Some(GroupType::Parens)) {
        Ok((group, span_end)) => {
            let (Lexeme { span, .. }, class) = do_lexeme_classes(
                &mut tree,
                &[
                    LexemeClass::Keyword("crate".into()),
                    LexemeClass::Keyword("self".into()),
                    LexemeClass::Keyword("super".into()),
                    LexemeClass::Keyword("in".into()),
                ],
            )?;
            // TODO: make this more ergonomic
            let path = match class {
                LexemeClass::Keyword(x) => match &*x {
                    "crate" => Spanned {
                        body: SimplePath {
                            from_root: false,
                            segments: vec![Spanned {
                                body: SimplePathSegment::CratePath,
                                span,
                            }],
                        },
                        span,
                    },
                    "self" => Spanned {
                        body: SimplePath {
                            from_root: false,
                            segments: vec![Spanned {
                                body: SimplePathSegment::SelfPath,
                                span,
                            }],
                        },
                        span,
                    },
                    "super" => Spanned {
                        body: SimplePath {
                            from_root: false,
                            segments: vec![Spanned {
                                body: SimplePathSegment::SuperPath,
                                span,
                            }],
                        },
                        span,
                    },
                    "in" => do_simple_path(&mut group.body.into_iter().peekmore())?,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            tree.accept();
            Ok(Spanned {
                body: Visibility::Scoped(path),
                span: Span::between(span_start, span_end),
            })
        }
        Err(_) => {
            tree.accept();
            Ok(Spanned {
                body: Visibility::Pub,
                span: span_start,
            })
        }
    }
}

pub fn do_item_mod(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let _kw_mod = do_lexeme_class(&mut tree, LexemeClass::Keyword("mod".into()))?;
    todo!()
}

pub fn do_item_value_static(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemValue>> {
    let mut tree = tree.into_rewinder();
    let _kw_static = do_lexeme_class(&mut tree, LexemeClass::Keyword("static".into()))?;
    todo!()
}

pub fn do_item_value_const(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemValue>> {
    let mut tree = tree.into_rewinder();
    let _kw_const = do_lexeme_class(&mut tree, LexemeClass::Keyword("const".into()))?;
    todo!()
}

pub fn do_item_value(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let val = match do_item_value_static(&mut tree) {
        Ok(val) => val,
        Err(x) => match do_item_value_const(&mut tree) {
            Ok(val) => val,
            Err(y) => Err(x | y)?,
        },
    };
    let span = val.span; // copy
    Ok(Spanned {
        body: ItemBody::Value(val),
        span,
    })
}

pub fn do_item_extern_crate(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let _kw_extern = do_lexeme_class(&mut tree, LexemeClass::Keyword("extern".into()))?;
    let _kw_crate = do_lexeme_class(&mut tree, LexemeClass::Keyword("crate".into()))?;
    todo!()
}

pub fn do_item_use(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let _kw_use = do_lexeme_class(&mut tree, LexemeClass::Keyword("use".into()))?;
    todo!()
}

pub fn do_user_type_struct(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<UserType>> {
    let mut tree = tree.into_rewinder();
    let _kw_struct = do_lexeme_class(&mut tree, LexemeClass::Keyword("struct".into()))?;
    todo!()
}

pub fn do_user_type_enum(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<UserType>> {
    let mut tree = tree.into_rewinder();
    let _kw_enum = do_lexeme_class(&mut tree, LexemeClass::Keyword("enum".into()))?;
    todo!()
}

pub fn do_item_user_type(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let user_type = match do_user_type_struct(&mut tree) {
        Ok(ty) => ty,
        Err(x) => match do_user_type_enum(&mut tree) {
            Ok(ty) => ty,
            Err(y) => Err(x | y)?,
        },
    };
    let span = user_type.span; // copy
    Ok(Spanned {
        body: ItemBody::UserType(user_type),
        span,
    })
}

pub fn do_compound_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<CompoundBlock>> {
    // TODO: support more than `unsafe` and `loop`
    let mut tree = tree.into_rewinder();
    let (
        Lexeme {
            span: span_start, ..
        },
        class,
    ) = do_lexeme_classes(
        &mut tree,
        &[
            LexemeClass::Keyword("unsafe".into()),
            LexemeClass::Keyword("loop".into()),
        ],
    )?;
    let block = do_block(&mut tree)?;
    let span = Span::between(span_start, block.span);
    tree.accept();
    Ok(Spanned {
        body: match class {
            LexemeClass::Keyword(x) if x == "unsafe" => CompoundBlock::Unsafe(block),
            LexemeClass::Keyword(x) if x == "loop" => CompoundBlock::Loop(block),
            _ => unreachable!(),
        },
        span,
    })
}

pub fn do_statement(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Statement>> {
    match do_lexeme_class(tree, LexemeClass::Punctuation(";".into())) {
        Ok(x) => Ok(Spanned {
            body: Statement::Empty,
            span: x.span,
        }),
        Err(a) => match do_item(tree) {
            // Oh yeah, items are statements
            Ok(x) => {
                let span = x.span;
                Ok(Spanned {
                    body: Statement::ItemDecl(x),
                    span,
                })
            }
            Err(b) => match do_compound_block(tree) {
                Ok(x) => {
                    let span = x.span;
                    Ok(Spanned {
                        body: Statement::Block(x),
                        span,
                    })
                }
                Err(c) => {
                    let mut rewinder = tree.into_rewinder();
                    match do_expression(&mut rewinder) {
                        Ok(x) => {
                            let span = x.span;
                            match do_lexeme_class(
                                &mut rewinder,
                                LexemeClass::Punctuation(Symbol::intern(";")),
                            ) {
                                Ok(lexeme) => {
                                    let span = Span::between(span, lexeme.span);
                                    rewinder.accept();
                                    Ok(Spanned {
                                        span,
                                        body: Statement::DiscardExpr(x),
                                    })
                                }
                                Err(e) => Err(e),
                            }
                        }
                        Err(d) => Err(a | b | c | d),
                    }
                } // TODO: Expr statements
            },
        },
    }
}

pub fn do_literal(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Literal>> {
    // Only handling int and string lits for now
    match do_lexeme_class(tree, LexemeClass::Number) {
        Ok(x) => {
            let span = x.span;
            Ok(Spanned {
                body: Literal {
                    val: Spanned {
                        body: *x.text().unwrap(),
                        span,
                    },
                    lit_kind: LiteralKind::Int(None),
                },
                span,
            })
        }
        Err(a) => match do_string(tree) {
            Ok((str, ty)) => {
                let span = str.span;
                Ok(Spanned {
                    body: Literal {
                        val: str,
                        lit_kind: LiteralKind::String(ty),
                    },
                    span,
                })
            }
            Err(b) => Err(a | b)?, // TODO: Literally every other kind of useful literal
        },
    }
}

pub fn do_primary_expression(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    match do_literal(tree) {
        Ok(x) => {
            let span = x.span;
            Ok(Spanned {
                body: Expr::Literal(x),
                span,
            })
        }
        Err(a) => match do_path(tree) {
            Ok(x) => {
                let span = x.span;
                Ok(Spanned {
                    body: Expr::IdExpr(x),
                    span,
                })
            }
            Err(b) => match do_compound_block(tree) {
                Ok(x) => {
                    let span = x.span;
                    Ok(Spanned {
                        body: Expr::BlockExpr(x),
                        span,
                    })
                }
                Err(c) => Err(a | b | c), // TODO: Literally every other kind of useful expression
            },
        },
    }
}

pub fn do_expression(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let mut lhs = do_primary_expression(tree)?;
    while let Ok(op) = do_lexeme_classes(
        tree,
        &[
            LexemeClass::Group(Some(GroupType::Parens)),
            LexemeClass::Keyword("as".into()),
        ],
    ) {
        // TODO
        match op {
            (
                Lexeme {
                    span,
                    body: LexemeBody::Group(group),
                },
                LexemeClass::Group(Some(GroupType::Parens)),
            ) => {
                let mut args_tree = group.body.into_iter().peekmore();
                let mut args = Vec::new();
                while !args_tree.peek().is_eof() {
                    args.push(do_expression(&mut args_tree)?);
                    match do_lexeme_class(&mut args_tree, LexemeClass::Punctuation(",".into())) {
                        Ok(_) => {}
                        Err(e) => {
                            if !args_tree.peek().is_eof() {
                                Err(e)?
                            }
                        }
                    }
                }
                let new_span = Span::between(lhs.span, span);
                lhs = Spanned {
                    body: Expr::FunctionCall {
                        base: Box::new(lhs),
                        method_name: None,
                        args,
                    },
                    span: new_span,
                };
            }
            (_, LexemeClass::Keyword(x)) if x == "as" => {
                let ty = do_type_no_bounds(tree)?;
                let new_span = Span::between(lhs.span, ty.span);
                lhs = Spanned {
                    body: Expr::AsCast(Box::new(lhs), Box::new(ty)),
                    span: new_span,
                }
            }
            _ => unreachable!(),
        }
    }
    Ok(lhs)
}

pub fn do_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Block>> {
    let (block, span) = do_lexeme_group(tree, Some(GroupType::Braces))?;
    let mut stmts = Vec::new();
    let mut block_tree = block.body.into_iter().peekmore();
    let mut stmt_failure = None;
    while !block_tree.peek().is_eof() {
        match do_statement(&mut block_tree) {
            Ok(stmt) => stmts.push(stmt),
            Err(e) => {
                stmt_failure = Some(e);
                break;
            }
        }
    }
    let tail_expr = if block_tree.peek().is_eof() {
        None
    } else {
        match do_expression(&mut block_tree) {
            Ok(expr) => {
                do_lexeme_class(&mut block_tree, LexemeClass::Eof)?;
                Some(Box::new(expr))
            }
            Err(a) => {
                if let Some(b) = stmt_failure {
                    return Err(a | b);
                } else {
                    return Err(a);
                }
            }
        }
    };
    Ok(Spanned {
        body: Block { stmts, tail_expr },
        span,
    })
}

pub fn do_path_segment(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<PathSegment>> {
    let ident = do_simple_path_segment(tree)?;
    let span = ident.span;
    // TODO: Generics
    // TODO: $crate
    Ok(Spanned {
        body: PathSegment {
            ident,
            generics: None,
        },
        span,
    })
}

pub fn do_path(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Spanned<Path>> {
    // TODO: support QSelf/::
    let mut segments = Vec::new();
    segments.push(do_path_segment(tree)?);
    let mut span = segments[0].span;
    while do_lexeme_class(tree, LexemeClass::Punctuation("::".into())).is_ok() {
        let segment = do_path_segment(tree)?;
        span.end = segment.span.end;
        segments.push(segment);
    }
    Ok(Spanned {
        body: Path {
            root: None, // TODO: support QSelf/::
            segments,
        },
        span,
    })
}

pub fn do_tuple_type(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Type>> {
    let (tuple_group, span) = do_lexeme_group(tree, Some(GroupType::Parens))?;
    let mut tuple_tree = tuple_group.body.into_iter().peekmore();
    let mut tuple = Vec::new();
    while !tuple_tree.peek().is_eof() {
        tuple.push(do_type(&mut tuple_tree)?);
        match do_lexeme_class(&mut tuple_tree, LexemeClass::Punctuation(",".into())) {
            Ok(_) => {}
            Err(e) => {
                if !tuple_tree.peek().is_eof() {
                    Err(e)?
                }
            }
        }
    }
    Ok(Spanned {
        body: Type::Tuple(tuple),
        span,
    })
}

pub fn do_pointer_type(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Type>> {
    let mut tree = tree.into_rewinder();
    let Lexeme {
        span: span_start, ..
    } = do_lexeme_class(&mut tree, LexemeClass::Punctuation("*".into()))?;
    let mutability_kw = do_lexeme_classes(
        &mut tree,
        &[
            LexemeClass::Keyword("mut".into()),
            LexemeClass::Keyword("const".into()),
        ],
    )?;
    let mutability_span = mutability_kw.0.span;
    let mutability = Spanned {
        body: match mutability_kw.1 {
            LexemeClass::Keyword(x) if x == "mut" => Mutability::Mut,
            LexemeClass::Keyword(x) if x == "const" => Mutability::Const,
            _ => unreachable!(),
        },
        span: mutability_span,
    };
    let inner = do_type_no_bounds(&mut tree)?;
    let span = Span::between(span_start, inner.span);
    tree.accept();
    Ok(Spanned {
        body: Type::Pointer(mutability, Box::new(inner)),
        span,
    })
}

pub fn do_type_no_bounds(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Type>> {
    match do_path(tree) {
        Ok(path) => {
            let span = path.span;
            Ok(Spanned {
                body: Type::Path(path),
                span,
            })
        }
        Err(a) => match do_tuple_type(tree) {
            Ok(ty) => Ok(ty),
            Err(b) => match do_lexeme_class(tree, LexemeClass::Punctuation("!".into())) {
                Ok(Lexeme { span, .. }) => Ok(Spanned {
                    body: Type::Never,
                    span,
                }),
                Err(c) => match do_pointer_type(tree) {
                    Ok(ty) => Ok(ty),
                    Err(d) => Err(a | b | c | d)?,
                },
            },
        },
    }
}

pub fn do_type(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Spanned<Type>> {
    // TODO: impl A + B, dyn A + B
    do_type_no_bounds(tree)
}

pub fn do_pattern(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Pattern>> {
    // TODO: anything interesting
    let lexeme = do_lexeme_class(tree, LexemeClass::Identifier)?;
    let span = lexeme.span;
    Ok(Spanned {
        body: Pattern::BareId(Spanned {
            body: lexeme.into_text().unwrap(),
            span,
        }),
        span,
    })
}

pub fn do_param(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Param>> {
    // TODO: handle params without names
    let mut tree = tree.into_rewinder();
    let pat = do_pattern(&mut tree)?;
    do_lexeme_class(&mut tree, LexemeClass::Punctuation(":".into()))?;
    let ty = do_type(&mut tree)?;
    tree.accept();
    let span = Span::between(pat.span, ty.span);
    Ok(Spanned {
        body: Param { pat: Some(pat), ty },
        span,
    })
}

pub fn do_item_fn(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let Lexeme {
        span: span_start, ..
    } = do_lexeme_class(&mut tree, LexemeClass::Keyword("fn".into()))?;
    let name = do_lexeme_class(&mut tree, LexemeClass::Identifier)?;
    let name = Spanned {
        body: *name.text().unwrap(),
        span: name.span,
    };
    let (params_group, _) = do_lexeme_group(&mut tree, Some(GroupType::Parens))?;
    // TODO: receiver
    let mut params_tree = params_group.body.into_iter().peekmore();
    let mut params = Vec::new();
    while !params_tree.peek().is_eof() {
        params.push(do_param(&mut params_tree)?);
        match do_lexeme_class(&mut params_tree, LexemeClass::Punctuation(",".into())) {
            Ok(_) => {}
            Err(e) => {
                if !params_tree.peek().is_eof() {
                    Err(e)?
                }
            }
        }
    }
    let ret_ty = match do_lexeme_class(&mut tree, LexemeClass::Punctuation("->".into())) {
        Ok(_) => Some(do_type(&mut tree)?),
        Err(_) => None,
    };
    let (body, span_end) = match do_lexeme_class(&mut tree, LexemeClass::Punctuation(";".into())) {
        Ok(Lexeme { span, .. }) => (None, span),
        Err(a) => match do_block(&mut tree) {
            Ok(block) => {
                let span = block.span;
                (Some(block), span)
            }
            Err(b) => Err(a | b)?,
        },
    };
    let span = Span::between(span_start, span_end);
    tree.accept();
    Ok(Spanned {
        body: ItemBody::Function(Spanned {
            body: Function {
                safety: None,
                abi: None,
                constness: None,
                is_async: None,
                name,
                generics: None,
                receiver: None, // TODO: parse receiver
                params,
                varargs: None,
                ret_ty,
                body,
            },
            span,
        }),
        span,
    })
}

pub fn do_string(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<(Spanned<Symbol>, StringType)> {
    let full_str = do_lexeme_class(tree, LexemeClass::String)?;
    let str_ty = *if let Lexeme {
        body:
            LexemeBody::Token(Token {
                ty: TokenType::String(str_ty),
                ..
            }),
        ..
    } = &full_str
    {
        str_ty
    } else {
        unreachable!()
    };
    let str = full_str.text().unwrap();
    let str = match str_ty {
        StringType::Default => &str[1..str.len() - 1], // Skip    " and "
        StringType::Byte => &str[2..str.len() - 1],    // Skip   b" and "
        StringType::Raw(x) => {
            let hashes = usize::from(x);
            &str[hashes + 2..str.len() - 1 - hashes] //   Skip  r#" and "#
        }
        StringType::RawByte(x) => {
            let hashes = usize::from(x);
            &str[hashes + 3..str.len() - 1 - hashes] //   Skip rb#" and "#
        }
    };
    let mut parsed = String::new();
    let mut str_iter = str.chars();
    while let Some(c) = str_iter.next() {
        match c {
            '\\' => match str_iter.next() {
                Some('0') => parsed.push('\0'),
                Some('n') => parsed.push('\n'),
                None => todo!("throw an error"),
                Some(x) => todo!("\\{}", x),
            },
            x => parsed.push(x),
        }
    }
    Ok((
        Spanned {
            body: parsed.into(),
            span: full_str.span,
        },
        str_ty,
    ))
}

pub fn do_item_extern_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let Lexeme {
        span: span_start, ..
    } = do_lexeme_class(&mut tree, LexemeClass::Keyword("extern".into()))?;
    let tag = do_string(&mut tree).ok().map(|x| x.0);
    let (block, span_end) = do_lexeme_group(&mut tree, Some(GroupType::Braces))?;
    let mut items = Vec::new();
    let mut block_tree = block.body.into_iter().peekmore();
    while !block_tree.peek().is_eof() {
        items.push(do_item(&mut block_tree)?);
    }
    let span = Span::between(span_start, span_end);
    tree.accept();
    Ok(Spanned {
        body: ItemBody::ExternBlock(Spanned {
            body: ExternBlock { tag, items },
            span,
        }),
        span,
    })
}

pub fn do_item(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Spanned<Item>> {
    let mut tree = tree.into_rewinder();
    let vis = do_visibility(&mut tree).ok();
    let item = match do_item_mod(&mut tree) {
        Ok(body) => body,
        Err(a) => match do_item_value(&mut tree) {
            Ok(body) => body,
            Err(b) => match do_item_extern_crate(&mut tree) {
                Ok(body) => body,
                Err(c) => match do_item_use(&mut tree) {
                    Ok(body) => body,
                    Err(d) => match do_item_user_type(&mut tree) {
                        Ok(body) => body,
                        Err(e) => match do_item_fn(&mut tree) {
                            Ok(body) => body,
                            Err(f) => match do_item_extern_block(&mut tree) {
                                Ok(body) => body,
                                Err(g) => Err(a | b | c | d | e | f | g)?,
                            },
                        },
                    },
                },
            },
        },
    };
    let span = vis.as_ref().map_or(item.span, |Spanned { span, .. }| {
        Span::between(*span, item.span)
    });
    tree.accept();
    Ok(Spanned {
        body: Item {
            vis,
            attrs: vec![],
            item,
        },
        span,
    })
}

pub fn do_mod(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Spanned<Mod>> {
    let mut attrs = Vec::new();
    let mut items = Vec::new();

    let mut external_attrs = Vec::new();
    let mut span = tree.peek().unwrap().span;

    while !tree.peek().is_eof() {
        tree.truncate_iterator_to_cursor();
        match do_internal_attr(tree) {
            Ok(attr) => attrs.push(attr),
            Err(x) => match do_external_attr(tree) {
                Ok(attr) => external_attrs.push(attr),
                Err(y) => match do_item(tree) {
                    Ok(mut item) => {
                        item.attrs.append(&mut external_attrs);
                        items.push(item);
                    }
                    Err(z) => Err(x | y | z)?,
                },
            },
        }
    }

    span = Span::between(span, tree.peek().unwrap().span);

    Ok(Spanned {
        body: Mod { attrs, items },
        span,
    })
}
