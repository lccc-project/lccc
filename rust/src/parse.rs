use core::ops::{BitOr, BitOrAssign, Deref, DerefMut};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    ast::{
        AssociatedTypeBound, Async, AsyncBlock, Attr, AttrInput, Auto, BinaryOp, BindingPattern,
        Block, CaptureSpec, Closure, ClosureParam, CompoundBlock, CondBlock, ConstParam,
        Constructor, ConstructorExpr, EnumVariant, Expr, Extern, ExternBlock, FieldInit, Function,
        GenericArg, GenericArgs, GenericBound, GenericParam, GenericParams, IfBlock, ImplBlock,
        Item, ItemBody, ItemValue, Label, LetStatement, Lifetime, LifetimeParam, Literal,
        LiteralKind, MatchArm, MatchArmValue, MatchBlock, Mod, Param, Path, PathSegment, Pattern,
        Safety, SelfParam, SimplePath, SimplePathSegment, Spanned, Statement, StructCtor,
        StructField, StructKind, TraitDef, TupleCtor, TupleField, Type, TypeParam, UnaryOp,
        UserType, UserTypeBody, Visibility, WhereClause,
    },
    interning::Symbol,
    lex::{
        AstFrag, AstFragClass, CharType, Group, GroupType, IsEof, Keyword, Lexeme, LexemeBody,
        LexemeClass, Punctuation, StringType, Token, TokenType,
    },
    sema::ty::Mutability,
    span::{Pos, Span},
};

use crate::lex::{keyword, punct};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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

pub fn expect(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    expected: &[LexemeClass],
) -> Result<()> {
    // Note: Intentionally forgetting this
    let mut tree = tree.into_rewinder();
    do_lexeme_classes(&mut tree, expected).map(drop)
}

pub fn do_lexeme_class(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    expected: LexemeClass,
) -> Result<Lexeme> {
    let mut tree = tree.into_rewinder();
    let lexeme = tree.peek();

    if expected.matches(lexeme) {
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
            got: LexemeClass::of(lexeme),
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

macro_rules! unwrap_frag {
    ($var:ident) => {
        (|frag| match frag {
            $crate::lex::AstFrag::$var(val) => Ok(val),
            frag => Err((frag, $crate::lex::AstFragClass::$var)),
        })
    };
}

pub fn do_ast_frag<T, F: FnOnce(AstFrag) -> core::result::Result<T, (AstFrag, AstFragClass)>>(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    matcher: F,
) -> Result<Spanned<T>> {
    let mut tree = tree.into_rewinder();
    let mut lexeme = do_lexeme_class(&mut tree, LexemeClass::AstFrag(None))?;
    match lexeme.body {
        LexemeBody::AstFrag(frag) => match matcher(*frag) {
            Ok(val) => Ok(Spanned {
                span: lexeme.span,
                body: val,
            }),
            Err((frag, expected)) => {
                lexeme.body = LexemeBody::AstFrag(Box::new(frag));
                Err(Error {
                    expected: vec![LexemeClass::AstFrag(Some(expected))],
                    got: LexemeClass::of(Some(&lexeme)),
                    span: lexeme.span,
                })
            }
        },
        _ => unreachable!(),
    }
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

pub fn do_lexeme_token(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    expected: LexemeClass,
) -> Result<(Span, Token)> {
    if let LexemeClass::Group(_) = expected {
        panic!("`do_lexeme_token` requires a non-group class")
    }

    let lexeme = do_lexeme_class(tree, expected)?;

    match lexeme.body {
        LexemeBody::Token(tok) => Ok((lexeme.span, tok)),
        _ => unreachable!(),
    }
}

pub fn do_token_classes(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    expected: &[LexemeClass],
) -> Result<(LexemeClass, Span, Token)> {
    let mut tree = tree.into_rewinder();
    let mut error = None;

    for &class in expected {
        match do_lexeme_token(&mut tree, class) {
            Ok((span, token)) => {
                tree.accept();
                return Ok((class, span, token));
            }
            Err(e) => {
                if let Some(err) = error.as_mut() {
                    *err |= e;
                } else {
                    error = Some(e);
                }
            }
        }
    }
    Err(error.unwrap())
}

pub fn do_identifier<I: Iterator<Item = Lexeme>>(
    tree: &mut PeekMoreIterator<I>,
) -> Result<Spanned<Symbol>> {
    let (span, tok) = do_lexeme_token(tree, LexemeClass::Identifier)?;

    Ok(Spanned {
        span,
        body: tok.body,
    })
}

pub fn do_alternation<R, I: Iterator<Item = Lexeme>>(
    tree: &mut PeekMoreIterator<I>,
    alternates: &[fn(&mut PeekMoreIterator<I>) -> Result<Spanned<R>>],
) -> Result<Spanned<R>> {
    let mut tree = tree.into_rewinder();
    let mut err = None;
    for alt in alternates {
        match alt(&mut tree) {
            Ok(val) => {
                tree.accept();
                return Ok(val);
            }
            Err(e) => {
                if let Some(err) = err.as_mut() {
                    *err |= e;
                } else {
                    err = Some(e);
                }
            }
        }
    }
    Err(err.unwrap())
}

pub fn do_simple_path_segment(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<SimplePathSegment>> {
    let (lexeme, class) = do_lexeme_classes(
        tree,
        &[
            LexemeClass::Identifier,
            LexemeClass::Keyword(Keyword::Super),
            LexemeClass::Keyword(Keyword::SelfPat),
            LexemeClass::Keyword(Keyword::Crate),
            LexemeClass::Punctuation(Punctuation::Dollar),
        ],
    )?;
    let span = lexeme.span;
    match class {
        LexemeClass::Identifier => Ok(Spanned {
            body: SimplePathSegment::Identifier(lexeme.into_text().unwrap()),
            span,
        }),
        LexemeClass::Keyword(Keyword::Super) => Ok(Spanned {
            body: SimplePathSegment::SuperPath,
            span,
        }),
        LexemeClass::Keyword(Keyword::SelfPat) => Ok(Spanned {
            body: SimplePathSegment::SelfPath,
            span,
        }),
        LexemeClass::Keyword(Keyword::Crate) => Ok(Spanned {
            body: SimplePathSegment::CratePath,
            span,
        }),
        LexemeClass::Punctuation(Punctuation::Dollar) => todo!(),
        _ => unreachable!(),
    }
}

pub fn do_simple_path(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<SimplePath>> {
    let mut tree = tree.into_rewinder();
    let (from_root, mut span) =
        match do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Path)) {
            Ok(x) => (true, x.span),
            Err(Error { span, .. }) => (false, span),
        };
    let mut segments = Vec::new();
    loop {
        let lexeme = do_simple_path_segment(&mut tree)?;
        span.end = lexeme.span.end;
        segments.push(lexeme);
        if do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Path)).is_err() {
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

    let input = match do_lexeme_classes(
        tree,
        &[
            LexemeClass::Punctuation(Punctuation::Assign),
            LexemeClass::Group(Some(GroupType::Parens)),
            LexemeClass::Eof,
        ],
    )? {
        (_, LexemeClass::Punctuation(_)) => Some(AttrInput::MetaValue(do_literal(tree)?)),
        (group, LexemeClass::Group(_)) => match group.body {
            LexemeBody::Group(g) => Some(AttrInput::DelimTokenTree(Spanned {
                body: g.body,
                span: group.span,
            })),
            _ => unreachable!(),
        },
        (_, _) => None,
    };

    Ok((name, input))
}

pub fn do_internal_attr(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Attr>> {
    let mut tree = tree.into_rewinder();
    let span_start = do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Hash))?.span;
    do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Not))?;
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
    let span_start = do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Hash))?.span;
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
    let span_start = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Pub))?.span;
    match do_lexeme_group(&mut tree, Some(GroupType::Parens)) {
        Ok((group, span_end)) => {
            let (Lexeme { span, .. }, class) = do_lexeme_classes(
                &mut tree,
                &[
                    LexemeClass::Keyword(Keyword::Crate),
                    LexemeClass::Keyword(Keyword::SelfPat),
                    LexemeClass::Keyword(Keyword::Super),
                    LexemeClass::Keyword(Keyword::In),
                ],
            )?;
            // TODO: make this more ergonomic
            let path = match class {
                LexemeClass::Keyword(x) => match x {
                    Keyword::Crate => Spanned {
                        body: SimplePath {
                            from_root: false,
                            segments: vec![Spanned {
                                body: SimplePathSegment::CratePath,
                                span,
                            }],
                        },
                        span,
                    },
                    Keyword::SelfPat => Spanned {
                        body: SimplePath {
                            from_root: false,
                            segments: vec![Spanned {
                                body: SimplePathSegment::SelfPath,
                                span,
                            }],
                        },
                        span,
                    },
                    Keyword::Super => Spanned {
                        body: SimplePath {
                            from_root: false,
                            segments: vec![Spanned {
                                body: SimplePathSegment::SuperPath,
                                span,
                            }],
                        },
                        span,
                    },
                    Keyword::In => do_simple_path(&mut group.body.into_iter().peekmore())?,
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
    let _kw_mod = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Mod))?;
    todo!()
}

pub fn do_item_value_static(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemValue>> {
    let mut tree = tree.into_rewinder();
    let _kw_static = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Static))?;
    todo!()
}

pub fn do_item_value_const(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemValue>> {
    let mut tree = tree.into_rewinder();
    let _kw_const = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Const))?;
    todo!()
}

pub fn do_item_value(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let val = do_alternation(&mut tree, &[do_item_value_static, do_item_value_const])?;
    tree.accept();
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
    let _kw_extern = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Extern))?;
    let _kw_crate = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Crate))?;
    todo!()
}

pub fn do_item_use(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let _kw_use = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Use))?;
    todo!()
}

pub fn do_where_clauses(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Vec<Spanned<WhereClause>>>> {
    let mut tree = tree.into_rewinder();
    let _kw_where = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Where))?;
    todo!()
}

pub fn do_struct_field(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<StructField>> {
    let mut tree = tree.into_rewinder();
    let vis = do_visibility(&mut tree);

    let name = match do_lexeme_token(&mut tree, LexemeClass::Identifier) {
        Ok((span, tok)) => Spanned {
            span,
            body: tok.body,
        },
        Err(e) => match vis {
            Ok(_) => return Err(e),
            Err(d) => return Err(d | e),
        },
    };
    let vis = vis.ok();

    let start_span = vis.as_ref().map(|v| v.span).unwrap_or(name.span);

    do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Colon))?;

    let ty = do_type(&mut tree)?;

    tree.accept();

    let span = Span::between(start_span, ty.span);
    let body = StructField { vis, name, ty };
    println!("{:?}", body);
    Ok(Spanned { span, body })
}

pub fn do_tuple_field(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<TupleField>> {
    let mut tree = tree.into_rewinder();
    let vis = do_visibility(&mut tree);

    let ty = match do_type(&mut tree) {
        Ok(ty) => ty,
        Err(e) => match vis {
            Ok(_) => return Err(e),
            Err(d) => return Err(d | e),
        },
    };

    let vis = vis.ok();

    tree.accept();

    let span = match &vis {
        Some(vis) => Span::between(vis.span, ty.span),
        None => ty.span,
    };

    let body = TupleField { vis, ty };

    Ok(Spanned { span, body })
}

pub fn do_constructor(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Constructor>> {
    let mut tree = tree.into_rewinder();
    match do_lexeme_group(&mut tree, Some(GroupType::Braces)) {
        Ok((group, span)) => {
            let mut inner_tree = group.body.into_iter().peekmore();
            let mut fields = Vec::new();
            loop {
                match do_struct_field(&mut inner_tree) {
                    Ok(field) => fields.push(field),
                    Err(e) => match do_lexeme_class(&mut inner_tree, LexemeClass::Eof) {
                        Ok(_) => break,
                        Err(d) => return Err(e | d),
                    },
                }

                match do_lexeme_classes(
                    &mut inner_tree,
                    &[
                        LexemeClass::Punctuation(Punctuation::Comma),
                        LexemeClass::Eof,
                    ],
                ) {
                    Ok((_, LexemeClass::Eof)) => break,
                    Ok(_) => {
                        continue;
                    }
                    Err(e) => return Err(e),
                }
            }
            tree.accept();
            Ok(Spanned {
                span,
                body: Constructor::Struct(StructCtor { fields }),
            })
        }
        Err(e) => match do_lexeme_group(&mut tree, Some(GroupType::Parens)) {
            Ok((group, span)) => {
                let mut inner_tree = group.body.into_iter().peekmore();
                let mut fields = Vec::new();
                loop {
                    match do_tuple_field(&mut inner_tree) {
                        Ok(field) => fields.push(field),
                        Err(e) => match do_lexeme_class(&mut inner_tree, LexemeClass::Eof) {
                            Ok(_) => break,
                            Err(_) => return Err(e),
                        },
                    }

                    match do_lexeme_classes(
                        &mut inner_tree,
                        &[
                            LexemeClass::Punctuation(Punctuation::Comma),
                            LexemeClass::Eof,
                        ],
                    ) {
                        Ok((_, LexemeClass::Eof)) => break,
                        Ok(_) => continue,
                        Err(e) => return Err(e),
                    }
                }
                tree.accept();
                Ok(Spanned {
                    span,
                    body: Constructor::Tuple(TupleCtor { fields }),
                })
            }
            Err(d) => Err(e | d),
        },
    }
}

pub fn do_user_type_struct(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<UserType>> {
    let mut tree = tree.into_rewinder();
    let struct_kind = match do_lexeme_token(&mut tree, LexemeClass::Keyword(Keyword::Struct)) {
        Ok((span, _)) => Spanned {
            span,
            body: StructKind::Struct,
        },
        Err(e) => match do_lexeme_token(&mut tree, LexemeClass::Keyword(Keyword::Union)) {
            Ok((span, _)) => Spanned {
                span,
                body: StructKind::Union,
            },
            Err(d) => match do_lexeme_token(&mut tree, LexemeClass::Identifier) {
                Ok((span, tok)) if tok.body == "union" => Spanned {
                    span,
                    body: StructKind::Union,
                },
                _ => return Err(e | d),
            },
        },
    };
    let start_span = struct_kind.span;

    let (span, id) = do_lexeme_token(&mut tree, LexemeClass::Identifier)?;

    let name = Spanned {
        body: id.body,
        span,
    };

    let mut end_span = span;

    let generics = do_generic_params(&mut tree).ok();

    if let Some(generics) = &generics {
        end_span = generics.span;
    }

    let where_clauses = do_where_clauses(&mut tree).ok();

    if let Some(where_clauses) = &where_clauses {
        end_span = where_clauses.span;
    }

    let ctor = match do_constructor(&mut tree) {
        Ok(ctor) => match &ctor.body {
            Constructor::Tuple(_) => {
                do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Semi))?;
                ctor
            }
            _ => ctor,
        },
        Err(e) => match do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Semi)) {
            Ok(_) => Spanned {
                body: Constructor::Unit,
                span: end_span.after(),
            },
            Err(d) => return Err(d | e),
        },
    };

    let span = Span::between(start_span, ctor.span);

    let body = UserType {
        name,
        generics,
        where_clauses,
        body: UserTypeBody::Struct(struct_kind, ctor),
    };

    tree.accept();

    Ok(Spanned { span, body })
}

pub fn do_user_type_enum(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<UserType>> {
    let mut tree = tree.into_rewinder();
    let kw_enum = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Enum))?;
    let start_span = kw_enum.span;

    let (span, id) = do_lexeme_token(&mut tree, LexemeClass::Identifier)?;

    let name = Spanned {
        body: id.body,
        span,
    };

    let generics = do_generic_params(&mut tree).ok();

    let where_clauses = do_where_clauses(&mut tree).ok();

    let (gr, endspan) = do_lexeme_group(&mut tree, Some(GroupType::Braces))?;

    let mut inner_tree = gr.body.into_iter().peekmore();

    let mut variants = Vec::new();
    loop {
        if do_lexeme_class(&mut inner_tree, LexemeClass::Eof).is_ok() {
            break;
        }
        let mut attrs = Vec::new();
        loop {
            match do_external_attr(&mut inner_tree) {
                Ok(attr) => attrs.push(attr),
                Err(_) => break,
            }
        }

        let (startspan, var_id) = do_lexeme_token(&mut inner_tree, LexemeClass::Identifier)?;

        // FIXME: What is this used for?
        let var_name = Spanned {
            body: var_id.body,
            span: startspan,
        };

        let ctor = do_constructor(&mut inner_tree).unwrap_or_else(|_| Spanned {
            body: Constructor::Unit,
            span: startspan.after(),
        });

        let (endspan, discriminant) = match do_lexeme_token(&mut inner_tree, punct![=]) {
            Ok(_) => {
                let expr = do_expression(&mut inner_tree)?;

                (expr.span, Some(expr))
            }
            Err(_) => (startspan, None),
        };

        let body = EnumVariant {
            attrs,
            ctor,
            discriminant,
        };

        let span = Span::between(startspan, endspan);
        variants.push(Spanned { body, span });

        match do_lexeme_classes(&mut inner_tree, &[LexemeClass::Eof, punct!(,)])? {
            (_, LexemeClass::Eof) => break,
            (_, punct!(,)) => continue,
            _ => unreachable!(),
        }
    }

    let body = UserTypeBody::Enum(variants);

    let span = Span::between(start_span, endspan);

    let utype = UserType {
        name,
        generics,
        where_clauses,
        body,
    };

    tree.accept();

    Ok(Spanned { body: utype, span })
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
    tree.accept();
    Ok(Spanned {
        body: ItemBody::UserType(user_type),
        span,
    })
}

pub fn do_if_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<IfBlock>> {
    let mut tree = tree.into_rewinder();
    let span_start = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::If))?.span;
    let cond = Box::new(do_expression_without_constructor(&mut tree)?);
    let block = do_block(&mut tree)?;
    let mut span_end = block.span;
    let mut elseifs = Vec::new();
    let mut elseblock = None;
    while let Ok(Lexeme { .. }) = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Else)) {
        match do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::If)) {
            Ok(Lexeme { span: if_span, .. }) => {
                let cond = Box::new(do_expression_without_constructor(&mut tree)?);
                let block = do_block(&mut tree)?;
                span_end = block.span;
                let span = Span::between(if_span, block.span);
                elseifs.push(Spanned {
                    body: CondBlock { cond, block },
                    span,
                });
            }
            Err(_) => {
                let block = do_block(&mut tree)?;
                span_end = block.span;
                elseblock = Some(block);
                break;
            }
        }
    }
    tree.accept();
    let span = Span::between(span_start, span_end);
    Ok(Spanned {
        body: IfBlock {
            cond,
            block,
            elseifs,
            elseblock,
        },
        span,
    })
}

pub fn do_match(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<MatchBlock>> {
    let mut tree = tree.into_rewinder();
    let Lexeme {
        span: start_span, ..
    } = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Match))?;

    let discriminee = Box::new(do_expression_without_constructor(&mut tree)?);

    let (inner, gspan) = do_lexeme_group(&mut tree, Some(GroupType::Braces))?;

    let mut inner_tree = inner.body.into_iter().peekmore();

    let mut arms = Vec::new();

    loop {
        if do_lexeme_class(&mut inner_tree, LexemeClass::Eof).is_ok() {
            break;
        }
        let discrim = do_pattern(&mut inner_tree)?;

        let guard = match do_lexeme_class(&mut inner_tree, LexemeClass::Keyword(Keyword::If)) {
            Ok(_) => Some(do_expression(&mut inner_tree)?),
            Err(_) => None,
        };

        do_lexeme_class(&mut inner_tree, punct!(=>))?;

        match do_block(&mut inner_tree) {
            Ok(block) => {
                let span = Span::between(discrim.span, block.span);
                let _ = do_lexeme_class(&mut inner_tree, punct!(,));
                let bspan = block.span;
                let value = block.map_span(|blk| {
                    MatchArmValue::Block(Spanned {
                        body: blk,
                        span: bspan,
                    })
                });
                arms.push(Spanned {
                    span,
                    body: MatchArm {
                        discrim,
                        guard,
                        value,
                    },
                });
            }
            Err(e) => match do_expression(&mut inner_tree) {
                Ok(expr) => {
                    let span = Span::between(discrim.span, expr.span);
                    let espan = expr.span;
                    let value = expr.map_span(|block| {
                        MatchArmValue::Expr(Spanned {
                            body: block,
                            span: espan,
                        })
                    });
                    arms.push(Spanned {
                        span,
                        body: MatchArm {
                            discrim,
                            guard,
                            value,
                        },
                    });

                    match do_lexeme_classes(&mut inner_tree, &[punct!(,), LexemeClass::Eof]) {
                        Ok((_, punct!(,))) => continue,
                        Ok((_, LexemeClass::Eof)) => break,
                        Ok((_, _)) => unreachable!(),
                        Err(e) => return Err(e),
                    }
                }
                Err(d) => return Err(e | d),
            },
        }
    }

    let span = Span::between(start_span, gspan);

    let body = MatchBlock { discriminee, arms };
    tree.accept();
    Ok(Spanned { body, span })
}

pub fn do_compound_block_if(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<CompoundBlock>> {
    let if_block = do_if_block(tree)?;

    Ok(Spanned {
        span: if_block.span,
        body: CompoundBlock::If(if_block),
    })
}

pub fn do_compound_block_match(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<CompoundBlock>> {
    let match_block = do_match(tree)?;

    Ok(Spanned {
        span: match_block.span,
        body: CompoundBlock::Match(match_block),
    })
}

pub fn do_keyword_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<CompoundBlock>> {
    let mut tree = tree.into_rewinder();
    let (
        Lexeme {
            span: span_start, ..
        },
        class,
    ) = do_lexeme_classes(
        &mut tree,
        &[
            LexemeClass::Keyword(Keyword::Unsafe),
            LexemeClass::Keyword(Keyword::Loop),
        ],
    )?;
    let block = do_block(&mut tree)?;
    let span = Span::between(span_start, block.span);
    tree.accept();
    Ok(Spanned {
        body: match class {
            LexemeClass::Keyword(Keyword::Unsafe) => CompoundBlock::Unsafe(block),
            LexemeClass::Keyword(Keyword::Loop) => CompoundBlock::Loop(block),
            _ => unreachable!(),
        },
        span,
    })
}

pub fn do_simple_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<CompoundBlock>> {
    let block = do_block(tree)?;
    let span = block.span;
    Ok(Spanned {
        body: CompoundBlock::SimpleBlock(block),
        span,
    })
}

pub fn do_compound_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<CompoundBlock>> {
    do_alternation(
        tree,
        &[
            do_compound_block_if,
            do_compound_block_match,
            do_keyword_block,
            do_simple_block,
        ],
    )
}

pub fn do_let_statement(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<LetStatement>> {
    let mut tree = tree.into_rewinder();
    let span_start = do_lexeme_class(&mut tree, LexemeClass::Keyword(Keyword::Let))?.span;
    let name = do_pattern(&mut tree)?;
    let ty = match do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Colon)) {
        Ok(_) => Some(do_type(&mut tree)?),
        Err(_) => None,
    };
    let val = match do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Assign)) {
        Ok(_) => Some(do_expression(&mut tree)?),
        Err(_) => None,
    };
    // TODO: let-else
    let span_end = do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Semi))?.span;
    tree.accept();
    Ok(Spanned {
        body: LetStatement {
            name,
            ty,
            val,
            else_block: None,
        },
        span: Span::between(span_start, span_end),
    })
}

pub fn do_statement(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Statement>> {
    match do_lexeme_class(tree, LexemeClass::Punctuation(Punctuation::Semi)) {
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
                Err(c) => match do_let_statement(tree) {
                    Ok(x) => {
                        let span = x.span;
                        Ok(Spanned {
                            body: Statement::LetStatement(x),
                            span,
                        })
                    }
                    Err(d) => {
                        let mut rewinder = tree.into_rewinder();
                        match do_expression(&mut rewinder) {
                            Ok(x) => {
                                let span = x.span;
                                match do_lexeme_class(
                                    &mut rewinder,
                                    LexemeClass::Punctuation(Punctuation::Semi),
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
                            Err(e) => Err(a | b | c | d | e),
                        }
                    }
                },
            },
        },
    }
}

pub fn do_literal(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Literal>> {
    // Only handling int, string, and char lits for now
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
            Err(b) => match do_char(tree) {
                Ok((ch, ty)) => {
                    let span = ch.span;
                    Ok(Spanned {
                        body: Literal {
                            val: ch,
                            lit_kind: LiteralKind::Char(ty),
                        },
                        span,
                    })
                }
                Err(c) => Err(a | b | c)?, // TODO: Literally every other kind of useful literal
            },
        },
    }
}

pub fn do_literal_expr(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let lit = do_literal(tree)?;

    let span = lit.span;

    Ok(Spanned {
        span,
        body: Expr::Literal(lit),
    })
}

pub fn do_id_with_ctor_expr<const ALLOW_CONSTRUCTOR: bool>(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let mut tree = tree.into_rewinder();
    let path = do_path(&mut tree)?;

    let mut fullspan = path.span;
    if ALLOW_CONSTRUCTOR {
        match do_lexeme_group(&mut tree, Some(GroupType::Braces)) {
            Ok((g, span)) => {
                fullspan = Span::between(fullspan, span);
                let mut ctor = ConstructorExpr {
                    ctor_name: path,
                    fields: vec![],
                    fill: None,
                };

                let mut inner_tree = g.body.into_iter().peekmore();

                loop {
                    match do_token_classes(
                        &mut inner_tree,
                        &[
                            LexemeClass::Identifier,
                            LexemeClass::Number,
                            LexemeClass::Eof,
                            LexemeClass::Punctuation(Punctuation::DotDot),
                        ],
                    )? {
                        (LexemeClass::Identifier, mut span, tok) => {
                            let name = Spanned {
                                span,
                                body: tok.body,
                            };

                            match do_lexeme_class(
                                &mut inner_tree,
                                LexemeClass::Punctuation(Punctuation::Colon),
                            ) {
                                Ok(_) => {
                                    let val = do_expression(&mut inner_tree)?;
                                    span = Span::between(span, val.span);
                                    match do_lexeme_classes(
                                        &mut inner_tree,
                                        &[
                                            LexemeClass::Punctuation(Punctuation::Comma),
                                            LexemeClass::Eof,
                                        ],
                                    ) {
                                        Ok((_, LexemeClass::Eof)) => {
                                            ctor.fields.push(Spanned {
                                                span,
                                                body: FieldInit {
                                                    name,
                                                    val: Some(val),
                                                },
                                            });
                                            break;
                                        }
                                        Ok((_, LexemeClass::Punctuation(_))) => {
                                            ctor.fields.push(Spanned {
                                                span,
                                                body: FieldInit {
                                                    name,
                                                    val: Some(val),
                                                },
                                            });
                                            continue;
                                        }
                                        Ok(_) => unreachable!(),
                                        Err(e) => return Err(e),
                                    }
                                }
                                Err(e) => match do_lexeme_classes(
                                    &mut inner_tree,
                                    &[
                                        LexemeClass::Punctuation(Punctuation::Comma),
                                        LexemeClass::Eof,
                                    ],
                                ) {
                                    Ok((_, LexemeClass::Eof)) => {
                                        ctor.fields.push(Spanned {
                                            span,
                                            body: FieldInit { name, val: None },
                                        });
                                        break;
                                    }
                                    Ok((_, LexemeClass::Punctuation(_))) => {
                                        ctor.fields.push(Spanned {
                                            span,
                                            body: FieldInit { name, val: None },
                                        });
                                        continue;
                                    }
                                    Ok(_) => unreachable!(),
                                    Err(d) => return Err(e | d),
                                },
                            }
                        }
                        (LexemeClass::Number, mut span, tok) => {
                            let name = Spanned {
                                span,
                                body: tok.body,
                            };

                            do_lexeme_class(
                                &mut inner_tree,
                                LexemeClass::Punctuation(Punctuation::Colon),
                            )?;
                            let val = do_expression(&mut inner_tree)?;
                            span = Span::between(span, val.span);
                            match do_lexeme_classes(
                                &mut inner_tree,
                                &[
                                    LexemeClass::Punctuation(Punctuation::Comma),
                                    LexemeClass::Eof,
                                ],
                            ) {
                                Ok((_, LexemeClass::Eof)) => {
                                    ctor.fields.push(Spanned {
                                        span,
                                        body: FieldInit {
                                            name,
                                            val: Some(val),
                                        },
                                    });
                                    break;
                                }
                                Ok((_, LexemeClass::Punctuation(_))) => {
                                    ctor.fields.push(Spanned {
                                        span,
                                        body: FieldInit {
                                            name,
                                            val: Some(val),
                                        },
                                    });
                                    continue;
                                }
                                Ok(_) => unreachable!(),
                                Err(e) => return Err(e),
                            }
                        }
                        (LexemeClass::Punctuation(_), _, _) => {
                            let val = do_expression(&mut inner_tree)?;

                            ctor.fill = Some(Box::new(val));

                            do_lexeme_class(&mut inner_tree, LexemeClass::Eof)?;
                            break;
                        }
                        (LexemeClass::Eof, _, _) => break,
                        _ => unreachable!(),
                    }
                }
                tree.accept();
                Ok(Spanned {
                    body: Expr::Constructor(Spanned {
                        body: ctor,
                        span: fullspan,
                    }),
                    span: fullspan,
                })
            }
            Err(_) => {
                tree.accept();
                Ok(Spanned {
                    body: Expr::IdExpr(path),
                    span: fullspan,
                })
            }
        }
    } else {
        tree.accept();
        Ok(Spanned {
            body: Expr::IdExpr(path),
            span: fullspan,
        })
    }
}

pub fn do_block_expr(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let block = do_compound_block(tree)?;
    let span = block.span;
    Ok(Spanned {
        body: Expr::BlockExpr(block),
        span,
    })
}

pub fn do_group_or_tuple_expr(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let (group, span) = do_lexeme_group(tree, Some(GroupType::Parens))?;

    let mut tree = group.body.into_iter().peekmore();

    match do_expression(&mut tree) {
        Ok(expr) => match do_lexeme_token(&mut tree, punct![,]) {
            Ok(_) => {
                let mut tuple = vec![expr];
                loop {
                    match do_expression(&mut tree) {
                        Ok(expr) => tuple.push(expr),
                        Err(e) => match do_lexeme_class(&mut tree, LexemeClass::Eof) {
                            Ok(_) => break,
                            Err(d) => return Err(e | d),
                        },
                    }

                    match do_lexeme_classes(&mut tree, &[punct![,], LexemeClass::Eof]) {
                        Ok((_, LexemeClass::Punctuation(_))) => continue,
                        Ok((_, LexemeClass::Eof)) => break,
                        Ok(_) => unreachable!(),
                        Err(e) => return Err(e),
                    }
                }
                Ok(Spanned {
                    body: Expr::Tuple(tuple),
                    span,
                })
            }
            Err(e) => match do_lexeme_class(&mut tree, LexemeClass::Eof) {
                Ok(_) => Ok(Spanned {
                    body: Expr::Group(Box::new(expr)),
                    span,
                }),
                Err(d) => Err(e | d),
            },
        },
        Err(e) => match do_lexeme_class(&mut tree, LexemeClass::Eof) {
            Ok(_) => Ok(Spanned {
                body: Expr::Tuple(vec![]),
                span,
            }),
            Err(d) => Err(e | d),
        },
    }
}

pub fn do_control_flow_expr<const ALLOW_CONSTRUCTOR: bool>(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let mut tree = tree.into_rewinder();
    match do_token_classes(
        &mut tree,
        &[
            keyword!(break),
            keyword!(continue),
            keyword!(return),
            keyword!(do),
            keyword!(yield),
        ],
    )? {
        (keyword!(return), mut span, _) => {
            let expr = match do_expression_maybe_constructor::<ALLOW_CONSTRUCTOR>(&mut tree) {
                Ok(expr) => {
                    span = Span::between(span, expr.span);

                    Some(Box::new(expr))
                }
                Err(_) => None,
            };

            Ok(Spanned {
                body: Expr::Return(expr),
                span,
            })
        }
        (keyword!(yield), mut span, _) => {
            let expr = match do_expression_maybe_constructor::<ALLOW_CONSTRUCTOR>(&mut tree) {
                Ok(expr) => {
                    span = Span::between(span, expr.span);

                    Some(Box::new(expr))
                }
                Err(_) => None,
            };

            Ok(Spanned {
                body: Expr::Yield(expr),
                span,
            })
        }
        (keyword!(break), mut span, _) => {
            let label = match do_lexeme_token(&mut tree, LexemeClass::Lifetime) {
                Ok((tspan, tok)) => {
                    span = Span::between(span, tspan);
                    if tok.body == "static" || tok.body == "_" {
                        let kw = match &*tok.body {
                            "static" => keyword!('static),
                            "_" => keyword!('_),
                            _ => unreachable!(),
                        };
                        return Err(Error {
                            expected: vec![LexemeClass::Lifetime],
                            got: kw,
                            span: tspan,
                        });
                    }
                    Some(Spanned {
                        body: Label {
                            name: Spanned {
                                body: tok.body,
                                span: tspan,
                            },
                        },
                        span: tspan,
                    })
                }
                Err(_) => None,
            };

            let expr = match do_expression_maybe_constructor::<ALLOW_CONSTRUCTOR>(&mut tree) {
                Ok(expr) => {
                    span = Span::between(span, expr.span);

                    Some(Box::new(expr))
                }
                Err(_) => None,
            };

            Ok(Spanned {
                body: Expr::Break(label, expr),
                span,
            })
        }
        (keyword!(continue), mut span, _) => {
            let label = match do_lexeme_token(&mut tree, LexemeClass::Lifetime) {
                Ok((tspan, tok)) => {
                    span = Span::between(span, tspan);
                    if tok.body == "static" || tok.body == "_" {
                        let kw = match &*tok.body {
                            "static" => keyword!('static),
                            "_" => keyword!('_),
                            _ => unreachable!(),
                        };
                        return Err(Error {
                            expected: vec![LexemeClass::Lifetime],
                            got: kw,
                            span: tspan,
                        });
                    }
                    Some(Spanned {
                        body: Label {
                            name: Spanned {
                                body: tok.body,
                                span: tspan,
                            },
                        },
                        span: tspan,
                    })
                }
                Err(_) => None,
            };

            Ok(Spanned {
                body: Expr::Continue(label),
                span,
            })
        }
        (keyword!(do), mut span, _) => {
            match do_lexeme_token(&mut tree, LexemeClass::Identifier) {
                Ok((ispan, tok)) if tok.body == "yeet" => {
                    span = Span::between(span, ispan);
                }
                Ok((span, _)) => {
                    return Err(Error {
                        expected: vec![keyword!(yeet)],
                        got: LexemeClass::Identifier,
                        span,
                    });
                }
                Err(mut e) => {
                    e.expected = vec![keyword!(yeet)];
                    return Err(e);
                }
            }

            let expr = match do_expression_maybe_constructor::<ALLOW_CONSTRUCTOR>(&mut tree) {
                Ok(expr) => {
                    span = Span::between(span, expr.span);

                    Some(Box::new(expr))
                }
                Err(_) => None,
            };

            Ok(Spanned {
                body: Expr::Yeet(expr),
                span,
            })
        }
        _ => unreachable!(),
    }
}

pub fn do_async_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let mut tree = tree.into_rewinder();
    let span = do_lexeme_token(&mut tree, keyword!(async))?.0;

    let capture_rule = match do_lexeme_token(&mut tree, keyword!(move)) {
        Ok((span, _)) => Some(Spanned {
            body: CaptureSpec::Move,
            span,
        }),
        Err(_) => None,
    };

    let block = do_block(&mut tree)?;
    tree.accept();

    let block_span = capture_rule
        .as_ref()
        .map(|c| c.span)
        .map_or(block.span, |s| Span::between(s, block.span));

    let block = Spanned {
        body: AsyncBlock {
            capture_rule,
            block,
        },
        span: block_span,
    };

    let span = Span::between(span, block.span);

    Ok(Spanned {
        body: Expr::AsyncBlock(block),
        span,
    })
}

pub fn do_const_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let mut tree = tree.into_rewinder();
    let span = do_lexeme_token(&mut tree, keyword!(const))?.0;

    let block = do_block(&mut tree)?;

    let span = Span::between(span, block.span);

    Ok(Spanned {
        body: Expr::ConstBlock(block),
        span,
    })
}

pub fn do_closure<const ALLOW_CONSTRUCTOR: bool>(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let mut tree = tree.into_rewinder();
    let (startspan, capture_rule) = match do_lexeme_token(&mut tree, keyword!(move)) {
        Ok((span, _)) => (
            span,
            Some(Spanned {
                body: CaptureSpec::Move,
                span,
            }),
        ),
        Err(e) => (e.span, None),
    };

    let params = match do_token_classes(&mut tree, &[punct!(||), punct!(|)])? {
        (punct!(||), span, _) => Spanned { body: vec![], span },
        (_, mut span, _) => {
            let mut params = Vec::new();

            loop {
                match do_lexeme_token(&mut tree, punct!(|)) {
                    Ok((ispan, _)) => {
                        span = Span::between(span, ispan);
                        break;
                    }
                    Err(_) => {}
                }

                let pat = do_pattern_param(&mut tree)?; // No | patterns
                let mut param_span = pat.span;
                let ty = match do_lexeme_token(&mut tree, punct!(:)) {
                    Ok(_) => {
                        let ty = do_type(&mut tree)?;

                        param_span = Span::between(param_span, ty.span);
                        Some(ty)
                    }
                    Err(_) => None,
                };

                params.push(Spanned {
                    body: ClosureParam { pat, ty },
                    span: param_span,
                });

                match do_token_classes(&mut tree, &[punct!(,), punct!(|)])? {
                    (punct!(,), _, _) => continue,
                    (_, ispan, _) => {
                        span = Span::between(span, ispan);
                        break;
                    }
                }
            }

            Spanned { body: params, span }
        }
    };

    let retty = match do_lexeme_token(&mut tree, punct!(->)) {
        Ok(_) => Some(do_type(&mut tree)?),
        Err(_) => None,
    };

    let body = Box::new(do_expression_maybe_constructor::<ALLOW_CONSTRUCTOR>(
        &mut tree,
    )?);

    let span = Span::between(startspan, body.span);
    tree.accept();

    let closure = Closure {
        capture_rule,
        params,
        retty,
        body,
    };

    Ok(Spanned {
        body: Expr::Closure(Spanned {
            body: closure,
            span,
        }),
        span,
    })
}

pub fn do_array_expr(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let (group, span) = do_lexeme_group(tree, Some(GroupType::Brackets))?;

    let mut tree = group.body.into_iter().peekmore();

    match do_expression(&mut tree) {
        Ok(expr) => {
            match do_lexeme_classes(&mut tree, &[punct!(,), punct!(;), LexemeClass::Eof])? {
                (_, punct!(,)) => {
                    let mut array = vec![expr];

                    loop {
                        match do_expression(&mut tree) {
                            Ok(expr) => {
                                array.push(expr);
                            }
                            Err(e) => match do_lexeme_class(&mut tree, LexemeClass::Eof) {
                                Ok(_) => break,
                                Err(d) => return Err(e | d),
                            },
                        }

                        match do_lexeme_classes(&mut tree, &[punct!(,), LexemeClass::Eof])? {
                            (_, LexemeClass::Eof) => break,
                            (_, LexemeClass::Punctuation(_)) => continue,
                            _ => unreachable!(),
                        }
                    }
                    Ok(Spanned {
                        body: Expr::Array(array),
                        span,
                    })
                }
                (_, punct!(;)) => {
                    let base = Box::new(expr);
                    let len = Box::new(do_expression(&mut tree)?);

                    Ok(Spanned {
                        body: Expr::ArrayRepeat { base, len },
                        span,
                    })
                }
                (_, LexemeClass::Eof) => Ok(Spanned {
                    body: Expr::Array(vec![expr]),
                    span,
                }),
                _ => unreachable!(),
            }
        }
        Err(e) => match do_lexeme_class(&mut tree, LexemeClass::Eof) {
            Ok(_) => Ok(Spanned {
                body: Expr::Array(vec![]),
                span,
            }),
            Err(d) => Err(e | d),
        },
    }
}

pub fn do_frag_expr(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let expr = do_ast_frag(tree, unwrap_frag!(Expr))?;

    let span = expr.span;

    Ok(Spanned {
        span,
        body: Expr::Frag(Box::new(expr)),
    })
}

pub fn do_primary_expression<const ALLOW_CONSTRUCTOR: bool>(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    do_alternation(
        tree,
        &[
            do_literal_expr,
            do_id_with_ctor_expr::<ALLOW_CONSTRUCTOR>,
            do_block_expr,
            do_group_or_tuple_expr,
            do_control_flow_expr::<ALLOW_CONSTRUCTOR>,
            do_async_block,
            do_const_block,
            do_closure::<ALLOW_CONSTRUCTOR>,
            do_array_expr,
            do_frag_expr,
        ],
    )
}

pub fn do_suffix_expression<const ALLOW_CONSTRUCTOR: bool>(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let mut tree = tree.into_rewinder();
    let mut lhs = do_primary_expression::<ALLOW_CONSTRUCTOR>(&mut tree)?;
    while let Ok(op) = do_lexeme_classes(
        &mut tree,
        &[
            LexemeClass::Group(Some(GroupType::Parens)),
            LexemeClass::Group(Some(GroupType::Brackets)),
            punct!(.),
            punct!(?),
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
                    match do_lexeme_class(&mut args_tree, punct!(,)) {
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
            (
                Lexeme {
                    span,
                    body: LexemeBody::Group(group),
                },
                LexemeClass::Group(Some(GroupType::Brackets)),
            ) => {
                let mut inner_tree = group.body.into_iter().peekmore();

                let index = do_expression(&mut inner_tree)?;

                do_lexeme_class(&mut inner_tree, LexemeClass::Eof)?;

                let new_span = Span::between(lhs.span, span);

                lhs = Spanned {
                    body: Expr::Index {
                        base: Box::new(lhs),
                        index: Box::new(index),
                    },
                    span: new_span,
                };
            }
            (_, punct!(.)) => {
                match do_token_classes(
                    &mut tree,
                    &[
                        LexemeClass::Identifier,
                        LexemeClass::Number,
                        keyword!(await),
                    ],
                )? {
                    (LexemeClass::Identifier, span, tok) => {
                        let name = Spanned {
                            body: tok.body,
                            span,
                        };

                        match do_lexeme_group(&mut tree, Some(GroupType::Parens)) {
                            Ok((group, gspan)) => {
                                let mut args_tree = group.body.into_iter().peekmore();
                                let mut args = Vec::new();
                                while !args_tree.peek().is_eof() {
                                    args.push(do_expression(&mut args_tree)?);
                                    match do_lexeme_class(&mut args_tree, punct!(,)) {
                                        Ok(_) => {}
                                        Err(e) => {
                                            if !args_tree.peek().is_eof() {
                                                Err(e)?
                                            }
                                        }
                                    }
                                }
                                let new_span = Span::between(lhs.span, gspan);
                                lhs = Spanned {
                                    body: Expr::FunctionCall {
                                        base: Box::new(lhs),
                                        method_name: Some(name),
                                        args,
                                    },
                                    span: new_span,
                                };
                            }
                            Err(_) => {
                                let new_span = Span::between(lhs.span, span);
                                lhs = Spanned {
                                    body: Expr::MemberAccess(Box::new(lhs), name),
                                    span: new_span,
                                }
                            }
                        }
                    }
                    (LexemeClass::Number, span, tok) => {
                        let name = Spanned {
                            body: tok.body,
                            span,
                        };
                        let new_span = Span::between(lhs.span, span);
                        lhs = Spanned {
                            body: Expr::MemberAccess(Box::new(lhs), name),
                            span: new_span,
                        }
                    }
                    (keyword!(await), span, _) => {
                        let new_span = Span::between(lhs.span, span);
                        lhs = Spanned {
                            body: Expr::Await(Box::new(lhs)),
                            span: new_span,
                        };
                    }
                    _ => unreachable!(),
                }
            }
            (lexeme, punct!(?)) => {
                let new_span = Span::between(lhs.span, lexeme.span);
                lhs = Spanned {
                    body: Expr::Await(Box::new(lhs)),
                    span: new_span,
                };
            }
            _ => unreachable!(),
        }
    }
    tree.accept();
    Ok(lhs)
}

pub fn do_raw_ref(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Mutability>> {
    let mut tree = tree.into_rewinder();

    match do_lexeme_token(&mut tree, LexemeClass::Identifier) {
        Ok((_, tok)) if tok.body == "raw" => {
            match do_token_classes(&mut tree, &[keyword!(mut), keyword!(const)]) {
                Ok((keyword!(mut), span, _)) => {
                    tree.accept();
                    Ok(Spanned {
                        span,
                        body: Mutability::Mut,
                    })
                }
                Ok((keyword!(const), span, _)) => {
                    tree.accept();
                    Ok(Spanned {
                        span,
                        body: Mutability::Const,
                    })
                }
                Ok(_) => unreachable!(),
                Err(e) => Err(e),
            }
        }
        Ok((span, _)) => Err(Error {
            expected: vec![keyword!(raw)],
            got: LexemeClass::Identifier,
            span,
        }),
        Err(mut e) => {
            e.expected = vec![keyword!(raw)];
            Err(e)
        }
    }
}

pub fn do_unary_expression<const ALLOW_CONSTRUCTOR: bool>(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let mut tree = tree.into_rewinder();
    match do_token_classes(
        &mut tree,
        &[
            punct!(&),
            punct!(&&),
            punct!(*),
            punct!(-),
            punct!(!),
            punct!(..),
            punct!(..=),
        ],
    ) {
        Ok((LexemeClass::Punctuation(x), outerspan, _)) => match x {
            Punctuation::BitAnd | Punctuation::BoolAnd => {
                let base = match do_lexeme_token(&mut tree, keyword!(mut)) {
                    Ok((span, _)) => {
                        let mt = Spanned {
                            body: Mutability::Mut,
                            span,
                        };
                        let ref_span = Span::between(outerspan, mt.span);
                        let inner = do_unary_expression::<ALLOW_CONSTRUCTOR>(&mut tree)?;
                        let span = Span::between(outerspan, inner.span);
                        let expr = Expr::UnaryExpr(
                            Spanned {
                                body: UnaryOp::AddrOf(Some(mt)),
                                span: ref_span,
                            },
                            Box::new(inner),
                        );

                        Ok(Spanned { body: expr, span })
                    }
                    Err(e) => match do_raw_ref(&mut tree) {
                        Ok(mt) => {
                            let raw_ref_span = Span::between(outerspan, mt.span);
                            let inner = do_unary_expression::<ALLOW_CONSTRUCTOR>(&mut tree)?;
                            let span = Span::between(outerspan, inner.span);
                            let expr = Expr::UnaryExpr(
                                Spanned {
                                    body: UnaryOp::RawAddrOf(mt),
                                    span: raw_ref_span,
                                },
                                Box::new(inner),
                            );

                            Ok(Spanned { body: expr, span })
                        }
                        Err(d) => {
                            let inner = match do_unary_expression::<ALLOW_CONSTRUCTOR>(&mut tree) {
                                Ok(expr) => expr,
                                Err(f) => return Err(e | d | f),
                            };
                            let span = Span::between(outerspan, inner.span);
                            let expr = Expr::UnaryExpr(
                                Spanned {
                                    body: UnaryOp::AddrOf(None),
                                    span: outerspan,
                                },
                                Box::new(inner),
                            );

                            Ok(Spanned { body: expr, span })
                        }
                    },
                }?;

                if x == Punctuation::BoolAnd {
                    let span = base.span;
                    let expr = Expr::UnaryExpr(
                        Spanned {
                            body: UnaryOp::AddrOf(None),
                            span: outerspan,
                        },
                        Box::new(base),
                    );

                    Ok(Spanned { body: expr, span })
                } else {
                    Ok(base)
                }
            }
            punct => {
                let op = match LexemeClass::Punctuation(punct) {
                    punct!(*) => UnaryOp::Deref,
                    punct!(-) => UnaryOp::Neg,
                    punct!(!) => UnaryOp::Not,
                    punct!(..) => UnaryOp::RangeTo,
                    punct!(..=) => UnaryOp::RangeToInclusive,
                    _ => unreachable!(),
                };
                let inner = do_unary_expression::<ALLOW_CONSTRUCTOR>(&mut tree)?;
                let span = Span::between(outerspan, inner.span);
                let expr = Expr::UnaryExpr(
                    Spanned {
                        body: op,
                        span: outerspan,
                    },
                    Box::new(inner),
                );
                Ok(Spanned { body: expr, span })
            }
        },
        Ok(_) => unreachable!(),
        Err(e) => match do_suffix_expression::<ALLOW_CONSTRUCTOR>(&mut tree) {
            Ok(expr) => {
                tree.accept();
                Ok(expr)
            }
            Err(d) => Err(e | d),
        },
    }
}

pub fn do_cast_expression<const ALLOW_CONSTRUCTOR: bool>(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    let mut tree = tree.into_rewinder();
    let mut lhs = do_unary_expression::<ALLOW_CONSTRUCTOR>(&mut tree)?;

    loop {
        match do_lexeme_token(&mut tree, keyword!(as)) {
            Ok(_) => {
                let ty = do_type_no_bounds(&mut tree)?;
                let new_span = Span::between(lhs.span, ty.span);
                lhs = Spanned {
                    body: Expr::AsCast(Box::new(lhs), Box::new(ty)),
                    span: new_span,
                }
            }
            Err(_) => break,
        }
    }

    tree.accept();

    Ok(lhs)
}

fn binary_ops(x: Punctuation) -> (BinaryOp, u32, u32) {
    match LexemeClass::Punctuation(x) {
        punct!(=) => (BinaryOp::Assign, 1, 0),
        punct!(+=) => (BinaryOp::AddAssign, 1, 0),
        punct!(-=) => (BinaryOp::SubAssign, 1, 0),
        punct!(*=) => (BinaryOp::MulAssign, 1, 0),
        punct!(/=) => (BinaryOp::DivAssign, 1, 0),
        punct!(%=) => (BinaryOp::RemAssign, 1, 0),
        punct!(&=) => (BinaryOp::BitAndAssign, 1, 0),
        punct!(|=) => (BinaryOp::BitOrAssign, 1, 0),
        punct!(^=) => (BinaryOp::BitXorAssign, 1, 0),
        punct!(<<=) => (BinaryOp::LeftShiftAssign, 1, 0),
        punct!(>>=) => (BinaryOp::RightShiftAssign, 1, 0),
        punct!(..) => (BinaryOp::Range, 2, 3), // TODO: special-case to require parens
        punct!(..=) => (BinaryOp::RangeInclusive, 2, 3), // TODO: ^
        punct!(||) => (BinaryOp::BoolOr, 4, 5),
        punct!(&&) => (BinaryOp::BoolAnd, 6, 7),
        punct!(==) => (BinaryOp::Equal, 8, 9), // TODO: special-case to require parens
        punct!(!=) => (BinaryOp::NotEqual, 8, 9), // TODO: ^
        punct!(<) => (BinaryOp::Less, 8, 9),   // TODO: ^
        punct!(>) => (BinaryOp::Greater, 8, 9), // TODO: ^
        punct!(<=) => (BinaryOp::LessEqual, 8, 9), // TODO: ^
        punct!(>=) => (BinaryOp::GreaterEqual, 8, 9), // TODO: ^
        punct!(|) => (BinaryOp::BitOr, 10, 11),
        punct!(^) => (BinaryOp::BitXor, 12, 13),
        punct!(&) => (BinaryOp::BitAnd, 14, 15),
        punct!(<<) => (BinaryOp::LeftShift, 16, 17),
        punct!(>>) => (BinaryOp::RightShift, 16, 17),
        punct!(+) => (BinaryOp::Add, 18, 19),
        punct!(-) => (BinaryOp::Sub, 18, 19),
        punct!(*) => (BinaryOp::Mul, 20, 21),
        punct!(/) => (BinaryOp::Div, 20, 21),
        punct!(%) => (BinaryOp::Rem, 20, 21),
        x => panic!("Not a binary operator {:?}", x),
    }
}

pub fn do_binary_expression<const ALLOW_CONSTRUCTOR: bool>(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    precedence: u32,
) -> Result<Spanned<Expr>> {
    let mut inner = do_cast_expression::<ALLOW_CONSTRUCTOR>(tree)?;
    loop {
        let mut tree = tree.into_rewinder();

        match do_token_classes(
            &mut tree,
            &[
                punct![=],
                punct![+=],
                punct![-=],
                punct![*=],
                punct![/=],
                punct![%=],
                punct![&=],
                punct![|=],
                punct![^=],
                punct![<<=],
                punct![>>=],
                punct![==],
                punct![!=],
                punct![<=],
                punct![>=],
                punct![<],
                punct![>],
                punct![&&],
                punct![||],
                punct![&],
                punct![|],
                punct![^],
                punct![+],
                punct![-],
                punct![*],
                punct![/],
                punct![%],
                punct![..],
                punct![..=],
            ],
        ) {
            Ok((LexemeClass::Punctuation(punct), span, _)) => {
                let (op, lbp, rbp) = binary_ops(punct);
                let op = Spanned { body: op, span };
                if lbp < precedence {
                    break;
                }
                {
                    let mut inner_tree = (&mut tree).into_rewinder();
                    match do_binary_expression::<ALLOW_CONSTRUCTOR>(&mut inner_tree, rbp) {
                        Ok(expr) => {
                            inner_tree.accept();
                            let span = Span::between(inner.span, expr.span);
                            inner = Spanned {
                                body: Expr::BinaryExpr(op, Box::new(inner), Box::new(expr)),
                                span,
                            };
                        }
                        Err(e) => {
                            if op.body == BinaryOp::Range {
                                let op = Spanned {
                                    body: UnaryOp::RangeFrom,
                                    span,
                                };
                                let span = Span::between(inner.span, op.span);

                                inner = Spanned {
                                    body: Expr::UnaryExpr(op, Box::new(inner)),
                                    span,
                                };
                            } else {
                                return Err(e);
                            }
                        }
                    }
                }

                tree.accept();
            }
            Ok(_) => unreachable!(),
            Err(_) => break,
        }
    }

    Ok(inner)
}

pub fn do_expression_maybe_constructor<const ALLOW_CONSTRUCTOR: bool>(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    do_binary_expression::<ALLOW_CONSTRUCTOR>(tree, 0)
}

pub fn do_expression(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    do_expression_maybe_constructor::<true>(tree)
}

pub fn do_expression_without_constructor(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Expr>> {
    do_expression_maybe_constructor::<false>(tree)
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

pub fn do_generic_arg(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<GenericArg>> {
    do_alternation(
        tree,
        &[
            do_generic_arg_associated,
            do_generic_arg_path,
            do_generic_arg_type,
            do_generic_arg_expr,
        ],
    )
}

pub fn do_generic_arg_path(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<GenericArg>> {
    let path = do_path(tree)?;

    let span = path.span;

    Ok(Spanned {
        body: GenericArg::Id(path),
        span,
    })
}

pub fn do_generic_arg_associated(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<GenericArg>> {
    let mut tree = tree.into_rewinder();

    let (span, id) = do_lexeme_token(&mut tree, LexemeClass::Identifier)?;
    let id = Spanned {
        body: id.body,
        span,
    };

    match do_token_classes(&mut tree, &[punct!(=), punct!(:)]) {
        Ok((punct!(=), eq, _)) => {
            let ty = do_type(&mut tree)?;
            let tspan = ty.span;

            let bound_span = Span::between(eq, tspan);
            let bound = AssociatedTypeBound::Exact(ty);

            let span = Span::between(span, tspan);

            Ok(Spanned {
                body: GenericArg::AssociatedType(
                    id,
                    Spanned {
                        body: bound,
                        span: bound_span,
                    },
                ),
                span,
            })
        }
        Ok((punct!(:), _, _)) => {
            todo!("Associated Type Bounds")
        }
        Ok(_) => unreachable!(),
        Err(e) => Err(e),
    }
}

pub fn do_generic_arg_type(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<GenericArg>> {
    let ty = do_type(tree)?;
    let span = ty.span;

    Ok(Spanned {
        body: GenericArg::Type(ty),
        span,
    })
}
pub fn do_generic_arg_expr(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<GenericArg>> {
    let expr = do_simple_block(tree)?;
    let span = expr.span;
    let expr = Expr::BlockExpr(expr);

    Ok(Spanned {
        body: GenericArg::Const(Spanned { body: expr, span }),
        span,
    })
}

pub fn do_generic_args(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<GenericArgs>> {
    let mut tree = tree.into_rewinder();
    let begin_span = do_lexeme_class(&mut tree, punct!(<))?.span;
    let mut args = vec![];
    let end_span = loop {
        let err = match do_lexeme_class(&mut tree, punct!(>)) {
            Ok(lex) => break lex.span,
            Err(e) => e,
        };

        match do_generic_arg(&mut tree) {
            Ok(arg) => args.push(arg),
            Err(e) => return Err(e | err),
        }

        match do_lexeme_classes(&mut tree, &[punct!(,), punct!(>)]) {
            Ok((_, punct!(,))) => continue,
            Ok((lex, punct!(>))) => break lex.span,
            Ok(_) => unreachable!(),
            Err(e) => return Err(e),
        }
    };

    tree.accept();
    Ok(Spanned {
        body: GenericArgs { args },
        span: Span::between(begin_span, end_span),
    })
}

pub fn do_path_segment(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<PathSegment>> {
    let ident = do_simple_path_segment(tree)?;
    let mut span = ident.span;
    let generics = {
        let mut tree = tree.into_rewinder();

        if do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Path)).is_ok() {
            match do_generic_args(&mut tree) {
                Ok(val) => {
                    span = Span::between(span, val.span);
                    tree.accept();
                    Some(val)
                }
                Err(e) => match do_lexeme_class(&mut tree, LexemeClass::IdentOrKeyword) {
                    Ok(_) => None,
                    Err(d) => return Err(e | d),
                },
            }
        } else {
            None
        }
    };
    Ok(Spanned {
        body: PathSegment { ident, generics },
        span,
    })
}

pub fn do_path(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Spanned<Path>> {
    // TODO: support QSelf/::
    let mut segments = Vec::new();
    segments.push(do_path_segment(tree)?);
    let mut span = segments[0].span;
    while do_lexeme_class(tree, LexemeClass::Punctuation(Punctuation::Path)).is_ok() {
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
        match do_lexeme_class(
            &mut tuple_tree,
            LexemeClass::Punctuation(Punctuation::Comma),
        ) {
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
    } = do_lexeme_class(&mut tree, LexemeClass::Punctuation(Punctuation::Mul))?;
    let mutability_kw = do_lexeme_classes(&mut tree, &[keyword!(mut), keyword!(const)])?;
    let mutability_span = mutability_kw.0.span;
    let mutability = Spanned {
        body: match mutability_kw.1 {
            keyword!(mut) => Mutability::Mut,
            keyword!(const) => Mutability::Const,
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
            Err(b) => match do_lexeme_class(tree, punct!(!)) {
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

pub fn do_pattern_binding(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Pattern>> {
    let mut tree = tree.into_rewinder();

    let mt = match do_lexeme_class(&mut tree, keyword!(mut)) {
        Ok(lex) => Some(Spanned {
            body: Mutability::Mut,
            span: lex.span,
        }),
        Err(_) => None,
    };

    let (span, tok) = do_lexeme_token(&mut tree, LexemeClass::Identifier)?;

    let id = Spanned {
        span,
        body: tok.body,
    };

    let binding = match do_lexeme_classes(&mut tree, &[punct!(@), punct!(::)]) {
        Ok((_, punct!(@))) => Some(Box::new(do_pattern_param(&mut tree)?)),
        Ok((lex, punct!(::))) => {
            return Err(Error {
                expected: vec![punct!(@)],
                got: punct!(::),
                span: lex.span,
            })
        } // bail out so that `do_pattern_const_ctor` works
        Ok(_) => unreachable!(),
        Err(_) => None,
    };

    tree.accept();

    match (mt, binding) {
        (None, None) => Ok(Spanned {
            body: Pattern::BareId(id),
            span,
        }),
        (ismut, bound_pattern) => {
            let startspan = ismut.as_ref().map(|mt| mt.span).unwrap_or(span);
            let endspan = bound_pattern
                .as_ref()
                .map(|binding| binding.span)
                .unwrap_or(span);

            let span = Span::between(startspan, endspan);

            let body = BindingPattern {
                ismut,
                id,
                bound_pattern,
            };

            let body = Pattern::Binding(Spanned { span, body });

            Ok(Spanned { span, body })
        }
    }
}

pub fn do_pattern_literal(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Pattern>> {
    let lit = do_literal(tree)?;

    let span = lit.span;

    let body = Pattern::LiteralPattern(lit);

    Ok(Spanned { span, body })
}

pub fn do_pattern_const_ctor(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Pattern>> {
    let mut tree = tree.into_rewinder();
    let path = do_path(&mut tree)?;

    let pspan = path.span;
    // TODO: Constructor
    tree.accept();
    Ok(Spanned {
        body: Pattern::Const(path),
        span: pspan,
    })
}

pub fn do_pattern_discard(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Pattern>> {
    let id = do_lexeme_class(tree, keyword!(_))?;

    Ok(Spanned {
        body: Pattern::Discard,
        span: id.span,
    })
}

pub fn do_pattern_param(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Pattern>> {
    do_alternation(
        tree,
        &[
            do_pattern_binding,
            do_pattern_const_ctor,
            do_pattern_discard,
            do_pattern_literal,
        ],
    )
}

pub fn do_pattern(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Pattern>> {
    // TODO: OR patterns
    let mut tree = tree.into_rewinder();
    let pat = do_pattern_param(&mut tree)?;
    tree.accept();
    Ok(pat)
}

pub fn do_param(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Param>> {
    // TODO: handle params without names - only in Rust 2015 though
    let mut tree = tree.into_rewinder();
    let pat = do_pattern_param(&mut tree)?;
    do_lexeme_class(&mut tree, punct!(:))?;
    let ty = do_type(&mut tree)?;
    tree.accept();
    let span = Span::between(pat.span, ty.span);
    Ok(Spanned {
        body: Param { pat: Some(pat), ty },
        span,
    })
}

pub fn do_lifetime(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Lifetime>> {
    let lex = do_lexeme_class(tree, LexemeClass::Lifetime)?;

    let span = lex.span;

    let body = match lex.body {
        LexemeBody::Token(tok) => match &*tok.body {
            "'static" => Lifetime::Static,
            "'_" => Lifetime::Elided,
            _ => Lifetime::Named(Spanned {
                body: tok.body,
                span,
            }),
        },
        _ => unreachable!(),
    };

    Ok(Spanned { body, span })
}

pub fn do_type_bound(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<GenericBound>> {
    let mut tree = tree.into_rewinder();
    match do_lifetime(&mut tree) {
        Ok(life) => {
            let span = life.span;
            tree.accept();
            Ok(Spanned {
                body: GenericBound::LifetimeBound(life),
                span,
            })
        }
        Err(e) => match do_path(&mut tree) {
            Ok(bound) => {
                let span = bound.span;
                tree.accept();
                Ok(Spanned {
                    body: GenericBound::TraitBound(bound),
                    span,
                })
            }
            Err(d) => Err(e | d),
        },
    }
}

pub fn do_generic_param(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<GenericParam>> {
    let mut tree = tree.into_rewinder();

    let res = match do_lexeme_classes(
        &mut tree,
        &[
            keyword!(const),
            LexemeClass::Identifier,
            LexemeClass::Lifetime,
        ],
    )? {
        (lex, LexemeClass::Keyword(_)) => {
            let begin_span = lex.span;
            let id = do_lexeme_class(&mut tree, LexemeClass::Identifier)?;

            let id = match id.body {
                LexemeBody::Token(tok) => Spanned {
                    body: tok.body,
                    span: id.span,
                },
                _ => unreachable!(),
            };

            do_lexeme_class(&mut tree, punct!(:))?;

            let ty = do_type(&mut tree)?;

            let span = Span::between(begin_span, ty.span);

            tree.accept();

            Spanned {
                body: GenericParam::Const(ConstParam { name: id, ty }),
                span,
            }
        }
        (id, LexemeClass::Identifier) => {
            let beginspan = id.span;
            let name = match id.body {
                LexemeBody::Token(tok) => Spanned {
                    body: tok.body,
                    span: id.span,
                },
                _ => unreachable!(),
            };

            let mut endspan = beginspan;

            let mut bounds = Vec::new();
            if let Ok(lex) = do_lexeme_class(&mut tree, punct!(:)) {
                endspan = lex.span;
                loop {
                    match do_type_bound(&mut tree) {
                        Ok(bound) => {
                            endspan = bound.span;
                            bounds.push(bound)
                        }
                        Err(_) => break,
                    }

                    match do_lexeme_class(&mut tree, punct!(+)) {
                        Ok(lex) => {
                            endspan = lex.span;
                            continue;
                        }
                        Err(_) => break,
                    }
                }
            }
            let span = Span::between(beginspan, endspan);
            tree.accept();
            Spanned {
                body: GenericParam::Type(TypeParam { name, bounds }),
                span,
            }
        }
        (life, LexemeClass::Lifetime) => {
            let beginspan = life.span;
            let name = match life.body {
                LexemeBody::Token(tok) => Spanned {
                    body: tok.body,
                    span: beginspan,
                },
                _ => unreachable!(),
            };

            match &**name {
                "'_" => {
                    return Err(Error {
                        expected: vec![LexemeClass::Lifetime],
                        got: keyword!('_),
                        span: name.span,
                    })
                }
                "static" => {
                    return Err(Error {
                        expected: vec![LexemeClass::Lifetime],
                        got: keyword!('static),
                        span: name.span,
                    })
                }
                _ => {}
            }

            let mut endspan = beginspan;

            let mut bounds = Vec::new();
            if let Ok(lex) = do_lexeme_class(&mut tree, punct!(:)) {
                endspan = lex.span;
                loop {
                    match do_lifetime(&mut tree) {
                        Ok(bound) => {
                            endspan = bound.span;
                            bounds.push(bound)
                        }
                        Err(_) => break,
                    }

                    match do_lexeme_class(&mut tree, punct!(+)) {
                        Ok(lex) => {
                            endspan = lex.span;
                            continue;
                        }
                        Err(_) => break,
                    }
                }
            }
            let span = Span::between(beginspan, endspan);
            tree.accept();
            Spanned {
                body: GenericParam::Lifetime(LifetimeParam { name, bounds }),
                span,
            }
        }
        _ => unreachable!(),
    };

    Ok(res)
}

pub fn do_generic_params(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<GenericParams>> {
    let mut tree = tree.into_rewinder();

    let lexeme = do_lexeme_class(&mut tree, punct!(<))?;

    let start_span = lexeme.span;

    let mut params = GenericParams { params: Vec::new() };

    let end_span = loop {
        match do_generic_param(&mut tree) {
            Ok(val) => params.params.push(val),
            Err(e) => match do_lexeme_class(&mut tree, punct!(>)) {
                Ok(lexeme) => break lexeme.span,
                Err(d) => return Err(e | d),
            },
        }
    };
    tree.accept();
    Ok(Spanned {
        body: params,
        span: Span::between(start_span, end_span),
    })
}

pub fn do_item_fn(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();

    let constness = match do_lexeme_token(&mut tree, keyword!(const)) {
        Ok((span, _)) => Some(Spanned {
            body: Mutability::Const,
            span,
        }),
        Err(_) => None,
    };

    let is_async = match do_lexeme_token(&mut tree, keyword!(async)) {
        Ok((span, _)) => Some(Spanned { body: Async, span }),
        Err(_) => None,
    };

    let safety = match do_lexeme_token(&mut tree, keyword!(unsafe)) {
        Ok((span, _)) => Some(Spanned {
            body: Safety::Unsafe,
            span,
        }),
        Err(_) => None,
    };

    let abi = match do_lexeme_token(&mut tree, keyword!(extern)) {
        Ok((span, _)) => {
            let (span, tag) = match do_string(&mut tree) {
                Ok((tok, _)) => (Span::between(span, tok.span), Some(tok)),
                Err(_) => (span, None),
            };

            Some(Spanned {
                body: Extern { tag },
                span,
            })
        }
        Err(_) => None,
    };

    let Lexeme {
        span: span_start, ..
    } = do_lexeme_class(&mut tree, keyword!(fn))?;
    let name = do_lexeme_class(&mut tree, LexemeClass::Identifier)?;
    let name = Spanned {
        body: *name.text().unwrap(),
        span: name.span,
    };

    let generics = do_generic_params(&mut tree).ok();

    let (params_group, _) = do_lexeme_group(&mut tree, Some(GroupType::Parens))?;
    // TODO: receiver
    let mut params_tree = params_group.body.into_iter().peekmore();
    let mut params = Vec::new();
    while !params_tree.peek().is_eof() {
        params.push(do_param(&mut params_tree)?);
        match do_lexeme_class(&mut params_tree, punct!(,)) {
            Ok(_) => {}
            Err(e) => {
                if !params_tree.peek().is_eof() {
                    Err(e)?
                }
            }
        }
    }
    let ret_ty = match do_lexeme_class(&mut tree, punct!(->)) {
        Ok(_) => Some(do_type(&mut tree)?),
        Err(_) => None,
    };
    let (body, span_end) = match do_lexeme_class(&mut tree, punct!(;)) {
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
                safety,
                abi,
                constness,
                is_async,
                name,
                generics,
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

#[allow(dead_code)]
pub fn do_receiver(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<SelfParam>> {
    let mut tree = tree.into_rewinder();
    match do_lexeme_token(&mut tree, punct!(&)) {
        Ok((span, _)) => {
            let mt = do_lexeme_token(&mut tree, keyword!(mut))
                .map(|(span, _)| Spanned {
                    span,
                    body: Mutability::Mut,
                })
                .ok();

            let (espan, _) = do_lexeme_token(&mut tree, keyword!(self))?;

            let span = Span::between(span, espan);
            let body = SelfParam::RefSelf(mt);
            tree.accept();
            Ok(Spanned { span, body })
        }
        Err(_) => {
            let mt = do_lexeme_token(&mut tree, keyword!(mut))
                .map(|(span, _)| Spanned {
                    span,
                    body: Mutability::Mut,
                })
                .ok();

            let (sspan, _) = do_lexeme_token(&mut tree, keyword!(self))?;
            let startspan = mt.map(|mt| mt.span).unwrap_or(sspan);

            let selfty = match do_lexeme_token(&mut tree, punct!(:)) {
                Ok(_) => Some(do_type(&mut tree)?),
                Err(_) => None,
            };
            let endspan = selfty.as_ref().map(|ty| ty.span).unwrap_or(sspan);
            let span = Span::between(startspan, endspan);

            let body = SelfParam::BaseSelf(mt, selfty);
            tree.accept();
            Ok(Spanned { span, body })
        }
    }
}

#[allow(dead_code)]
pub fn do_item_fn_in_trait(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let Lexeme {
        span: span_start, ..
    } = do_lexeme_class(&mut tree, keyword!(fn))?;
    let name = do_lexeme_class(&mut tree, LexemeClass::Identifier)?;
    let name = Spanned {
        body: *name.text().unwrap(),
        span: name.span,
    };

    let generics = do_generic_params(&mut tree).ok();

    let (params_group, _) = do_lexeme_group(&mut tree, Some(GroupType::Parens))?;
    // TODO: receiver
    let mut params_tree = params_group.body.into_iter().peekmore();

    let _receiver = match do_receiver(&mut params_tree) {
        Ok(x) => {
            do_lexeme_classes(&mut params_tree, &[punct!(,), LexemeClass::Eof])?;
            Some(x)
        }
        Err(_) => None,
    };
    let mut params = Vec::new();
    while !params_tree.peek().is_eof() {
        params.push(do_param(&mut params_tree)?);
        match do_lexeme_class(&mut params_tree, punct!(,)) {
            Ok(_) => {}
            Err(e) => {
                if !params_tree.peek().is_eof() {
                    Err(e)?
                }
            }
        }
    }
    let ret_ty = match do_lexeme_class(&mut tree, punct!(->)) {
        Ok(_) => Some(do_type(&mut tree)?),
        Err(_) => None,
    };
    let (body, span_end) = match do_lexeme_class(&mut tree, punct!(;)) {
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
                generics,
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

pub fn do_char(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<(Spanned<Symbol>, CharType)> {
    let full_str = do_lexeme_class(tree, LexemeClass::Character)?;
    let chr_ty = *if let Lexeme {
        body:
            LexemeBody::Token(Token {
                ty: TokenType::Character(chr_ty),
                ..
            }),
        ..
    } = &full_str
    {
        chr_ty
    } else {
        unreachable!()
    };
    let str = full_str.text().unwrap();
    let str = match chr_ty {
        CharType::Default => &str[1..str.len() - 1], // Skip    ' and '
        CharType::Byte => &str[2..str.len() - 1],    // Skip   b' and '
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
    dbg!(&full_str);
    Ok((
        Spanned {
            body: parsed.into(),
            span: full_str.span,
        },
        chr_ty,
    ))
}

pub fn do_item_extern_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let Lexeme {
        span: span_start, ..
    } = do_lexeme_class(&mut tree, keyword!(extern))?;
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

pub fn do_item_impl(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    expect(&mut tree, &[keyword!(unsafe), keyword!(impl)])?;

    let mut startspan = None;

    let safety = match do_lexeme_class(&mut tree, keyword!(unsafe)) {
        Ok(tok) => {
            startspan = Some(tok.span);
            Some(Spanned {
                body: Safety::Unsafe,
                span: tok.span,
            })
        }
        Err(_) => None,
    };

    let startspan = startspan.unwrap_or(do_lexeme_class(&mut tree, keyword!(impl))?.span);

    let generics = do_generic_params(&mut tree).ok();

    let tr = {
        let mut tree = (&mut tree).into_rewinder();

        match do_path(&mut tree) {
            Ok(path) => match do_lexeme_class(&mut tree, keyword!(for)) {
                Ok(_) => {
                    tree.accept();
                    Some(path)
                }
                Err(_) => None,
            },
            Err(_) => None,
        }
    };

    let ty = do_type(&mut tree)?;

    let id_span = Span::between(startspan, ty.span);

    let where_clauses = do_where_clauses(&mut tree).ok();

    let mut body = Vec::new();

    let (group, gspan) = do_lexeme_group(&mut tree, Some(GroupType::Braces))?;

    let mut inner_tree = group.body.into_iter().peekmore();

    loop {
        match do_item_in_trait(&mut inner_tree) {
            Ok(item) => body.push(item),
            Err(e) => match do_lexeme_class(&mut inner_tree, LexemeClass::Eof) {
                Ok(_) => break,
                Err(d) => return Err(e | d),
            },
        }
    }

    let impl_id = Spanned {
        span: id_span,
        body: xlang::gen_rand(),
    };

    let body = ImplBlock {
        safety,
        tr,
        ty,
        generics,
        where_clauses,
        body,
        impl_id,
    };

    let span = Span::between(startspan, gspan);

    tree.accept();

    Ok(Spanned {
        body: ItemBody::ImplBlock(Spanned { span, body }),
        span,
    })
}

pub fn do_item_trait(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    expect(
        &mut tree,
        &[keyword!(unsafe), keyword!(auto), keyword!(trait)],
    )?;
    let mut startspan = None;

    let safety = match do_lexeme_class(&mut tree, keyword!(unsafe)) {
        Ok(tok) => {
            startspan.get_or_insert(tok.span);
            Some(Spanned {
                body: Safety::Unsafe,
                span: tok.span,
            })
        }
        Err(_) => None,
    };

    let auto = match do_lexeme_class(&mut tree, keyword!(auto)) {
        Ok(tok) => {
            startspan.get_or_insert(tok.span);
            Some(Spanned {
                body: Auto,
                span: tok.span,
            })
        }
        Err(_) => None,
    };

    do_lexeme_class(&mut tree, keyword!(trait))?;

    let name = do_identifier(&mut tree)?;

    let startspan = startspan.unwrap_or(name.span);

    let generics = do_generic_params(&mut tree).ok();

    let supertraits = match do_lexeme_token(&mut tree, punct!(:)) {
        Ok(_) => {
            let mut bounds_list = Vec::new();
            loop {
                match do_type_bound(&mut tree) {
                    Ok(bound) => bounds_list.push(bound),
                    Err(_) => break,
                }

                match do_lexeme_classes(&mut tree, &[punct!(+)]) {
                    Ok(_) => continue,
                    Err(_) => break,
                }
            }
            Some(bounds_list)
        }
        Err(_) => None,
    };

    let where_clauses = do_where_clauses(&mut tree).ok();

    let mut body = Vec::new();

    let (group, gspan) = do_lexeme_group(&mut tree, Some(GroupType::Braces))?;

    let mut inner_tree = group.body.into_iter().peekmore();

    loop {
        match do_item_in_trait(&mut inner_tree) {
            Ok(item) => body.push(item),
            Err(e) => match do_lexeme_class(&mut inner_tree, LexemeClass::Eof) {
                Ok(_) => break,
                Err(d) => return Err(e | d),
            },
        }
    }

    let body = TraitDef {
        safety,
        auto,
        name,
        generics,
        supertraits,
        where_clauses,
        body,
    };

    let span = Span::between(startspan, gspan);

    tree.accept();

    Ok(Spanned {
        body: ItemBody::Trait(Spanned { span, body }),
        span,
    })
}

pub fn do_item_in_trait(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Item>> {
    let mut tree = tree.into_rewinder();
    let mut attrs = Vec::new();
    loop {
        match do_external_attr(&mut tree) {
            Ok(attr) => attrs.push(attr),
            Err(_) => break,
        }
    }
    let vis = do_visibility(&mut tree).ok();
    let item = do_alternation(&mut tree, &[do_item_fn, do_item_value])?;
    let span = vis.as_ref().map_or(item.span, |Spanned { span, .. }| {
        Span::between(*span, item.span)
    });
    tree.accept();
    Ok(Spanned {
        body: Item { vis, attrs, item },
        span,
    })
}

pub fn do_item(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Spanned<Item>> {
    let mut tree = tree.into_rewinder();
    let mut attrs = Vec::new();
    loop {
        match do_external_attr(&mut tree) {
            Ok(attr) => attrs.push(attr),
            Err(_) => break,
        }
    }
    let vis = do_visibility(&mut tree).ok();
    let item = do_alternation(
        &mut tree,
        &[
            do_item_mod,
            do_item_extern_block,
            do_item_extern_crate,
            do_item_fn,
            do_item_value,
            do_item_use,
            do_item_user_type,
            do_item_trait,
            do_item_impl,
        ],
    )?;
    let span = vis.as_ref().map_or(item.span, |Spanned { span, .. }| {
        Span::between(*span, item.span)
    });
    tree.accept();
    Ok(Spanned {
        body: Item { vis, attrs, item },
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
            Err(x) => match do_item(tree) {
                Ok(mut item) => {
                    item.attrs.append(&mut external_attrs);
                    items.push(item);
                }
                Err(z) => Err(x | z)?,
            },
        }
    }

    span = Span::between(span, tree.peek().unwrap().span);

    Ok(Spanned {
        body: Mod { attrs, items },
        span,
    })
}
