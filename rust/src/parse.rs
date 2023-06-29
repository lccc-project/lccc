use core::ops::{BitOr, BitOrAssign, Deref, DerefMut};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    ast::{
        Attr, AttrInput, Block, ExternBlock, Function, Item, ItemBody, ItemValue, Mod, SimplePath,
        SimplePathSegment, Spanned, Statement, UserType, Visibility,
    },
    lex::{Group, GroupType, Lexeme, LexemeBody, LexemeClass},
    span::{Pos, Span},
};

#[derive(Debug)]
pub struct Error {
    expected: Vec<LexemeClass>,
    got: LexemeClass,
    span: Span,
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
        } else if self.span.end < rhs.span.end {
            *self = rhs;
        } else if self.span.end == rhs.span.end {
            self.expected.append(&mut rhs.expected);
        } else {
            todo!("{:?}, {:?}", self, rhs); // Uh oh. Multiple files collided.
        }
    }
}

pub type Result<T> = core::result::Result<T, Error>;

struct Rewinder<'a, T: Iterator<Item = Lexeme>> {
    inner: &'a mut PeekMoreIterator<T>,
    cursor: usize,
}

impl<T: Iterator<Item = Lexeme>> Rewinder<'_, T> {
    fn accept(&mut self) {
        self.cursor = self.inner.cursor(); // We're good; don't rewind when we drop
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

trait IntoRewinder<'a> {
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
    let kw_mod = do_lexeme_class(&mut tree, LexemeClass::Keyword("mod".into()))?;
    todo!()
}

pub fn do_item_value_static(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemValue>> {
    let mut tree = tree.into_rewinder();
    let kw_static = do_lexeme_class(&mut tree, LexemeClass::Keyword("static".into()))?;
    todo!()
}

pub fn do_item_value_const(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemValue>> {
    let mut tree = tree.into_rewinder();
    let kw_const = do_lexeme_class(&mut tree, LexemeClass::Keyword("const".into()))?;
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
    let kw_extern = do_lexeme_class(&mut tree, LexemeClass::Keyword("extern".into()))?;
    let kw_crate = do_lexeme_class(&mut tree, LexemeClass::Keyword("crate".into()))?;
    todo!()
}

pub fn do_item_use(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let kw_use = do_lexeme_class(&mut tree, LexemeClass::Keyword("use".into()))?;
    todo!()
}

pub fn do_user_type_struct(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<UserType>> {
    let mut tree = tree.into_rewinder();
    let kw_struct = do_lexeme_class(&mut tree, LexemeClass::Keyword("struct".into()))?;
    todo!()
}

pub fn do_user_type_enum(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<UserType>> {
    let mut tree = tree.into_rewinder();
    let kw_enum = do_lexeme_class(&mut tree, LexemeClass::Keyword("enum".into()))?;
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

pub fn do_statement(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Statement>> {
    todo!()
}

pub fn do_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Block>> {
    let (block, span) = do_lexeme_group(tree, Some(GroupType::Braces))?;
    let mut stmts = Vec::new();
    let mut block_tree = block.body.into_iter().peekmore();
    while block_tree.peek().is_some() {
        let stmt = do_statement(&mut block_tree);
        if let Ok(stmt) = stmt {
            stmts.push(stmt);
        } else {
            break;
        }
    }
    let tail_expr = None; // TODO
    Ok(Spanned {
        body: Block { stmts, tail_expr },
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
    let _params = do_lexeme_group(&mut tree, Some(GroupType::Parens))?;
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
    Ok(Spanned {
        body: ItemBody::Function(Spanned {
            body: Function {
                safety: None,
                abi: None,
                constness: None,
                isasync: None,
                name,
                generics: None,
                reciever: None,     // TODO: parse params
                params: Vec::new(), // TODO: parse params
                varargs: None,
                retty: None, // TODO: duh
                body,
            },
            span,
        }),
        span,
    })
}

pub fn do_item_extern_block(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<ItemBody>> {
    let mut tree = tree.into_rewinder();
    let Lexeme {
        span: span_start, ..
    } = do_lexeme_class(&mut tree, LexemeClass::Keyword("extern".into()))?;
    let tag = do_lexeme_class(&mut tree, LexemeClass::String)
        .ok()
        .map(|x| Spanned {
            body: *x.text().unwrap(),
            span: x.span,
        });
    let (block, span_end) = do_lexeme_group(&mut tree, Some(GroupType::Braces))?;
    let mut items = Vec::new();
    let mut block_tree = block.body.into_iter().peekmore();
    while block_tree.peek().is_some() {
        items.push(do_item(&mut block_tree)?);
    }
    let span = Span::between(span_start, span_end);
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
    Ok(Spanned {
        body: Item {
            vis,
            attrs: vec![],
            item,
        },
        span,
    })
}

pub fn do_mod(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Mod> {
    let mut attrs = Vec::new();
    let mut items = Vec::new();

    let mut external_attrs = Vec::new();

    while tree.peek().is_some() {
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

    Ok(Mod { attrs, items })
}
