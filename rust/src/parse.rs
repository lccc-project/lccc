use core::ops::{BitOr, BitOrAssign, Deref, DerefMut};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    ast::{Attr, AttrInput, Item, Mod, SimplePath, SimplePathSegment, Spanned},
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
        } else if self.span.end > rhs.span.end {
            *self = rhs;
        } else if self.span.end == rhs.span.end {
            self.expected.append(&mut rhs.expected);
        } else {
            todo!(); // Uh oh. Multiple files collided.
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
        tree.next();
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
                return Ok((x, class))
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
    let (from_root, mut span) = match do_lexeme_class(&mut tree, LexemeClass::Punctuation("::".into())) {
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

pub fn do_item(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Item> {
    todo!()
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
