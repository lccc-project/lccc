use core::ops::{BitOr, BitOrAssign};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    ast::{Attr, AttrInput, Item, Mod, SimplePath, SimplePathSegment, SimplePathSegmentBody},
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

pub fn do_lexeme_class(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    expected: LexemeClass,
) -> Result<Lexeme> {
    let lexeme = tree.peek();
    let got = LexemeClass::of(lexeme);
    if got.is(&expected) {
        let result = lexeme.unwrap().clone();
        tree.advance_cursor();
        Ok(result)
    } else {
        let span = lexeme.map_or_else(
            || Span::new_simple(Pos::default(), Pos::default()),
            |x| x.span,
        );
        tree.reset_cursor();
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
    let mut error = None;
    for &class in expected {
        let result = do_lexeme_class(tree, class);
        match result {
            Ok(x) => return Ok((x, class)),
            Err(x) => match &mut error {
                Some(old) => *old |= x,
                old @ None => *old = Some(x),
            }
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

pub fn do_simple_path_segment(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<SimplePathSegment> {
    let (lexeme, class) = do_lexeme_classes(tree, &[LexemeClass::Identifier, LexemeClass::Keyword("super".into()), LexemeClass::Keyword("self".into()), LexemeClass::Keyword("crate".into()), LexemeClass::Punctuation("$".into())])?;
    let span = lexeme.span;
    match class {
        LexemeClass::Identifier => Ok(SimplePathSegment { body: SimplePathSegmentBody::Identifier(lexeme.into_text().unwrap()), span }),
        LexemeClass::Keyword(x) if x == "super" => Ok(SimplePathSegment { body: SimplePathSegmentBody::SuperPath, span }),
        LexemeClass::Keyword(x) if x == "self" => Ok(SimplePathSegment { body: SimplePathSegmentBody::SelfPath, span }),
        LexemeClass::Keyword(x) if x == "crate" => Ok(SimplePathSegment { body: SimplePathSegmentBody::CratePath, span }),
        LexemeClass::Punctuation(x) if x == "$" => todo!(),
        _ => unreachable!(),
    }
}

pub fn do_simple_path(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<SimplePath> {
    let from_root = do_lexeme_class(tree, LexemeClass::Punctuation("::".into())).is_ok();
    let mut segments = Vec::new();
    loop {
        let lexeme = do_simple_path_segment(tree)?;
        segments.push(lexeme);
        if do_lexeme_class(tree, LexemeClass::Punctuation("::".into())).is_err() {
            break;
        }
    }
    Ok(SimplePath { from_root, segments })
}

pub fn do_attr_body(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<(SimplePath, Option<AttrInput>)> {
    let name = do_simple_path(tree)?;
    Ok((name, None))
}

pub fn do_internal_attr(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Attr> {
    do_lexeme_class(tree, LexemeClass::Punctuation("#".into()))?;
    do_lexeme_class(tree, LexemeClass::Punctuation("!".into()))?;
    let (group, span) = do_lexeme_group(tree, Some(GroupType::Brackets))?;
    let (name, input) = do_attr_body(&mut group.body.into_iter().peekmore())?;
    Ok(Attr { name, input, span })
}

pub fn do_external_attr(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Attr> {
    do_lexeme_class(tree, LexemeClass::Punctuation("#".into()))?;
    let body = do_lexeme_group(tree, Some(GroupType::Brackets))?;
    todo!()
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

    Ok(Mod {
        attrs: Vec::new(),
        items: Vec::new(),
    })
}
