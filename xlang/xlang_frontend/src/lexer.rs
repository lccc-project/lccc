use core::convert::Infallible;
use core::hash::Hash;
use std::ops::{
    Bound, Range, RangeBounds, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive,
};

use xlang::abi::string::String;
use xlang::abi::{vec, vec::Vec};

use crate::{
    iter::{PeekMoreIterator, Peekmore},
    span::{NoHygiene, Pos, Span, Spanned, Speekable, Speekerator},
    symbol::Symbol,
};

use unicode_xid::UnicodeXID;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct DefaultTy;

#[derive(Debug)]
pub enum Error {
    UnexpectedEof(Pos),
    UnrecognizedChar(char, Pos),
    Custom(Pos, String),
}

pub type Result<T> = core::result::Result<T, Error>;

pub fn is_ident_start_unicode(c: char) -> bool {
    c.is_xid_start()
}

pub fn is_ident_part_unicode(c: char) -> bool {
    c.is_xid_continue()
}

pub fn is_ident_start_ascii(c: char) -> bool {
    c.is_ascii_alphabetic()
}

pub fn is_ident_part_ascii(c: char) -> bool {
    c.is_ascii_alphanumeric()
}

pub trait CharCount {
    fn contains(&self, c: usize) -> bool;
    fn end_bound(&self) -> Bound<&usize>;
}

impl CharCount for usize {
    fn contains(&self, c: usize) -> bool {
        c == *self
    }

    fn end_bound(&self) -> Bound<&usize> {
        Bound::Included(self)
    }
}

impl CharCount for Range<usize> {
    fn contains(&self, c: usize) -> bool {
        self.contains(&c)
    }
    fn end_bound(&self) -> Bound<&usize> {
        <Self as RangeBounds<usize>>::end_bound(&self)
    }
}

impl CharCount for RangeTo<usize> {
    fn contains(&self, c: usize) -> bool {
        self.contains(&c)
    }
    fn end_bound(&self) -> Bound<&usize> {
        <Self as RangeBounds<usize>>::end_bound(&self)
    }
}

impl CharCount for RangeFrom<usize> {
    fn contains(&self, c: usize) -> bool {
        self.contains(&c)
    }
    fn end_bound(&self) -> Bound<&usize> {
        <Self as RangeBounds<usize>>::end_bound(&self)
    }
}

impl CharCount for RangeInclusive<usize> {
    fn contains(&self, c: usize) -> bool {
        self.contains(&c)
    }
    fn end_bound(&self) -> Bound<&usize> {
        <Self as RangeBounds<usize>>::end_bound(&self)
    }
}

impl CharCount for RangeToInclusive<usize> {
    fn contains(&self, c: usize) -> bool {
        self.contains(&c)
    }
    fn end_bound(&self) -> Bound<&usize> {
        <Self as RangeBounds<usize>>::end_bound(&self)
    }
}

impl CharCount for RangeFull {
    fn contains(&self, _: usize) -> bool {
        true
    }
    fn end_bound(&self) -> Bound<&usize> {
        <Self as RangeBounds<usize>>::end_bound(&self)
    }
}

pub struct HexEscape<C>(pub char, pub C);

impl<R: CharCount> EscapeMatcher for HexEscape<R> {
    fn try_match<I: Iterator<Item = char>>(
        &self,
        sel: char,
        it: &mut PeekMoreIterator<Speekable<I>>,
        accepter: &mut String,
    ) -> Result<bool> {
        if sel != self.0 {
            return Ok(false);
        }

        let max = self.1.end_bound();
        let mut counter = 0usize;

        while let Some((pos, c)) = it.peek().copied() {
            if !c.is_digit(16) {
                if self.1.contains(counter) {
                    return Ok(true);
                } else {
                    return Err(Error::UnrecognizedChar(c, pos));
                }
            }
            counter += 1;
            accepter.push(c);
            it.next();
            if !(Bound::Unbounded, max).contains(&counter) {
                return Ok(true);
            }
        }

        if self.1.contains(counter) {
            Ok(true)
        } else {
            Err(Error::UnexpectedEof(it.last_pos()))
        }
    }
}

pub struct UnicodeEscapeRust<C>(pub char, pub C);

impl<R: CharCount> EscapeMatcher for UnicodeEscapeRust<R> {
    fn try_match<I: Iterator<Item = char>>(
        &self,
        sel: char,
        it: &mut PeekMoreIterator<Speekable<I>>,
        accepter: &mut String,
    ) -> Result<bool> {
        if sel != self.0 {
            return Ok(false);
        }

        match it.next() {
            Some((_, '{')) => accepter.push('{'),
            Some((pos, c)) => return Err(Error::UnrecognizedChar(c, pos)),
            None => return Err(Error::UnexpectedEof(it.last_pos())),
        }

        let max = self.1.end_bound();
        let mut counter = 0usize;

        while let Some((pos, c)) = it.peek().copied() {
            if !c.is_digit(16) {
                if self.1.contains(counter) {
                    return Ok(true);
                } else {
                    return Err(Error::UnrecognizedChar(c, pos));
                }
            }
            counter += 1;
            if !(Bound::Unbounded, max).contains(&counter) {
                return Ok(true);
            }
        }

        if self.1.contains(counter) {
            match it.next() {
                Some((_, '{')) => accepter.push('}'),
                Some((pos, c)) => return Err(Error::UnrecognizedChar(c, pos)),
                None => return Err(Error::UnexpectedEof(it.last_pos())),
            }
            Ok(true)
        } else {
            Err(Error::UnexpectedEof(it.last_pos()))
        }
    }
}

pub trait EscapeMatcher {
    fn try_match<I: Iterator<Item = char>>(
        &self,
        sel: char,
        it: &mut PeekMoreIterator<Speekable<I>>,
        accepter: &mut String,
    ) -> Result<bool>;
}

impl EscapeMatcher for char {
    fn try_match<I: Iterator<Item = char>>(
        &self,
        sel: char,
        _: &mut PeekMoreIterator<Speekable<I>>,
        _: &mut String,
    ) -> Result<bool> {
        Ok(sel == *self)
    }
}

impl<E: EscapeMatcher> EscapeMatcher for &E {
    fn try_match<I: Iterator<Item = char>>(
        &self,
        sel: char,
        it: &mut PeekMoreIterator<Speekable<I>>,
        accepter: &mut String,
    ) -> Result<bool> {
        (**self).try_match(sel, it, accepter)
    }
}

impl<E: EscapeMatcher, const N: usize> EscapeMatcher for [E; N] {
    fn try_match<I: Iterator<Item = char>>(
        &self,
        sel: char,
        it: &mut PeekMoreIterator<Speekable<I>>,
        accepter: &mut String,
    ) -> Result<bool> {
        for e in self {
            if e.try_match(sel, it, accepter)? {
                return Ok(true);
            }
        }
        return Ok(false);
    }
}

macro_rules! impl_for_tuple{
    ($($T:ident),* $(,)?) => {

    impl<$($T: EscapeMatcher),*> EscapeMatcher for ($($T,)*){
            #[allow(unused_variables, non_snake_case)] // I'm not typing two idents into a macro input when there's a perfectly good ident already.
            fn try_match<_I: Iterator<Item=char>>(&self, sel: char, it: &mut PeekMoreIterator<Speekable<_I>>, accepter: &mut String) -> Result<bool>{
                let ($($T,)*) = self;

                $({
                    if EscapeMatcher::try_match($T, sel, it, accepter)?{
                        return Ok(true)
                    }
                })*

                Ok(false)
            }
        }
    }
}

impl_for_tuple!();
impl_for_tuple!(A);
impl_for_tuple!(A, B);
impl_for_tuple!(A, B, C);
impl_for_tuple!(A, B, C, D);
impl_for_tuple!(A, B, C, D, E);
impl_for_tuple!(A, B, C, D, E, F);
impl_for_tuple!(A, B, C, D, E, F, G);
impl_for_tuple!(A, B, C, D, E, F, G, H);
impl_for_tuple!(A, B, C, D, E, F, G, H, I);
impl_for_tuple!(A, B, C, D, E, F, G, H, I, J);
impl_for_tuple!(A, B, C, D, E, F, G, H, I, J, K);
impl_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
impl_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M);

pub fn parse_escape<I: Iterator<Item = char>, E: EscapeMatcher>(
    it: &mut PeekMoreIterator<Speekable<I>>,
    escapes: E,
) -> Result<String> {
    match it.next() {
        Some((pos, c)) => {
            let mut v = String::from(c);
            if !escapes.try_match(c, it, &mut v)? {
                Err(Error::UnrecognizedChar(c, pos))
            } else {
                Ok(v)
            }
        }
        None => Err(Error::UnexpectedEof(it.last_pos())),
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum TokenType<IdTy, StrTy, CharTy = StrTy, O = Infallible> {
    Eof,
    Identifier(IdTy),
    String(StrTy),
    Char(CharTy),
    Integer,
    Float,
    Punct,
    Other(O),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Token<IdTy, StrTy, CharTy = StrTy, O = Infallible> {
    pub ty: TokenType<IdTy, StrTy, CharTy, O>,
    pub body: Symbol,
}

pub type LToken<L> =
    Token<<L as Lexer>::IdentTy, <L as Lexer>::StrTy, <L as Lexer>::CharTy, <L as Lexer>::OtherTy>;
pub type LTokenType<L> = TokenType<
    <L as Lexer>::IdentTy,
    <L as Lexer>::StrTy,
    <L as Lexer>::CharTy,
    <L as Lexer>::OtherTy,
>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Group<Lex, H = NoHygiene, GType = Infallible> {
    pub ty: GType,
    pub stream: Vec<Spanned<Lex, H>>,
}

pub type LGroup<L> = Group<Lexeme<L>, <L as Lexer>::Hygiene, <L as Lexer>::GroupType>;
pub enum Lexeme<L: Lexer> {
    Token(LToken<L>),
    Group(Group<Self, L::Hygiene, L::GroupType>),
    Other(L::OtherLex),
}

impl<L: Lexer> Clone for Lexeme<L>
where
    LToken<L>: Clone,
    L::GroupType: Clone,
    L::OtherLex: Clone,
    L::Hygiene: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Lexeme::Token(tok) => Lexeme::Token(tok.clone()),
            Lexeme::Group(group) => Lexeme::Group(group.clone()),
            Lexeme::Other(other) => Self::Other(other.clone()),
        }
    }
}

impl<L: Lexer> PartialEq for Lexeme<L>
where
    LToken<L>: PartialEq,
    L::GroupType: PartialEq,
    L::OtherLex: PartialEq,
    L::Hygiene: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Token(this), Self::Token(other)) => this == other,
            (Self::Group(this), Self::Group(other)) => this == other,
            (Self::Other(this), Self::Other(other)) => this == other,
            _ => false,
        }
    }
}

impl<L: Lexer> Eq for Lexeme<L>
where
    LToken<L>: Eq,
    L::GroupType: Eq,
    L::OtherLex: Eq,
    L::Hygiene: Eq,
{
}

impl<L: Lexer> Hash for Lexeme<L>
where
    LToken<L>: Hash,
    L::GroupType: Hash,
    L::OtherLex: Hash,
    L::Hygiene: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let discrim = core::mem::discriminant(self);
        discrim.hash(state);
        match self {
            Self::Token(this) => this.hash(state),
            Self::Group(this) => this.hash(state),
            Self::Other(this) => this.hash(state),
        }
    }
}

impl<L: Lexer> core::fmt::Debug for Lexeme<L>
where
    LToken<L>: core::fmt::Debug,
    L::GroupType: core::fmt::Debug,
    L::OtherLex: core::fmt::Debug,
    L::Hygiene: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Lexeme::Token(this) => f.debug_tuple("Token").field(this).finish(),
            Lexeme::Group(this) => f.debug_tuple("Group").field(this).finish(),
            Lexeme::Other(this) => this.fmt(f),
        }
    }
}

pub trait Lexer {
    type Hygiene: Default;
    type IdentTy;
    type StrTy;
    type CharTy;
    type OtherTy;
    type GroupType;
    type OtherLex;
    fn is_ident_start(&self, c: char) -> bool;
    fn is_ident_part(&self, c: char) -> bool;
    fn ident_type(&self, id: &str) -> Self::IdentTy;
    fn is_num_sep(&self, c: char) -> bool;
    fn is_num_prefix_frag(&self, c: &str) -> bool;
    fn is_num_prefix(&self, c: &str) -> Option<u32>;
    fn check_valid_num<I: Iterator<Item = char>>(
        &self,
        c: &str,
        it: &mut PeekMoreIterator<Speekable<I>>,
    ) -> Result<()>;
    fn is_float_in_radix(&self, radix: u32) -> bool;
    fn is_float_exp_char(&self, c: char, radix: u32) -> bool;
    fn is_group_start(&self, c: char) -> Option<(Self::GroupType, char)>;
    fn next_token_start<I: Iterator<Item = char>>(
        &self,
        it: &mut PeekMoreIterator<Speekable<I>>,
    ) -> Result<Option<(char, Pos)>>;
    fn punct_list(&self) -> &[&str];
    fn check_valid_punct<I: Iterator<Item = char>>(
        &self,
        c: &str,
        it: &mut PeekMoreIterator<Speekable<I>>,
    ) -> Result<()>;
    fn default_str_type(&self) -> Self::StrTy;
    fn default_char_type(&self) -> Self::CharTy;
    fn check_escape<I: Iterator<Item = char>>(
        &self,
        it: &mut PeekMoreIterator<Speekable<I>>,
    ) -> Result<String>;
    #[allow(unused_variables)] // I don't want `_it` to be silencing the warning downstream in implementors
    fn parse_rest<I: Iterator<Item = char>>(
        &self,
        preceeding: Spanned<LToken<Self>, Self::Hygiene>,
        it: &mut PeekMoreIterator<Speekable<I>>,
    ) -> Result<Spanned<LToken<Self>, Self::Hygiene>> {
        Ok(preceeding)
    }
}

fn do_lexeme_in_group<L: Lexer, I: Iterator<Item = char>>(
    lex: &L,
    iter: &mut PeekMoreIterator<Speekable<I>>,
    end_char: Option<char>,
) -> Result<Spanned<Lexeme<L>, L::Hygiene>> {
    let (c, pos) = match lex.next_token_start(iter)? {
        Some((c, pos)) => (c, pos),
        None => {
            if end_char.is_some() {
                return Err(Error::UnexpectedEof(iter.last_pos()));
            } else {
                let pos = iter.last_pos();
                return Ok(Spanned::new(
                    Lexeme::Token(Token {
                        ty: TokenType::Eof,
                        body: Symbol::intern(""),
                    }),
                    Span::new_simple(pos, pos, iter.file_name()),
                ));
            }
        }
    };

    if Some(c) == end_char {
        return Ok(Spanned::new(
            Lexeme::Token(Token {
                ty: TokenType::Eof,
                body: Symbol::intern(""),
            }),
            Span::new_simple(pos, pos, iter.file_name()),
        ));
    }

    let l = match c {
        c if lex.is_ident_start(c) => {
            let mut s = String::from(c);
            while let Some((_, c)) = iter.peek() {
                if lex.is_ident_part(*c) {
                    s.push(*c);
                    iter.next();
                } else {
                    break;
                }
            }
            let id_ty = lex.ident_type(&s);

            Lexeme::Token(Token {
                ty: TokenType::Identifier(id_ty),
                body: Symbol::intern_by_value(s),
            })
        }
        c @ '0'..='9' => {
            let mut s = String::from(c);
            let mut radix = lex.is_num_prefix(&s);
            if !lex.is_num_prefix_frag(&s) {
                radix = Some(10);
            }
            let mut flt_piece = 0;

            while let Some((_, c)) = iter.peek().copied() {
                if c.is_digit(radix.unwrap_or(10)) {
                    s.push(c);
                    radix.get_or_insert(10);
                } else if lex.is_num_sep(c) {
                    // pass
                } else if radix.is_none() {
                    s.push(c);
                    if let Some(r) = lex.is_num_prefix(&s) {
                        radix = Some(r);
                    } else if lex.is_num_prefix_frag(&s) {
                        continue;
                    } else {
                        s.pop();
                        break;
                    }
                } else if flt_piece == 0 && c == '.' {
                    if !lex.is_float_in_radix(*radix.get_or_insert(10)) {
                        break;
                    }
                    s.push(c);
                    flt_piece = 1;
                } else if flt_piece < 2 && lex.is_float_exp_char(c, radix.unwrap_or(10)) {
                    s.push(c);
                    radix.get_or_insert(10);
                    flt_piece = 2;
                } else {
                    break;
                }

                iter.next();
            }

            lex.check_valid_num(&s, iter)?;
            let tok_ty = if flt_piece == 0 {
                TokenType::Integer
            } else {
                TokenType::Float
            };
            Lexeme::Token(Token {
                ty: tok_ty,
                body: Symbol::intern_by_value(s),
            })
        }
        '\"' => {
            let sty = lex.default_str_type();
            let mut st = String::new();

            loop {
                match iter.next() {
                    Some((_, '"')) => break,
                    Some((_, c)) if c == '\\' => {
                        st.push('\\');
                        st.push_str(&lex.check_escape(iter)?);
                    }
                    Some((_, c)) => st.push(c),
                    None => return Err(Error::UnexpectedEof(iter.last_pos())),
                }
            }

            Lexeme::Token(Token {
                ty: TokenType::String(sty),
                body: Symbol::intern_by_value(st),
            })
        }
        '\'' => {
            let sty = lex.default_char_type();
            let mut st = String::new();

            match iter.next() {
                Some((pos, '\'')) => return Err(Error::UnrecognizedChar('\'', pos)),
                Some((_, c)) if c == '\\' => {
                    st.push('\\');
                    st.push_str(&lex.check_escape(iter)?);
                }
                Some((_, c)) => st.push(c),
                None => return Err(Error::UnexpectedEof(iter.last_pos())),
            }

            Lexeme::Token(Token {
                ty: TokenType::Char(sty),
                body: Symbol::intern_by_value(st),
            })
        }
        c => {
            if let Some((gty, end)) = lex.is_group_start(c) {
                let mut stream = Vec::new();
                loop {
                    let lexeme = do_lexeme_in_group(lex, iter, Some(end))?;
                    let is_eof = matches!(
                        lexeme.body(),
                        Lexeme::Token(Token {
                            ty: TokenType::Eof,
                            ..
                        })
                    );
                    stream.push(lexeme);

                    if is_eof {
                        break;
                    }
                }

                Lexeme::Group(Group { ty: gty, stream })
            } else {
                let mut st = String::from(c);

                let puncts = lex.punct_list();

                if !puncts.iter().any(|&s| s.starts_with(&*st)) {
                    return Err(Error::UnrecognizedChar(c, pos));
                }

                while let Some((_, c)) = iter.peek().copied() {
                    st.push(c);
                    if !puncts.iter().any(|&s| s.starts_with(&*st)) {
                        st.pop();
                        break;
                    }
                    iter.next();
                }
                lex.check_valid_punct(&st, iter)?;

                Lexeme::Token(Token {
                    ty: TokenType::Punct,
                    body: Symbol::intern_by_value(st),
                })
            }
        }
    };

    let span = Span::new_simple(pos, iter.last_pos(), iter.file_name());

    match l {
        Lexeme::Token(tok) => {
            let span = lex.parse_rest(Spanned::new(tok, span), iter)?;
            Ok(span.map(|tok| Lexeme::Token(tok)))
        }
        lex => Ok(Spanned::new(lex, span)),
    }
}

pub fn do_lexeme<L: Lexer, I: Iterator<Item = char>>(
    lex: &L,
    iter: &mut PeekMoreIterator<Speekable<I>>,
) -> Result<Spanned<Lexeme<L>, L::Hygiene>> {
    do_lexeme_in_group(lex, iter, None)
}

pub fn lex_file<L: Lexer, I: Iterator<Item = char>>(
    lex: &L,
    it: I,
    file_name: impl Into<Symbol>,
) -> Result<Vec<Spanned<Lexeme<L>, L::Hygiene>>> {
    let mut lexemes = vec![];
    let mut it = it.speekable(file_name.into()).peekmore();
    loop {
        let lexeme = do_lexeme(lex, &mut it)?;
        let is_eof = matches!(
            lexeme.body(),
            Lexeme::Token(Token {
                ty: TokenType::Eof,
                ..
            })
        );
        lexemes.push(lexeme);

        if is_eof {
            break;
        }
    }
    Ok(lexemes)
}
