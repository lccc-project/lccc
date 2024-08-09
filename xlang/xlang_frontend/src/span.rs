use core::ops::{Deref, DerefMut};

use xlang::abi::ops::{ChangeOutputType, Residual, Try};

use xlang::abi::try_;

use crate::helpers::FetchIncrement;
use crate::symbol::Symbol;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Pos {
    row_col: u32,
}

impl Pos {
    pub const fn new(row: u32, col: u32) -> Self {
        [()][(row > 0xFFFFF) as usize];
        [()][(col > 0xFFF) as usize];
        Self {
            row_col: row | (col << 20),
        }
    }

    pub const fn synthetic() -> Self {
        Self { row_col: !0 }
    }

    pub const fn is_synthetic(&self) -> bool {
        self.row_col == !0
    }

    #[allow(dead_code)]
    pub const fn row(self) -> u32 {
        self.row_col & 0xFFFFF
    }

    #[allow(dead_code)]
    pub const fn col(self) -> u32 {
        self.row_col >> 20
    }

    pub const fn next_col(mut self) -> Self {
        Self::new(self.row(), self.col() + 1)
    }
}

impl core::fmt::Debug for Pos {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}:{}", self.row(), self.col())
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct NoHygiene;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Span<H = NoHygiene> {
    begin: Pos,
    end: Pos,
    file: Symbol,
    hygiene: H,
}

impl<H> Span<H> {
    pub fn new_simple<S: Into<Symbol>>(begin: Pos, end: Pos, file: S) -> Self
    where
        H: Default,
    {
        Self::new_with_hygiene(begin, end, file, H::default())
    }

    pub fn new_with_hygiene<S: Into<Symbol>>(begin: Pos, end: Pos, file: S, hygiene: H) -> Self {
        Self {
            begin,
            end,
            file: file.into(),
            hygiene,
        }
    }

    pub fn synthetic() -> Self
    where
        H: Default,
    {
        Self::new_simple(Pos::synthetic(), Pos::synthetic(), "")
    }

    pub fn synthetic_with_hygiene(hygiene: H) -> Self {
        Self::new_with_hygiene(Pos::synthetic(), Pos::synthetic(), "", hygiene)
    }

    pub fn file(&self) -> Symbol {
        self.file
    }

    pub fn begin(&self) -> Pos {
        self.begin
    }

    pub fn end(&self) -> Pos {
        self.end
    }

    pub fn hygiene(&self) -> &H {
        &self.hygiene
    }

    pub fn is_empty(&self) -> bool {
        self.begin == self.end
    }

    pub fn is_synthetic(&self) -> bool {
        self.is_empty() && self.begin.is_synthetic()
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Spanned<T, H = NoHygiene> {
    val: T,
    span: Span<H>,
}

impl<T, H> Deref for Spanned<T, H> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.val
    }
}

impl<T, H> DerefMut for Spanned<T, H> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.val
    }
}

impl<T, H> Spanned<T, H> {
    pub const fn new(val: T, span: Span<H>) -> Self {
        Self { val, span }
    }

    pub fn into_inner(self) -> T {
        self.val
    }

    pub const fn span(&self) -> &Span<H> {
        &self.span
    }

    pub const fn body(&self) -> &T {
        &self.val
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U, H> {
        let Self { val, span } = self;

        Spanned::new(f(val), span)
    }

    pub fn try_map<R: Try, F: FnOnce(T) -> R>(
        self,
        f: F,
    ) -> ChangeOutputType<R, Spanned<R::Output, H>>
    where
        R::Residual: Residual<Spanned<R::Output, H>>,
    {
        let Self { val, span } = self;

        Try::from_output(Spanned::new(try_!(f(val)), span))
    }

    pub fn copy_span<U, F: FnOnce(&T) -> U>(&self, f: F) -> Spanned<U, H>
    where
        H: Copy,
    {
        let Self { val, span } = self;

        Spanned::new(f(val), *span)
    }

    pub fn try_copy_span<R: Try, F: FnOnce(&T) -> R>(
        &self,
        f: F,
    ) -> ChangeOutputType<R, Spanned<R::Output, H>>
    where
        R::Residual: Residual<Spanned<R::Output, H>>,
        H: Copy,
    {
        let Self { val, span } = self;

        Try::from_output(Spanned::new(try_!(f(val)), *span))
    }
}

impl<A, B, H> Spanned<(A, B), H> {
    pub fn unzip(self) -> (Spanned<A, H>, Spanned<B, H>)
    where
        H: Copy,
    {
        let Self { val, span } = self;

        (Spanned::new(val.0, span), Spanned::new(val.1, span))
    }
}

pub trait Speekerator: Iterator<Item = char> + Sized {
    fn speekable(self, file_name: impl Into<Symbol>) -> Speekable<Self>;
}

impl<I: Iterator<Item = char>> Speekerator for I {
    fn speekable(self, file_name: impl Into<Symbol>) -> Speekable<Self> {
        Speekable::new(self, file_name)
    }
}

pub struct Speekable<I: Iterator<Item = char>> {
    inner: I,
    peeked: Option<(Pos, char)>,
    index: usize,
    pos: Pos,
    fname: Symbol,
    row_map: Vec<usize>,
}

#[allow(dead_code)]
impl<I: Iterator<Item = char>> Speekable<I> {
    fn new(inner: I, fname: impl Into<Symbol>) -> Self {
        Self {
            inner,
            index: 0,
            peeked: None,
            pos: Pos::new(1, 1),
            fname: fname.into(),
            row_map: vec![0],
        }
    }

    pub fn file_name(&self) -> Symbol {
        self.fname
    }

    pub fn row_map(&self) -> &[usize] {
        &self.row_map
    }

    fn tick(&mut self) {
        let mut expect_newline = false;
        while self.peeked.is_none() {
            let c = self.inner.next();
            if expect_newline && c != Some('\n') {
                panic!("todo: diag");
            }
            if let Some('\r') = c {
                self.index += 1; // Yeet the carriage return, try again
                expect_newline = true;
            } else if let Some(c) = c {
                let idx = self.index.fetch_increment();
                let next_pos = match c {
                    '\n' => Pos::new(self.pos.row() + 1, 1),
                    _ => Pos::new(self.pos.row(), self.pos.col() + 1),
                };

                if next_pos.row() != self.pos.row() {
                    self.row_map.push(idx);
                }

                self.peeked = Some((self.pos, c));
                self.pos = next_pos;
            } else {
                break;
            }
        }
    }

    pub fn peek(&mut self) -> Option<(Pos, char)> {
        self.tick();
        self.peeked.as_ref().map(|&(pos, x)| (pos, x))
    }

    pub fn last_pos(&self) -> Pos {
        self.pos
    }
}

impl<I: Iterator<Item = char>> Iterator for Speekable<I> {
    type Item = (Pos, char);

    fn next(&mut self) -> Option<(Pos, char)> {
        self.tick();
        self.peeked.take()
    }
}
