use core::ops::{Deref, DerefMut};

use xlang::abi::ops::{ChangeOutputType, Residual, Try};

use xlang::abi::try_;

use crate::symbol::Symbol;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Pos {
    row: u32,
    col: u32,
}

impl Pos {
    pub const fn new(row: u32, col: u32) -> Self {
        Self { row, col }
    }

    pub const fn synthetic() -> Self {
        Self::new(!0, !0)
    }

    pub const fn row(self) -> u32 {
        self.row
    }

    pub const fn col(self) -> u32 {
        self.col
    }

    pub const fn is_synthetic(self) -> bool {
        self.row == !0 && self.col == !0
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
                    '\n' => Pos::new(self.pos.row + 1, 1),
                    _ => Pos::new(self.pos.row, self.pos.col + 1),
                };

                if next_pos.row != self.pos.row {
                    self.row_map.push(idx);
                }

                self.peeked = Some((self.pos, c));
                self.pos = next_pos;
            } else {
                break;
            }
        }
    }

    pub fn speek(&mut self) -> Option<&(Pos, char)> {
        self.tick();
        self.peeked.as_ref()
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.tick();
        self.peeked.as_ref().map(|(_, x)| x)
    }

    pub fn snext(&mut self) -> Option<(Pos, char)> {
        self.tick();
        self.peeked.take()
    }

    pub fn last_pos(&self) -> Pos {
        self.pos
    }
}

impl<I: Iterator<Item = char>> Iterator for Speekable<I> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.tick();
        self.peeked.take().map(|(_, x)| x)
    }
}
