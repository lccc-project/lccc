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
pub struct Span<H> {
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
pub struct Spanned<T, H> {
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
