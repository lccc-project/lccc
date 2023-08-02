use core::{
    cmp::Ordering,
    fmt::{self, Debug},
};

use core::ops::{Deref, DerefMut};

use crate::{helpers::FetchIncrement, interning::Symbol};

#[derive(Clone, Copy, Hash, Eq, Default)]
pub struct Pos {
    pub row: u32,
    pub col: u32,
}

impl Pos {
    pub const fn new(row: u32, col: u32) -> Self {
        Self { row, col }
    }

    pub const fn synthetic() -> Self {
        Self { row: !0, col: !0 }
    }

    pub const fn row(self) -> u32 {
        self.row
    }

    pub const fn col(self) -> u32 {
        self.col
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl PartialEq for Pos {
    fn eq(&self, other: &Self) -> bool {
        self.row == other.row && self.col == other.col
    }
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.row.cmp(&other.row) {
            Ordering::Equal => {}
            x => return Some(x),
        }
        Some(self.col.cmp(&other.col))
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
    pub file: Symbol,
    pub hygiene: HygieneRef,
}

impl Span {
    pub fn new_simple(start: Pos, end: Pos, file: impl Into<Symbol>) -> Self {
        Self {
            start,
            end,
            file: file.into(),
            hygiene: HygieneRef::default(),
        }
    }

    pub fn empty() -> Self {
        Self::new_simple(Pos::default(), Pos::default(), Symbol::default())
    }

    pub fn synthetic() -> Self {
        Self::new_simple(Pos::synthetic(), Pos::synthetic(), Symbol::default())
    }

    pub fn is_empty(self) -> bool {
        self.start == self.end
    }

    pub fn is_synthetic(self) -> bool {
        self.is_empty() && self.start == Pos::synthetic()
    }

    pub fn between(start: Self, end: Self) -> Self {
        assert_eq!(start.file, end.file);
        assert_eq!(start.hygiene, end.hygiene);
        Self {
            start: start.start,
            end: end.end,
            file: start.file,
            hygiene: start.hygiene,
        }
    }

    pub fn with_start(mut self, nstart: Pos) -> Self {
        self.start = nstart;
        self
    }

    pub fn after(mut self) -> Self {
        self.start = self.end;
        self
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_synthetic() {
            f.write_str("(synthetic)")
        } else {
            write!(f, "({:?} - {:?} [{:?}])", self.start, self.end, self.file)
        }
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Spanned<T> {
    pub body: T,
    pub span: Span,
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?}) ", self.span)?;
        self.body.fmt(f)?;
        Ok(())
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.body
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.body
    }
}

impl<T> Spanned<T> {
    pub fn copy_span<U, F: FnOnce(&T) -> U>(&self, f: F) -> Spanned<U> {
        Spanned {
            body: f(&self.body),
            span: self.span,
        }
    }

    pub fn try_copy_span<U, E, F: FnOnce(&T) -> Result<U, E>>(
        &self,
        f: F,
    ) -> Result<Spanned<U>, E> {
        match f(&self.body) {
            Ok(body) => Ok(Spanned {
                body,
                span: self.span,
            }),
            Err(e) => Err(e),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct HygieneRef(pub u64);

impl Default for HygieneRef {
    fn default() -> Self {
        Self::simple_with_edition(RustEdition::Rust2021)
    }
}

impl HygieneRef {
    pub const fn simple_with_edition(edition: RustEdition) -> Self {
        Self::new(0, HygieneMode::CallSite, edition)
    }

    pub const fn new(xref_id: u64, mode: HygieneMode, edition: RustEdition) -> Self {
        let mode = mode as u8;
        let edition = edition as u16;

        let val = ((mode as u64) << 60) | ((edition as u64) << 48) | (xref_id & 0xFFFFFFFFFFFF);

        Self(val)
    }

    #[allow(dead_code)]
    pub const fn xref_id(self) -> u64 {
        self.0 & 0xFFFFFFFFFFFF
    }

    #[allow(dead_code)]
    pub const fn mode(self) -> Option<HygieneMode> {
        let val = self.0 >> 60;

        HygieneMode::from_val(val as u8)
    }

    #[allow(dead_code)]
    pub const fn edition(self) -> Option<RustEdition> {
        let val = (self.0 >> 48) & 0xFFF;

        RustEdition::from_val(val as u16)
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum HygieneMode {
    CallSite,
    MixedSite,
    DefSite,
    NoGlobals,
}

union Transmuter<T: Copy, U: Copy> {
    val: T,
    res: U,
}

impl HygieneMode {
    #[allow(dead_code)]
    pub const fn from_val(val: u8) -> Option<HygieneMode> {
        if val > 3 {
            None
        } else {
            let val = Transmuter { val };

            Some(unsafe { val.res })
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum RustEdition {
    Rust2015,
    Rust2018,
    Rust2021,
    Rust2024,
}

impl RustEdition {
    #[allow(dead_code)]
    pub const fn from_val(val: u16) -> Option<RustEdition> {
        if val > 3 {
            None
        } else {
            let val = Transmuter { val };

            Some(unsafe { val.res })
        }
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
