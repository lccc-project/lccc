use core::fmt;

use crate::interning::Symbol;

#[derive(Clone, Copy, Default)]
pub struct Pos {
    pub row: usize,
    pub col: usize,
    pub idx: usize,
    pub file: Symbol,
}

impl Pos {
    pub fn new(row: usize, col: usize, idx: usize, file: impl Into<Symbol>) -> Self {
        Self { row, col, idx, file: file.into() }
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

pub struct Span {
    pub start: Pos,
    pub end: Pos,
    pub hygiene: HygieneRef,
}

impl Span {
    pub fn new_simple(start: Pos, end: Pos) -> Self {
        Self {
            start,
            end,
            hygiene: HygieneRef::default(),
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?} - {:?})", self.start, self.end)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct HygieneRef {
    pub hygiene_id: u64,
    pub mode: HygieneMode,
    pub edition: RustEdition,
}

impl Default for HygieneRef {
    fn default() -> Self {
        Self {
            hygiene_id: 0,
            mode: HygieneMode::CallSite,
            edition: RustEdition::Rust2021,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum HygieneMode {
    CallSite,
    MixedSite,
    DefSite,
    NoGlobals,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum RustEdition {
    Rust2015,
    Rust2018,
    Rust2021,
    Rust2024,
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
    pos: Pos,
}

#[allow(dead_code)]
impl<I: Iterator<Item = char>> Speekable<I> {
    fn new(inner: I, file_name: impl Into<Symbol>) -> Self {
        Self {
            inner,
            peeked: None,
            pos: Pos::new(1, 1, 0, file_name),
        }
    }

    fn tick(&mut self) {
        if self.peeked.is_none() {
            let c = self.inner.next();
            if let Some(c) = c {
                let next_pos = match c {
                    '\n' => Pos::new(self.pos.row + 1, 1, self.pos.idx + 1, self.pos.file),
                    _ => Pos::new(self.pos.row, self.pos.col + 1, self.pos.idx + 1, self.pos.file),
                };
                self.peeked = Some((self.pos, c));
                self.pos = next_pos;
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
