#[derive(Debug)]
pub struct Pos {
    row: usize,
    col: usize,
}

#[derive(Debug)]
pub struct Span {
    start: Pos,
    end: Pos,
    hygiene: HygieneMode,
}

#[derive(Debug, PartialEq,Eq,Hash)]
pub struct HygieneRef{
    pub hygiene_id: u64,
    pub mode: HygieneMode,
    pub edition: RustEdition,
}

#[derive(Debug, PartialEq,Eq,Hash)]
pub enum HygieneMode{
    CallSite,
    MixedSite,
    DefSite,
    NoGlobals
}

#[derive(Debug, PartialEq,Eq,Hash)]
pub enum RustEdition{
    Rust2015,
    Rust2018,
    Rust2021,
    Rust2024
}