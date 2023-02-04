#[derive(Debug)]
pub struct Pos {
    row: usize,
    col: usize,
}

#[derive(Debug)]
pub struct Span {
    start: Pos,
    end: Pos,
}
