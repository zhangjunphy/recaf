use std::fmt;

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub offset: usize, // offset of bytes
    pub row: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(offset: usize, row: usize, col: usize) -> Pos {
        Pos{offset, row, col}
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({},{})", self.row, self.col)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SrcSpan {
    pub start: Pos,
    pub end: Pos,
}

impl fmt::Display for SrcSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}-{}]", self.start, self.end)
    }
}
