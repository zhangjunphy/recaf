use crate::error::Error;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Pos {
    pub row: usize,
    pub col: usize,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({},{})", self.row, self.col)
    }
}

#[derive(Debug, Clone)]
pub struct SrcRange {
    pub start: Pos,
    pub end: Pos,
}

impl fmt::Display for SrcRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}-{}]", self.start, self.end)
    }
}

pub struct SourcePosMap {
    mappings: Vec<Pos>,
}

impl SourcePosMap {
    pub fn new(s: &String) -> Result<SourcePosMap, Error> {
        let mut res = SourcePosMap {
            mappings: Vec::new(),
        };
        for (row, l) in s.lines().enumerate() {
            for (col, _) in l.chars().enumerate() {
                res.mappings.push(Pos { row, col });
            }
        }

        return Ok(res);
    }

    pub fn lookup_offset(&self, offset: usize) -> Option<&Pos> {
        self.mappings.get(offset)
    }

    pub fn lookup(&self, start: usize, end: usize) -> Option<SrcRange> {
        self.lookup_offset(start).and_then(|s| {
            self.lookup_offset(end)
                .and_then(|e| Some(SrcRange { start: s.clone(), end: e.clone() }))
        })
    }
}
