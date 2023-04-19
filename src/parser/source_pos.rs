use crate::error::Error;
use std::fmt;
use std::fs;
use std::io::{BufRead, BufReader};

pub struct Pos {
    pub row: usize,
    pub col: usize,
}

pub struct SrcRange {
    pub start: Pos,
    pub end: Pos,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}, {}", self.row, self.col)
    }
}

pub struct SourcePosMap {
    mappings: Vec<Pos>,
}

impl SourcePosMap {
    pub fn build(f: &fs::File) -> Result<SourcePosMap, Error> {
        let reader = BufReader::new(f);
        let mut res = SourcePosMap {
            mappings: Vec::new(),
        };
        for (row, l) in reader.lines().enumerate() {
            if let Ok(line) = l {
                for (col, _) in line.chars().enumerate() {
                    res.mappings.push(Pos { row, col });
                }
            } else {
                return Err(Error::new(""));
            }
        }

        return Ok(res);
    }

    pub fn lookup_offset(&self, offset: usize) -> Option<&Pos> {
        self.mappings.get(offset)
    }

    pub fn lookup(&self, start: usize, end: usize) -> Option<&SrcRange> {
        self.lookup_offset(start).and_then(|s| {
            self.lookup_offset(end)
                .and_then(|e| Some(SrcRange { start: s, end: e }))
        });
    }
}
