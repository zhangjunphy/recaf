use crate::error::Error;
use std::fmt;

/*
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

    pub fn lookup(&self, start: usize, end: usize) -> Option<SrcSpan> {
        self.lookup_offset(start).and_then(|s| {
            self.lookup_offset(end)
                .and_then(|e| Some(SrcSpan { start: s.clone(), end: e.clone() }))
        })
    }
}
*/
