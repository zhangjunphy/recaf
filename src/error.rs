use crate::parser::source_pos::Pos;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Error {
    msg: String,
}

impl Error {
    pub fn new<S: Into<String>>(msg: S) -> Error {
        Error { msg: msg.into() }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: {}", self.msg)
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxError {
    pos: Pos,
    msg: String,
}

impl SyntaxError {
    pub fn new<S: Into<String>>(pos: Pos, msg: S) -> SyntaxError {
        SyntaxError {
            pos,
            msg: msg.into(),
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Syntax error [{}]: {}", self.pos, self.msg)
    }
}
