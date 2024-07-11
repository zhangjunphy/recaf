use crate::source_pos::SrcSpan;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Error {
    range: Option<SrcSpan>,
    msg: String,
}

impl Error {
    pub fn new_span<S: Into<String>>(range: SrcSpan, msg: S) -> Error {
        Error {
            range: Some(range),
            msg: msg.into(),
        }
    }

    pub fn new<S: Into<String>>(msg: S) -> Error {
        Error {
            range: None,
            msg: msg.into(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.range {
            None => write!(f, "error: {}", self.msg),
            Some(range) => write!(f, "error {}: {}", range, self.msg)
        }
        
    }
}
