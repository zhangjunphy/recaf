use crate::parser::source_pos::SrcRange;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Error {
    range: Option<SrcRange>,
    msg: String,
}

impl Error {
    pub fn new<S: Into<String>>(range: Option<SrcRange>, msg: S) -> Error {
        Error {
            range,
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
