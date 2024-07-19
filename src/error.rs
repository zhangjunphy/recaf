use crate::source_pos::SrcSpan;
use std::fmt;

#[macro_export]
macro_rules! err_pos {
    ($start:expr, $end:expr, $($arg:tt)*) => {{
        let res = Err(Error::new(Some(SrcSpan::new($start, $end)), format!($($arg)*)));
        res
    }}
}
#[macro_export]
macro_rules! err_span {
    ($e:expr, $($arg:tt)*) => {{
        let res = Err(Error::new($e, format!($($arg)*)));
        res
    }}
}
#[macro_export]
macro_rules! err {
    ($($arg:tt)*) => {{
        let res = Err(Error::new(None, format!($($arg)*)));
        res
    }}
}

#[derive(Debug, Clone)]
pub struct Error {
    pub span: Option<SrcSpan>,
    pub msg: String,
}

impl Error {
    pub fn new<S: Into<String>>(span: Option<SrcSpan>, msg: S) -> Error {
        Error {
            span,
            msg: msg.into(),
        }
    }

    pub fn msg<S: Into<String>>(msg: S) -> Error {
        Error {
            span: None,
            msg: msg.into(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.span {
            None => write!(f, "error: {}", self.msg),
            Some(range) => write!(f, "error {}: {}", range, self.msg),
        }
    }
}
