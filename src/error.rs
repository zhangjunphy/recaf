use crate::source_pos::SrcSpan;
use std::fmt;

#[macro_export]
macro_rules! err_pos {
    ($start:expr, $end:expr, $($arg:tt)*) => {{
        Error::new(Some(SrcSpan::new($start, $end)), format!($($arg)*))
    }}
}
#[macro_export]
macro_rules! err_span {
    ($e:expr, $($arg:tt)*) => {{
        Error::new($e, format!($($arg)*))
    }}
}
#[macro_export]
macro_rules! err {
    ($($arg:tt)*) => {{
        Error::new(None, format!($($arg)*))
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
