use crate::error::Error;
use crate::source_pos as sp;
use std::collections::BTreeSet;
use regex::Regex;

#[derive(Clone)]
pub enum Tok {
    Import,
    If,
    For,
    While,
    Return,
    Break,
    Continue,

    Int,
    Bool,
    Void,

    ID(String),

    True,
    False,
    HexLiteral(String),
    DecLiteral(String),
    Char(char),
    String(String),

    AssignOp(String),
    IncrementOp(String),
    ArithOp(String),
    RelOp(String),
    EqOp(String),
    CondOp(String),

    QMark,
    SemiColon,

    LParen,
    RParen,
    LBrack,
    RBrack,
    LCurly,
    RCurly,
}

pub struct Lexer<'input> {
    sequence: &'input str,
    pos: sp::Pos,
    token_map: Vec<(&'input str, Tok)>,
    whitespace: BTreeSet<char>,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
type TokenItem = Spanned<Tok, sp::Pos, Error>;

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            sequence: input,
            pos: sp::Pos::new(0, 0, 0),
            token_map: Lexer::token_map(),
            whitespace: BTreeSet::from([' ', '\n', '\t', '\r']),
        }
    }

    fn token_map() -> Vec<(&'input str, Tok)> {
        // We have to be careful to put longer patterns to the front so they get matched
        // before some of its subsequences.
        // There might be some better design but this should work for the moment.
        vec![
            ("if", Tok::If),
            ("import", Tok::Import),
            ("for", Tok::For),
            ("while", Tok::While),
            ("return", Tok::Return),
            ("break", Tok::Break),
            ("continue", Tok::Continue),
            ("int", Tok::Int),
            ("bool", Tok::Bool),
            ("void", Tok::Void),
            ("true", Tok::True),
            ("false", Tok::False),
            ("==", Tok::EqOp("==".to_string())),
            ("!=", Tok::EqOp("!=".to_string())),
            ("+=", Tok::AssignOp("+=".to_string())),
            ("-=", Tok::AssignOp("-=".to_string())),
            ("=", Tok::AssignOp("=".to_string())),
            ("++", Tok::IncrementOp("++".to_string())),
            ("--", Tok::IncrementOp("--".to_string())),
            ("+", Tok::ArithOp("+".to_string())),
            ("-", Tok::ArithOp("-".to_string())),
            ("*", Tok::ArithOp("*".to_string())),
            ("/", Tok::ArithOp("/".to_string())),
            ("<=", Tok::RelOp("<=".to_string())),
            (">=", Tok::RelOp(">=".to_string())),
            ("<", Tok::RelOp("<".to_string())),
            (">", Tok::RelOp(">".to_string())),
            ("&&", Tok::CondOp("&&".to_string())),
            ("||", Tok::CondOp("||".to_string())),
            ("?", Tok::QMark),
            (":", Tok::SemiColon),
            ("(", Tok::LParen),
            (")", Tok::RParen),
            ("[", Tok::LBrack),
            ("]", Tok::RBrack),
            ("{", Tok::LCurly),
            ("}", Tok::RCurly),
        ]
    }

    fn scan(&mut self) -> Option<TokenItem> {
        self.consume_space();
        let to_scan = self.to_scan();
        if to_scan.starts_with('"') {
            return self.match_string();
        } else if to_scan.starts_with('\'') {
            return self.match_char();
        } else if to_scan.starts_with("/*") {
            return self.match_block_comment();
        } else if to_scan.starts_with("//") {
            return self.match_line_comment();
        } else if let Some(token) = self.match_token() {
            return Some(token);
        } else if let Some(id) = self.match_id() {
            return Some(id);
        }
        None
    }

    fn to_scan(&self) -> &str {
        &self.sequence[self.pos.offset..]
    }

    fn peek_next(&self) -> Option<char> {
        if self.sequence.len() <= self.pos.offset {
            return None;
        }
        self.sequence[self.pos.offset..].chars().next()
    }

    fn forward_pos(&self, nchars: usize) -> Result<sp::Pos, usize> {
        let to_scan = self.to_scan();
        let char_count = to_scan.chars().count();
        let mut res = self.pos.clone();
        for i in 0..nchars {
            match to_scan.char_indices().nth(i) {
                None => return Err(i),
                Some((offset, char)) => {
                    if char == '\n' {
                        res.row += 1;
                        res.col = 0;
                    } else {
                        res.col += 1;
                    }
                    res.offset += offset;
                    if i + 1 < char_count {
                        let (next_offset, _) = to_scan.char_indices().nth(i+1).unwrap();
                        res.offset = next_offset;
                    } else {
                        res.offset = self.to_scan().len();
                    }
                }
            }
        }
        Ok(res)
    }

    fn advance(&mut self, nchars: usize) -> Result<sp::Pos, usize> {
        self.forward_pos(nchars).and_then(|p| {
            self.pos = p;
            Ok(p)
        })
    }

    fn consume_space(&mut self) {
        loop {
            let next = self.peek_next();
            match next {
                None => return,
                Some(c) =>
                    if self.whitespace.contains(&c) {
                        assert!(self.advance(1).is_ok());
                    }
            }
        }
    }

    fn match_token(&mut self) -> Option<TokenItem> {
        if self.sequence.chars().count() <= self.pos.offset {
            return None;
        }
        let mut res: Option<TokenItem> = None;
        let mut nchars = 0;
        for (pattern, tok) in &self.token_map {
            if self.to_scan().starts_with(pattern) {
                nchars = pattern.chars().count();
                let end = self.forward_pos(nchars);
                assert!(end.is_ok());
                res = Some(Ok((self.pos, tok.clone(), end.unwrap())));
            }
        }
        assert!(self.advance(nchars).is_ok());
        res
    }

    fn match_escape_char(&mut self) -> Option<char> {
        let to_scan = self.to_scan();
        if to_scan.starts_with("\\n") {
            self.advance(2).ok()?;
            return Some('\n');
        } else if to_scan.starts_with("\\\\") {
            self.advance(2).ok()?;
            return Some('\\');
        } else if to_scan.starts_with("\\t") {
            self.advance(2).ok()?;
            return Some('\t');
        } else if to_scan.starts_with("\\'") {
            self.advance(2).ok()?;
            return Some('\'');
        } else if to_scan.starts_with("\\\"") {
            self.advance(2).ok()?;
            return Some('"');
        } else {
            return None
        }
    }
    fn match_id(&mut self) -> Option<TokenItem> {
        let re = Regex::new(r"^[A-Za-z_][0-9A-Za-z_]*").unwrap();
        let m = re.find(self.to_scan())?;
        let id = m.as_str().to_string();
        let start = self.pos;
        let end = self.advance(id.chars().count()).ok()?;
        Some(Ok((start, Tok::ID(id), end)))
    }
    fn match_char(&mut self) -> Option<TokenItem> {
        let start = self.pos;
        let lquote = self.peek_next();
        assert!(lquote.is_some() && lquote.unwrap() == '\'');
        self.advance(1).ok()?;
        let c: char;
        if let Some(ec) = self.match_escape_char() {
            c = ec;
        } else if self.to_scan().starts_with('\\') {
            return None
        } else {
            c = self.peek_next()?;
            self.advance(1).ok()?;
        }
        let rquote = self.peek_next()?;
        if rquote != '\'' {
            return Some(Err(Error::new("Invalid escape sequence.")));
        }
        Some(Ok((start, Tok::Char(c), self.pos)))
    }
    fn match_string(&mut self) -> Option<TokenItem> {
        let start = self.pos;
        let lquote = self.peek_next();
        assert!(lquote.is_some() && lquote.unwrap() == '\"');
        self.advance(1).ok()?;
        let mut res = String::new();
        loop {
            if self.to_scan().is_empty() {
                return Some(Err(Error::new("String literal reaches end of file.")));
            }

            if self.to_scan().starts_with('\"') {
                self.advance(1).ok()?;
                break;
            } else if let Some(c) = self.match_escape_char() {
                res.push(c);
            } else if self.to_scan().starts_with('\\') {
                return Some(Err(Error::new("Invalid escape sequence.")));
            } else {
                res.push(self.to_scan().chars().next()?);
            }
        }
        Some(Ok((start, Tok::String(res), self.pos)))
    }
    fn match_line_comment(&mut self) -> Option<TokenItem> {
        assert!(self.to_scan().starts_with("//"));
        self.advance(2).ok()?;
        while !self.to_scan().starts_with('\n') {
            self.advance(1).ok()?;
        }
        None
    }
    fn match_block_comment(&mut self) -> Option<TokenItem> {
        assert!(self.to_scan().starts_with("/*"));
        let mut depth: i32 = 0;
        loop {
            if self.to_scan().starts_with("/*") {
                self.advance(2).ok()?;
                depth += 1;
            } else if self.to_scan().starts_with("*/") {
                self.advance(2).ok()?;
                depth -= 1;
            }

            if depth <= 0 {
                break;
            }
        }
        assert!(depth == 0);
        None
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, sp::Pos, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan()
    }
}

/*
extern {
    type Location = usize;
    type Error = lexer::LexicalError;

    enum lexer::Tok {
        " " => lexer::Tok::Space,
        "\t" => lexer::Tok::Tab,
        "\n" => lexer::Tok::Linefeed,
    }
}
*/
