use crate::error::Error;
use crate::source_pos::{Pos, SrcSpan};
use regex::Regex;
use std::collections::BTreeSet;

#[derive(Clone, Debug, PartialEq, Eq)]
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
    Colon,

    LParen,
    RParen,
    LBrack,
    RBrack,
    LCurly,
    RCurly,

    Semicolon,
}

pub struct Lexer<'input> {
    sequence: &'input str,
    pos: Pos,
    token_map: Vec<(&'input str, Tok)>,
    whitespace: BTreeSet<char>,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
type TokenItem = Spanned<Tok, Pos, Error>;

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            sequence: input,
            pos: Pos::new(0, 0, 0),
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
            (":", Tok::Colon),
            ("(", Tok::LParen),
            (")", Tok::RParen),
            ("[", Tok::LBrack),
            ("]", Tok::RBrack),
            ("{", Tok::LCurly),
            ("}", Tok::RCurly),
            (";", Tok::Semicolon),
        ]
    }

    fn scan(&mut self) -> Option<TokenItem> {
        self.consume_non_syntatic();
        let to_scan = self.to_scan();
        if to_scan.is_empty() {
            return None;
        }
        let next_char = to_scan.chars().next().unwrap();

        if to_scan.starts_with('"') {
            return self.match_string();
        } else if to_scan.starts_with('\'') {
            return self.match_char();
        } else if let Some(token) = self.match_token() {
            return Some(token);
        } else if let Some(id) = self.match_id() {
            return Some(id);
        } else {
            let start = self.pos;
            self.advance(1);
            return Some(Err(Error::new_span(
                start,
                self.pos,
                format!("Unable to handle character: '{next_char}'"),
            )));
        }
    }

    fn to_scan(&self) -> &str {
        &self.sequence[self.pos.offset..]
    }

    fn peek_next(&self) -> Option<(char, SrcSpan)> {
        if self.sequence.len() <= self.pos.offset {
            return None;
        }
        let start = self.pos;
        let c = self.to_scan().chars().next().unwrap();
        let end = self.forward_pos(1).unwrap();
        return Some((c, SrcSpan::new(start, end)));
    }

    fn forward_pos(&self, nchars: usize) -> Result<Pos, usize> {
        let mut ci = self.to_scan().char_indices().peekable();
        let mut res = self.pos.clone();
        for i in 0..nchars {
            match ci.next() {
                None => return Err(i),
                Some((_, c)) => {
                    if c == '\n' {
                        res.row += 1;
                        res.col = 0;
                    } else {
                        res.col += 1;
                    }
                }
            }
        }
        if let Some((next_offset, _)) = ci.peek() {
            res.offset += *next_offset;
        } else {
            // We have reached the EOF.
            res.offset = self.sequence.len();
        }
        Ok(res)
    }

    fn advance(&mut self, nchars: usize) -> Pos {
        let pos = self.forward_pos(nchars).and_then(|p| {
            self.pos = p;
            Ok(p)
        });
        assert!(pos.is_ok());
        pos.unwrap()
    }

    fn consume_non_syntatic(&mut self) -> bool {
        let mut res: bool = false;
        loop {
            let consumed =
                self.consume_space() + self.consume_line_comment() + self.consume_block_comment();
            res = if consumed > 0 { true } else { res };
            if consumed == 0 {
                break;
            }
        }
        res
    }
    fn consume_space(&mut self) -> usize {
        let start = self.pos;
        loop {
            let next = self.peek_next();
            match next {
                None => break,
                Some((c, _)) => {
                    if self.whitespace.contains(&c) {
                        self.advance(1);
                    } else {
                        break;
                    }
                }
            }
        }
        self.pos.offset - start.offset
    }
    fn consume_line_comment(&mut self) -> usize {
        if !self.to_scan().starts_with("//") {
            return 0;
        }

        let start = self.pos;
        self.advance(2);
        while !self.to_scan().is_empty() && !self.to_scan().starts_with('\n') {
            self.advance(1);
        }
        if self.to_scan().starts_with('\n') {
            self.advance(1);
        } else {
            assert!(self.to_scan().is_empty());
        }
        return self.pos.offset - start.offset;
    }
    fn consume_block_comment(&mut self) -> usize {
        if !self.to_scan().starts_with("/*") {
            return 0;
        }

        let start = self.pos;
        assert!(self.to_scan().starts_with("/*"));
        let mut depth: i32 = 0;
        loop {
            if self.to_scan().starts_with("/*") {
                self.advance(2);
                depth += 1;
            } else if self.to_scan().starts_with("*/") {
                self.advance(2);
                depth -= 1;
            }

            if depth <= 0 {
                break;
            }
        }
        assert!(depth == 0);
        return self.pos.offset - start.offset;
    }

    fn match_token(&mut self) -> Option<TokenItem> {
        let mut res: Option<TokenItem> = None;
        let mut nchars = 0;
        for (pattern, tok) in &self.token_map {
            if self.to_scan().starts_with(pattern) {
                nchars = pattern.chars().count();
                let end = self.forward_pos(nchars);
                assert!(end.is_ok());
                res = Some(Ok((self.pos, tok.clone(), end.unwrap())));
                break;
            }
        }
        self.advance(nchars);
        res
    }

    fn match_escape_char(&mut self) -> Option<char> {
        let to_scan = self.to_scan();
        if to_scan.starts_with("\\n") {
            self.advance(2);
            return Some('\n');
        } else if to_scan.starts_with("\\\\") {
            self.advance(2);
            return Some('\\');
        } else if to_scan.starts_with("\\t") {
            self.advance(2);
            return Some('\t');
        } else if to_scan.starts_with("\\'") {
            self.advance(2);
            return Some('\'');
        } else if to_scan.starts_with("\\\"") {
            self.advance(2);
            return Some('"');
        } else {
            return None;
        }
    }
    fn match_id(&mut self) -> Option<TokenItem> {
        let re = Regex::new(r"^[A-Za-z_][0-9A-Za-z_]*").unwrap();
        let m = re.find(self.to_scan())?;
        let id = m.as_str().to_string();
        let start = self.pos;
        let end = self.advance(id.chars().count());
        Some(Ok((start, Tok::ID(id), end)))
    }
    fn match_char(&mut self) -> Option<TokenItem> {
        let start = self.pos;
        let (lquote, _) = self.peek_next()?;
        if lquote != '\'' {
            return None;
        }
        self.advance(1);
        let res: char;
        if let Some(ec) = self.match_escape_char() {
            res = ec;
        } else if self.to_scan().starts_with('\\') {
            let end = self.forward_pos(1);
            return Some(Err(Error::new_span(
                start,
                end.unwrap(),
                "Invalid escape sequence.",
            )));
        } else if let Some((c, _)) = self.peek_next() {
            res = c;
            self.advance(1);
        } else {
            return Some(Err(Error::new_span(
                start,
                self.pos,
                "Char literal reaches end of file.",
            )));
        }
        let rquote = self.peek_next();
        if rquote.is_none() || rquote.unwrap().0 != '\'' {
            return Some(Err(Error::new_span(
                start,
                self.pos,
                "Char literal not enclosed.",
            )));
        }
        self.advance(1);
        Some(Ok((start, Tok::Char(res), self.pos)))
    }
    fn match_string(&mut self) -> Option<TokenItem> {
        let start = self.pos;
        let (lquote, _) = self.peek_next()?;
        if lquote != '\"' {
            return None;
        }
        self.advance(1);
        let mut res = String::new();
        loop {
            if self.to_scan().starts_with('\"') {
                self.advance(1);
                break;
            } else if let Some(c) = self.match_escape_char() {
                res.push(c);
            } else if self.to_scan().starts_with('\\') {
                let end = self.forward_pos(1).unwrap();
                return Some(Err(Error::new_span(
                    self.pos,
                    end,
                    "Invalid escape sequence.",
                )));
            } else if let Some(c) = self.to_scan().chars().next() {
                self.advance(1);
                res.push(c);
            } else {
                return Some(Err(Error::new_span(
                    start,
                    self.pos,
                    "String literal reaches end of file.",
                )));
            }
        }
        Some(Ok((start, Tok::String(res), self.pos)))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, Pos, Error>;

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
