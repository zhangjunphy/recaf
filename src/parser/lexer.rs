use crate::error::Error;
use crate::source_pos as sp;
use std::collections::BTreeSet;

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
            return self.match_comment();
        } else if to_scan.starts_with("//") {
            return self.match_comment();
        } else {
            return self.match_token();
        }
    }

    fn peek_next(&self) -> Option<char> {
        if self.sequence.len() <= self.pos.offset {
            return None;
        }
        self.sequence[self.pos.offset..].chars().next()
    }

    fn to_scan(&self) -> &str {
        &self.sequence[self.pos.offset..]
    }

    fn forward_pos(&self, offset: usize) -> Result<sp::Pos, usize> {
        let mut chars = self.to_scan().chars();
        let mut res = self.pos.clone();
        for i in 0..offset {
            match chars.nth(i) {
                None => return Err(i),
                Some(c) => {
                    if c == '\n' {
                        res.row += 1;
                        res.col = 0;
                    } else {
                        res.col += 1;
                    }
                    res.offset += 1;
                }
            }
        }
        Ok(res)
    }

    fn advance(&mut self, offset: usize) -> Result<(), usize> {
        self.forward_pos(offset).and_then(|p| {
            self.pos = p;
            Ok(())
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
        if self.sequence.len() <= self.pos.offset {
            return None;
        }
        let mut res: Option<TokenItem> = None;
        let mut offset = 0;
        for (pattern, tok) in &self.token_map {
            if self.to_scan().starts_with(pattern) {
                offset = pattern.chars().count();
                let end = self.forward_pos(offset);
                assert!(end.is_ok());
                res = Some(Ok((self.pos, tok.clone(), end.unwrap())));
            }
        }
        assert!(self.advance(offset).is_ok());
        res
    }

    fn match_id(&mut self) -> Option<TokenItem> {
        None
    }
    fn match_char(&mut self) -> Option<TokenItem> {
        None
    }
    fn match_string(&mut self) -> Option<TokenItem> {
        None
    }
    fn match_comment(&mut self) -> Option<TokenItem> {
        None
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, sp::Pos, Error>;

    /*
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.chars.next() {
                Some((i, '')) => _,
                Some((i, ' ')) => return Some(Ok((i, Tok::Space, i + 1))),
                Some((i, '\t')) => return Some(Ok((i, Tok::Tab, i + 1))),
                Some((i, '\n')) => return Some(Ok((i, Tok::Linefeed, i + 1))),
                None => return None, // End of file
                _ => continue,       // Comment; skip this character
            }
        }
    }
    */

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
