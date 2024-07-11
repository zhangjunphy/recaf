use crate::error::Error;
use crate::source_pos as sp;

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
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
type TokenItem = Spanned<Tok, sp::Pos, Error>;

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        // We have to be careful to put longer patterns to the front so they get matched
        // before some of its subsequence.
        // There might be some better design but this should work for the moment.
        let token_map = vec![
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
        ];

        Lexer {
            sequence: input,
            pos: sp::Pos::new(0, 0, 0),
            token_map,
        }
    }

    fn scan(&mut self) -> Option<TokenItem> {
        let to_scan = &self.sequence[self.pos.offset..];
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

    fn match_token(&mut self) -> Option<TokenItem> {
        if self.sequence.len() <= self.pos.offset {
            return None;
        }
        for (pattern, tok) in &self.token_map {
            if self.sequence[self.pos.offset..].starts_with(pattern) {
                return Some(Ok((self.pos, tok.clone(), self.pos)));
            }
        }
        return None;
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
