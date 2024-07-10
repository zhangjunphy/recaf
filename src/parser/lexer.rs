use std::str::CharIndices;
use recaf::error::Error;

pub enum Tok {
    Space,
    Tab,
    Linefeed,
}

pub struct Lexer<'input> {
    chars: CharIndices<'input>,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer { chars: input.char_indices() }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, usize, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.chars.next() {
                Some((i, ' ')) => return Some(Ok((i, Tok::Space, i+1))),
                Some((i, '\t')) => return Some(Ok((i, Tok::Tab, i+1))),
                Some((i, '\n')) => return Some(Ok((i, Tok::Linefeed, i+1))),

                None => return None, // End of file
                _ => continue, // Comment; skip this character
            }
        }
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
