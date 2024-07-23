pub mod lexer;
pub mod state;
pub mod util;

use crate::ast::*;
use crate::err_span;
use crate::error::Error;
use crate::parser::grammar::ProgramParser;
use crate::parser::lexer::Lexer;
use crate::parser::state::ParserState;
use crate::source_pos::SrcSpan;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar, "/parser/grammar.rs");

pub fn parse(content: &String) -> Result<Program, Error> {
    use lalrpop_util::ParseError;
    let state = ParserState::new();
    let ast = ProgramParser::new().parse(&state, Lexer::new(content));
    ast.map_err(|e| match &e {
        ParseError::InvalidToken { location } => {
            err_span!(Some(SrcSpan::new(*location, *location)), "{}", e)
        }
        ParseError::UnrecognizedEof { location, expected:_ } => {
            err_span!(Some(SrcSpan::new(*location, *location)), "{}", e)
        }
        ParseError::UnrecognizedToken {
            token: (l, _, r),
            expected: _,
        } => {
            err_span!(Some(SrcSpan::new(*l, *r)), "{}", e)
        }
        ParseError::ExtraToken { token: (l, _, r) } => {
            err_span!(Some(SrcSpan::new(*l, *r)), "{}", e)
        }
        ParseError::User { error } => error.clone(),
    })
}
