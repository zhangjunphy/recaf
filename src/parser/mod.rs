pub mod ast;
pub mod lexer;
pub mod parser;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar, "/parser/grammar.rs");
