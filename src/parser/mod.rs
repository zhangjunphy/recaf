pub mod lexer;
pub mod util;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar, "/parser/grammar.rs");
