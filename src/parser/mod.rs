pub mod lexer;
pub mod util;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar, "/parser/grammar.rs");

use crate::ast::*;
use crate::source_pos::SrcSpan;
use std::cell::RefCell;

pub struct ParserState {
    block_count: RefCell<usize>,
}

impl ParserState {
    pub fn new() -> Self {
        ParserState {
            block_count: RefCell::new(crate::consts::ROOT_BLOCK_ID + 1),
        }
    }

    pub fn block(&self, fields: Vec<FieldDecl>, stmts: Vec<Statement>, span: SrcSpan) -> Block {
        let block_id = *self.block_count.borrow();
        *self.block_count.borrow_mut() += 1;
        Block {
            id: block_id,
            fields,
            statements: stmts,
            span: Some(span),
        }
    }
}
