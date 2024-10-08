use crate::ast::*;
use crate::source_pos::SrcSpan;
use std::cell::RefCell;

pub struct ParserState {
    block_count: RefCell<u64>,
}

impl ParserState {
    pub fn new() -> Self {
        ParserState {
            block_count: RefCell::new(crate::consts::ROOT_SCOPE_ID + 1),
        }
    }

    pub fn block(&self, fields: Vec<FieldDecl>, stmts: Vec<Statement>, span: SrcSpan) -> Block {
        let block_id = *self.block_count.borrow();
        *self.block_count.borrow_mut() += 1;
        Block {
            scope: Scope { id: block_id },
            fields,
            statements: stmts,
            span: Some(span),
        }
    }
}
