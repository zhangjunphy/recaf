use crate::cfg::def::CFG;
use crate::ir;
use crate::ast;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Program {
    pub imports: Vec<String>,
    pub globals: Vec<Rc<ast::FieldDecl>>,
    pub cfgs: HashMap<String, CFG<ir::BasicBlock>>,
}
