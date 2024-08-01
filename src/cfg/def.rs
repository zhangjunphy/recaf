use std::collections::HashMap;
use std::rc::Rc;
use crate::ir;

pub struct CFG<Tn, Te> {
    pub name: String,
    pub nodes: HashMap<ir::Label, Rc<Tn>>,
    pub edges: HashMap<ir::Label, Vec<(ir::Label, Te)>>,
    pub redges: HashMap<ir::Label, Vec<ir::Label>>,
}

pub struct Program {
    pub imports: Vec<String>,
    pub globals: Vec<Rc<ir::Var>>,
    pub cfgs: HashMap<String, CFG<ir::BasicBlock>>,
}
