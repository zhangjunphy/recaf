use crate::ir;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::hash::Hash;

pub struct CFG<Ti, Tn, Te>
where
    Ti: Hash,
{
    pub name: String,
    pub entry: Ti,
    pub exit: Ti,
    pub nodes: HashMap<Ti, Rc<RefCell<Tn>>>,
    pub edges: HashMap<Ti, Vec<(Ti, Te)>>,
    pub redges: HashMap<Ti, Vec<Ti>>,
}

pub struct Program {
    pub imports: Vec<String>,
    pub globals: Vec<Rc<ir::Var>>,
    pub cfgs: HashMap<String, CFG<ir::Label, ir::BasicBlock, ir::Branch>>,
}
