use crate::ir;
use std::collections::{BTreeSet, HashMap};
use std::rc::Rc;

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct StackSlot {
    pub start: u64,
    pub size: u64,
}

impl PartialOrd for StackSlot {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.start.partial_cmp(&other.start)
    }
}

impl Ord for StackSlot {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start.cmp(&other.start)
    }
}

pub struct StackFrame {
    vars: HashMap<ir::VVar, Rc<StackSlot>>,
    slots: BTreeSet<Rc<StackSlot>>,
    pub size: u64,
}

impl StackFrame {
    pub fn new(func: &ir::Function) -> Self {
        let mut res = StackFrame {
            vars: HashMap::new(),
            slots: BTreeSet::new(),
            size: 0,
        };
        // Handle arguments of func
        for arg in &func.args {
            res.push_var(arg);
        }
        // Aggregate local variables in func
        for block in &func.body {
            for arg in &block.args {
                res.push_var(arg);
            }
            for stmt in &block.statements {
                if let Some(var) = stmt.write_to_var() {
                    res.push_var(var);
                }
            }
        }
        res
    }

    pub fn push_var(&mut self, var: &ir::VVar) -> &Rc<StackSlot> {
        let var_size = var.var.ty.size();
        self.slots.insert(Rc::new(StackSlot {
            start: self.size,
            size: var_size,
        }));
        self.size += var_size;
        let slot = self.slots.last().unwrap();
        self.vars.insert(var.clone(), slot.clone());
        slot
    }

    pub fn get_var(&self, var: &ir::VVar) -> Option<&Rc<StackSlot>> {
        self.vars.get(var)
    }
}
