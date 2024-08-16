use crate::ir;
use std::collections::{BTreeSet, HashMap};

#[derive(PartialEq, Eq)]
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

pub struct StackFrame<'a> {
    vars: HashMap<ir::VVar, &'a StackSlot>,
    slots: BTreeSet<StackSlot>,
    size: u64,
}

impl<'a> StackFrame<'a> {
    pub fn new() -> Self {
        StackFrame {
            vars: HashMap::new(),
            slots: BTreeSet::new(),
            size: 0,
        }
    }

    pub fn push_var(&'a mut self, var: &ir::VVar) -> &'a StackSlot {
        let var_size = var.var.ty.size();
        self.slots.insert(StackSlot {
            start: self.size,
            size: var_size,
        });
        self.size += var_size;
        let slot = self.slots.last().unwrap();
        self.vars.insert(var.clone(), slot);
        slot
    }

    pub fn get_var(&'a self, var: &ir::VVar) -> Option<&'a StackSlot> {
        self.vars.get(var).map(|s| *s)
    }
}
