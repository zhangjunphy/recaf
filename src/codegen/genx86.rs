use crate::ir;
use super::x86;

pub struct CodeGenX86 {
}

impl CodeGenX86 {
    pub fn run(&self, module: ir::Module) -> Vec<x86::Block> {
        Vec::new()
    }
}
