use super::x86;
use crate::ast;
use crate::ir;

pub struct CodeGenX86 {}

pub enum LabelKind {
    Str,
    Bb
}

impl CodeGenX86 {
    pub fn run(&self, module: ir::Module) -> Vec<x86::Block> {
        let mut res = Vec::new();
        let mut globals = self.gen_global(&module.globals);
        res.append(&mut globals);
        res
    }

    pub fn gen_global(&self, globals: &Vec<(ir::VVar, Option<ast::Literal>)>) -> Vec<x86::Block> {
        let blocks = Vec::new();
        let count = 0;
        for (var, init) in globals {
            let mut block = x86::Block {
                label: x86::Label {
                    str: format!("str.{}", count),
                },
                asms: Vec::new(),
            };
            block.asms.push();
        }
        blocks
    }
}
