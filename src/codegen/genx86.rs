use super::mem;
use super::x86;
use crate::ast;
use crate::ir;
use std::collections::HashMap;

pub struct CodeGenX86 {
    str_lit_blocks: HashMap<ir::VVar, x86::Label>,
    int_lit_blocks: HashMap<ir::VVar, x86::Label>,
}

pub enum LabelKind {
    Str,
    Bb,
}

impl CodeGenX86 {
    pub fn new() -> Self {
        CodeGenX86 {
            str_lit_blocks: HashMap::new(),
            int_lit_blocks: HashMap::new(),
        }
    }
    pub fn run(&mut self, module: ir::Module) -> x86::Assembly {
        let mut res = Vec::new();
        res.push(self.gen_global(&module.globals));
        x86::Assembly { sections: res }
    }

    pub fn gen_global(&mut self, globals: &Vec<(ir::VVar, Option<ast::Literal>)>) -> x86::Section {
        let mut section = x86::Section {
            kind: x86::SectionKind::Data,
            blocks: Vec::new(),
        };
        for (var, init) in globals {
            match &var.var.ty {
                ast::Type::Int | ast::Type::Char | ast::Type::Bool => {
                    let label = x86::Label::new(format!("intlit.{}", var.var.id).as_str());
                    self.int_lit_blocks.insert(var.clone(), label.clone());
                    let mut block = x86::Block {
                        label: label.clone(),
                        asms: Vec::new(),
                    };
                    block.asms.push(x86::AsmX86::Zero(var.var.ty.size()));
                    section.blocks.push(block);
                }
                ast::Type::Ptr(_) => {
                    panic!("Global variable of pointer type is not supported yet.")
                }
                ast::Type::Void => {
                    panic!("Global variable of void type.")
                }
                ast::Type::Array(inner, _) => match **inner {
                    ast::Type::Char => {
                        let label = x86::Label::new(
                            format!("strlit.{}", self.str_lit_blocks.len()).as_str(),
                        );
                        self.str_lit_blocks.insert(var.clone(), label.clone());
                        let mut block = x86::Block {
                            label: label.clone(),
                            asms: Vec::new(),
                        };
                        block.asms.push(x86::AsmX86::String(
                            init.as_ref().unwrap().str_lit().unwrap(),
                        ));
                        section.blocks.push(block);
                    }
                    _ => {
                        panic!("Non-string array in global scope.")
                    }
                },
            }
        }
        section
    }

    pub fn gen_function(&mut self, func: &ir::Function) -> x86::Section {
        let mut section = x86::Section {
            kind: x86::SectionKind::Text,
            blocks: Vec::new(),
        };

        // Add an entry point into this function.
        // Allocate stack frame for local variables.
        let frame = mem::StackFrame::new(func);
        let mut entry = x86::Block {
            label: x86::Label::new(func.name.as_str()),
            asms: Vec::new(),
        };
        entry.asms.push(x86::AsmX86::Enter(frame.size));
        section.blocks.push(entry);

        for bb in &func.body {
            let label = x86::Label::new(format!("{}", bb.label).as_str());
            let mut block = x86::Block {
                label,
                asms: Vec::new(),
            };
            for stmt in &bb.statements {
            }
        }

        section
    }

    pub fn gen_statement(&self, stmt: &ir::Statement, frame: &mem::StackFrame) -> Vec<x86::AsmX86> {
        let res = Vec::new();
        match stmt {
        }
        res
    }
}
