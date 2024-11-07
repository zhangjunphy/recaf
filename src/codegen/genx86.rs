use super::mem;
use super::x86;
use crate::ast;
use crate::ir;
use std::collections::HashMap;

pub struct CodeGenX86 {
    str_lit_blocks: HashMap<ir::VVar, x86::Label>,
    int_lit_blocks: HashMap<ir::VVar, x86::Label>,
    module: ir::Module,
}

pub enum LabelKind {
    Str,
    Bb,
}

impl CodeGenX86 {
    pub fn new(module: ir::Module) -> Self {
        CodeGenX86 {
            str_lit_blocks: HashMap::new(),
            int_lit_blocks: HashMap::new(),
            module,
        }
    }

    pub fn run(&mut self) -> x86::Assembly {
        let mut res = Vec::new();
        res.push(self.gen_global());
        x86::Assembly { sections: res }
    }

    fn gen_global(&mut self) -> x86::Section {
        let globals = &self.module.globals;
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
        let mut frame = mem::StackFrame::new(func);
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
            for stmt in &bb.statements {}
        }

        section
    }

    pub fn gen_statement(
        &self,
        stmt: &ir::Statement,
        frame: &mut mem::StackFrame,
    ) -> Vec<x86::AsmX86> {
        let mut asms = Vec::new();
        match stmt {
            ir::Statement::Assign { dst, src } => {
                let dst_mem = self.var_mem(dst, frame).unwrap();
                let mov = x86::AsmX86::MovQ {
                    src: self.get_val_src(src, frame),
                    dest: x86::Dest::Mem(dst_mem),
                };
                asms.push(mov);
            }
            ir::Statement::Call {
                dst,
                method,
                arguments,
            } => {
                asms.append(&mut self.setup_call_site(arguments, frame));
                asms.push(x86::AsmX86::Call(x86::Label {
                    str: method.clone(),
                }));
                if let Some(dst) = dst {
                    let slot = frame.push_var(dst);
                    let mem = x86::Mem::RegOffset(x86::Reg::RSP, slot.start as i64);
                    asms.push(x86::AsmX86::MovQ {
                        src: x86::Src::Reg(x86::Reg::RAX),
                        dest: x86::Dest::Mem(mem),
                    });
                }
            }
            ir::Statement::Alloca { dst, ty, size } => frame.push_var(dst),
        }
        asms
    }

    fn setup_call_site(
        &self,
        args: &Vec<ir::Val>,
        frame: &mut mem::StackFrame,
    ) -> Vec<x86::AsmX86> {
        let mut asms = Vec::new();
        let arg_regs = vec![
            x86::Reg::RDI,
            x86::Reg::RSI,
            x86::Reg::RDX,
            x86::Reg::RCX,
            x86::Reg::R(8),
            x86::Reg::R(9),
        ];
        // Setup first 6 args
        for (reg, arg) in std::iter::zip(arg_regs, args) {
            asms.push(x86::AsmX86::MovQ {
                src: self.get_val_src(arg, frame),
                dest: x86::Dest::Reg(reg),
            })
        }
        // Push remaining args into stack
        for arg in args[6..].iter().rev() {
            let (_, mut stack_asms) = self.push_stack(arg, frame);
            asms.append(&mut stack_asms);
        }

        asms
    }

    fn push_stack(
        &self,
        val: &ir::Val,
        frame: &mut mem::StackFrame,
    ) -> (x86::Mem, Vec<x86::AsmX86>) {
        let mut asms = Vec::new();
        let mem = match val {
            ir::Val::Var(var) => {
                let slot = frame.push_var(var);
                let mem = x86::Mem::RegOffset(x86::Reg::RSP, slot.start as i64);
                asms.push(x86::AsmX86::MovQ {
                    src: x86::Src::Mem(self.var_mem(var, frame).unwrap()),
                    dest: x86::Dest::Mem(mem.clone()),
                });
                mem
            }
            ir::Val::Imm(lit) => {
                let slot = frame.push_lit(lit);
                x86::Mem::RegOffset(x86::Reg::RSP, slot.start as i64)
            }
        };
        (mem, asms)
    }

    fn get_val_src(&self, val: &ir::Val, frame: &mem::StackFrame) -> x86::Src {
        match val {
            ir::Val::Var(var) => x86::Src::Mem(self.var_mem(var, frame).unwrap()),
            ir::Val::Imm(lit) => x86::Src::Imm(lit.as_i64().unwrap()),
        }
    }

    fn var_mem(&self, var: &ir::VVar, frame: &mem::StackFrame) -> Option<x86::Mem> {
        if let Some(slot) = frame.get_var(var) {
            return Some(x86::Mem::RegOffset(x86::Reg::RSP, slot.start as i64));
        }
        if let Some(label) = self.int_lit_blocks.get(var) {
            return Some(x86::Mem::Label(label.clone()));
        }
        None
    }
}
