//! Build the actual cfg from the partial cfg.
//! 1. Re-index variables into SSA
//! 2. Insert missing basic block parameters

use super::def;
use super::def::{Edge, CFG};
use super::partial;
use crate::ast;
use crate::dominator;
use crate::ir;
use crate::semantic;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

struct VarVersionCache<'s> {
    pub count: HashMap<ir::Var, usize>,
    pub latest_version_in_scope: HashMap<ir::Var, HashMap<ast::Scope, usize>>,
    pub symbols: &'s semantic::ProgramSymbols,
}

impl<'s> VarVersionCache<'s> {
    fn new(symbols: &'s semantic::ProgramSymbols) -> Self {
        VarVersionCache {
            count: HashMap::new(),
            latest_version_in_scope: HashMap::new(),
            symbols,
        }
    }

    fn new_version(&mut self, var: &ir::Var, scope: ast::Scope) -> usize {
        let version = self.count.entry(var.clone()).or_insert(0);
        *version += 1;
        let scope_map = self
            .latest_version_in_scope
            .entry(var.clone())
            .or_insert(HashMap::new());
        scope_map.insert(scope, *version);
        *version
    }

    fn latest_version_in(&self, var: &ir::Var, scope: ast::Scope) -> Option<usize> {
        let var_version = self.latest_version_in_scope.get(&var)?;
        let mut scope = Some(scope);
        while let Some(s) = scope {
            if let Some(v) = var_version.get(&s) {
                return Some(*v);
            }
            scope = self.symbols.find_parent_scope(&s).map(|s| *s);
        }
        None
    }
}

pub struct CFGBuild<'s> {
    pub symbols: &'s semantic::ProgramSymbols,
    var_versions: RefCell<VarVersionCache<'s>>,
}

impl<'s> CFGBuild<'s> {
    pub fn new(symbols: &'s semantic::ProgramSymbols) -> Self {
        CFGBuild {
            symbols,
            var_versions: RefCell::new(VarVersionCache::new(symbols)),
        }
    }

    pub fn build(&self, p: &ast::Program) -> def::Program {
        let mut partial_build = partial::CFGPartialBuild::new(self.symbols);
        let mut program = partial_build.build(p);
        for (_, cfg) in &mut program.cfgs {
            self.update_cfg(cfg);
        }
        program
    }

    fn update_cfg(&self, cfg: &mut CFG<ir::Label, ir::BasicBlock, Edge>) {
        self.add_bb_args(cfg);
        self.add_version_to_vars(cfg);
    }

    fn add_bb_args(&self, cfg: &CFG<ir::Label, ir::BasicBlock, Edge>) {
        let df = dominator::DominanceFrontier::new(cfg);
        for (label, bb) in cfg.nodes() {
            let bb_writes = bb.borrow().write_vars();
            for f in df.get_frontier(label) {
                let frontier_bb = cfg.get_node(f).unwrap();
                let frontier_reads = frontier_bb.borrow().read_vars();
                frontier_reads.intersection(&bb_writes).for_each(|v| {
                    if !frontier_bb.borrow().args.contains(v) {
                        frontier_bb.borrow_mut().args.push(v.clone());
                    }
                })
            }
        }
    }

    fn add_version_to_vars(&self, cfg: &mut CFG<ir::Label, ir::BasicBlock, Edge>) {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::from([cfg.entry]);
        let mut edge_updates: Vec<(ir::Label, ir::Label, def::Edge)> = Vec::new();
        while let Some(l) = queue.pop_front() {
            visited.insert(l);
            let bb = cfg.get_node(&l).unwrap();
            for i in 0..bb.borrow().args.len() {
                let var = &bb.borrow().args[i].var;
                let version = self
                    .var_versions
                    .borrow_mut()
                    .new_version(var, bb.borrow().ast_scope);
                *bb.borrow().args[i].version.borrow_mut() = version;
            }

            for i in 0..bb.borrow().statements.len() {
                for read in &mut bb.borrow().statements[i].read_vars() {
                    let version = self
                        .var_versions
                        .borrow()
                        .latest_version_in(&read.var, bb.borrow().ast_scope)
                        .unwrap();
                    *read.version.borrow_mut() = version;
                }
                for write in &mut bb.borrow().statements[i].write_to_var() {
                    let version = self
                        .var_versions
                        .borrow_mut()
                        .new_version(&write.var, bb.borrow().ast_scope);
                    *write.version.borrow_mut() = version;
                }
            }

            for o in cfg.out_nodes(&l) {
                let edge = cfg.get_edge(&l, o).unwrap().borrow();
                match &*edge {
                    def::Edge::Continue => (),
                    def::Edge::JumpTrue(val) => {
                        let versioned_val = match val {
                            i @ ir::Val::Imm(_) => i.clone(),
                            ir::Val::Var(v) => {
                                let version = self
                                    .var_versions
                                    .borrow()
                                    .latest_version_in(&v.var, bb.borrow().ast_scope)
                                    .unwrap();
                                let res = v.clone();
                                *res.version.borrow_mut() = version;
                                ir::Val::Var(res)
                            }
                        };
                        edge_updates.push((l, *o, def::Edge::JumpTrue(versioned_val.clone())));
                    }
                    def::Edge::JumpFalse(val) => {
                        let versioned_val = match val {
                            i @ ir::Val::Imm(_) => i.clone(),
                            ir::Val::Var(v) => {
                                let version = self
                                    .var_versions
                                    .borrow()
                                    .latest_version_in(&v.var, bb.borrow().ast_scope)
                                    .unwrap();
                                let res = v.clone();
                                *res.version.borrow_mut() = version;
                                ir::Val::Var(res)
                            }
                        };
                        edge_updates.push((l, *o, def::Edge::JumpFalse(versioned_val.clone())));
                    }
                };
            }
        }

        edge_updates.into_iter().for_each(|(s, d, e)|{
            cfg.insert_edge(s, d, Rc::new(RefCell::new(e)));
        })
    }
}
