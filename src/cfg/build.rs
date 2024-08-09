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

    fn new_version(&mut self, var: &ir::Var, scope: ast::Scope) {
        let version = self.count.entry(var.clone()).or_insert(0);
        *version += 1;
        let scope_map = self
            .latest_version_in_scope
            .entry(var.clone())
            .or_insert(HashMap::new());
        scope_map.insert(scope, *version);
    }

    fn latest_version_in(&mut self, var: &ir::Var, scope: ast::Scope) -> Option<usize> {
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
    pub var_versions: RefCell<VarVersionCache<'s>>,
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

        program
    }

    fn update_cfg(&self, cfg: &mut CFG<ir::Label, ir::BasicBlock, Edge>) {
        self.add_bb_args(cfg);
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

    fn add_version_to_vars(&self, cfg: &CFG<ir::Label, ir::BasicBlock, Edge>) {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::from([&cfg.entry]);
        while let Some(l) = queue.pop_front() {
            let bb = cfg.get_node(l).unwrap();
            for i in [0..bb.borrow().args.len()] {
            }
        }
    }
}
