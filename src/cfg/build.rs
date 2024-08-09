//! Build the actual cfg from the partial cfg.
//! 1. Re-index variables into SSA
//! 2. Insert missing basic block parameters

use super::def::{Edge, CFG};
use super::partial;
use crate::ast;
use crate::dominator;
use crate::ir;
use crate::semantic;
use std::collections::HashMap;

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
}

impl<'s> CFGBuild<'s> {
    pub fn new(symbols: &'s semantic::ProgramSymbols) -> Self {
        CFGBuild { symbols }
    }

    pub fn build(&mut self, p: &ast::Program) {
        let mut partial_build = partial::CFGPartialBuild::new(self.symbols);
        let program = partial_build.build(p);
    }

    fn build_cfg(&mut self, cfg: &CFG<ir::Label, ir::BasicBlock, Edge>) {
        let df = dominator::DominanceFrontier::new(cfg);
    }
}
