//! Build the actual cfg from the partial cfg.
//! 1. Re-index variables into SSA
//! 2. Insert missing basic block parameters

use super::def::{Edge, CFG};
use super::partial;
use crate::ast;
use crate::dominator;
use crate::ir;
use crate::semantic;

/// Uniquify vars by versioning.
pub struct VarVersion {
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
