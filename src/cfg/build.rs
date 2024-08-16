//! Build the actual cfg from the partial cfg.
//! 1. Re-index variables into SSA
//! 2. Insert missing basic block parameters

use super::def;
use super::def::{Edge, CFG};
use super::partial;
use crate::ast;
use crate::consts;
use crate::graph::dominator;
use crate::graph::Graph;
use crate::ir;
use crate::semantic;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

struct VarVersionCache<'s> {
    pub count: HashMap<ir::Var, u64>,
    pub var_version_in_scope: HashMap<ir::Var, HashMap<ast::Scope, u64>>,
    pub var_version_in_bb: HashMap<ir::Var, HashMap<ir::Label, u64>>,
    pub symbols: &'s semantic::ProgramSymbols,
}

impl<'s> VarVersionCache<'s> {
    fn new(symbols: &'s semantic::ProgramSymbols) -> Self {
        VarVersionCache {
            count: HashMap::new(),
            var_version_in_scope: HashMap::new(),
            var_version_in_bb: HashMap::new(),
            symbols,
        }
    }

    fn new_version(&mut self, var: &ir::Var, scope: ast::Scope, bb: ir::Label) -> u64 {
        let count = self.count.entry(var.clone()).or_insert(0);
        let version = *count;
        *count += 1;
        let scope_map = self
            .var_version_in_scope
            .entry(var.clone())
            .or_insert(HashMap::new());
        scope_map.insert(scope, version);
        let bb_map = self
            .var_version_in_bb
            .entry(var.clone())
            .or_insert(HashMap::new());
        bb_map.insert(bb, version);
        version
    }

    fn latest_version_in_bb(
        &self,
        var: &ir::Var,
        bb: ir::Label,
        cfg: &CFG<ir::Label, ir::BasicBlock, Edge>,
    ) -> Option<u64> {
        let mut bb_opt = Some(bb);
        // NOTE: We can do a depth-first seach as divergent control flows has been covered by
        // bb arguments already.
        while let Some(bb) = bb_opt {
            let var_version = self
                .var_version_in_bb
                .get(&var)
                .and_then(|m| m.get(&bb))
                .map(|v| *v);
            if var_version.is_some() {
                return var_version;
            } else {
                bb_opt = cfg.in_neighbors(&bb).first().map(|l| **l);
            }
        }
        None
    }

    fn latest_version_in_scope(&self, var: &ir::Var, scope: ast::Scope) -> Option<u64> {
        let var_version = self.var_version_in_scope.get(&var)?;
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
        self.init_global_version(&program);
        for (_, cfg) in &mut program.cfgs {
            self.update_cfg(cfg);
        }
        program
    }

    fn update_cfg(&self, cfg: &mut CFG<ir::Label, ir::BasicBlock, Edge>) {
        self.add_bb_args(cfg);
        self.add_version_to_vars(cfg);
        self.add_bb_call_args(cfg);
    }

    fn vars_in_bb(l: &ir::Label, cfg: &CFG<ir::Label, ir::BasicBlock, Edge>) -> HashSet<ir::VVar> {
        let mut queue = VecDeque::from([l]);
        let mut enqueued = HashSet::from([l]);

        let mut res = HashSet::new();
        while let Some(n) = queue.pop_front() {
            for s in &cfg.get_node(&n).unwrap().borrow().statements {
                res.extend(s.write_to_var().into_iter().map(|v| v.clone()));
            }

            for i in cfg.in_neighbors(&n) {
                if !enqueued.contains(i) {
                    queue.push_back(i);
                    enqueued.insert(i);
                }
            }
        }
        res
    }

    /// HACK: Hack around basic block argument insertion for now.
    fn vars_in_bb_predecessors(
        l: &ir::Label,
        cfg: &CFG<ir::Label, ir::BasicBlock, Edge>,
    ) -> HashSet<ir::VVar> {
        let preds = cfg.in_neighbors(l);
        if preds.is_empty() {
            return HashSet::new();
        }
        let mut res = Self::vars_in_bb(preds[0], cfg);
        for o in &preds[1..] {
            res = res
                .intersection(&Self::vars_in_bb(o, cfg))
                .map(|l| l.clone())
                .collect();
        }
        res
    }

    fn add_bb_args(&self, cfg: &CFG<ir::Label, ir::BasicBlock, Edge>) {
        let df = dominator::DominanceFrontier::new(&cfg.entry, cfg);
        for (label, bb) in cfg.nodes() {
            let bb_writes: HashSet<_> = bb
                .borrow()
                .write_vars()
                .into_iter()
                .map(|v| v.clone())
                .collect();
            for f in df.get_frontier(label) {
                if f == label {
                    continue;
                }
                let frontier_bb = cfg.get_node(f).unwrap();
                let vars = Self::vars_in_bb_predecessors(f, cfg);
                bb_writes.intersection(&vars).for_each(|v| {
                    if !frontier_bb.borrow().args.contains(v) {
                        frontier_bb.borrow_mut().args.push((*v).clone());
                    }
                })
            }
        }
    }

    fn init_global_version(&self, p: &def::Program) {
        for g in &p.globals {
            let version = self.var_versions.borrow_mut().new_version(
                &g.var,
                ast::Scope::new(consts::ROOT_SCOPE_ID),
                ir::Label::new(0),
            );
            g.version.set(version);
        }
    }

    fn add_version_to_vars(&self, cfg: &mut CFG<ir::Label, ir::BasicBlock, Edge>) {
        let mut enqueued = HashSet::from([cfg.entry]);
        let mut queue = VecDeque::from([cfg.entry]);
        let mut edge_updates: Vec<(ir::Label, ir::Label, def::Edge)> = Vec::new();
        while let Some(l) = queue.pop_front() {
            let bb = cfg.get_node(&l).unwrap();
            for i in 0..bb.borrow().args.len() {
                let var = &bb.borrow().args[i].var;
                let version =
                    self.var_versions
                        .borrow_mut()
                        .new_version(var, bb.borrow().ast_scope, l);
                bb.borrow().args[i].version.set(version);
            }

            for i in 0..bb.borrow().statements.len() {
                for read in &mut bb.borrow().statements[i].read_vars() {
                    let version = self
                        .var_versions
                        .borrow()
                        .latest_version_in_scope(&read.var, bb.borrow().ast_scope)
                        .unwrap();
                    read.version.set(version);
                }
                for write in &mut bb.borrow().statements[i].write_to_var() {
                    let version = self.var_versions.borrow_mut().new_version(
                        &write.var,
                        bb.borrow().ast_scope,
                        l,
                    );
                    write.version.set(version);
                }
            }

            for o in cfg.out_neighbors(&l) {
                if !enqueued.contains(o) {
                    queue.push_back(*o);
                    enqueued.insert(*o);
                }
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
                                    .latest_version_in_scope(&v.var, bb.borrow().ast_scope)
                                    .unwrap();
                                let res = v.clone();
                                res.version.set(version);
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
                                    .latest_version_in_scope(&v.var, bb.borrow().ast_scope)
                                    .unwrap();
                                let res = v.clone();
                                res.version.set(version);
                                ir::Val::Var(res)
                            }
                        };
                        edge_updates.push((l, *o, def::Edge::JumpFalse(versioned_val.clone())));
                    }
                };
            }
        }

        edge_updates.into_iter().for_each(|(s, d, e)| {
            cfg.insert_edge(s, d, Rc::new(RefCell::new(e)));
        })
    }

    fn add_bb_call_args(&self, cfg: &mut CFG<ir::Label, ir::BasicBlock, Edge>) {
        for (l, bb) in cfg.nodes() {
            if bb.borrow().statements.is_empty() {
                continue;
            }
            let s = (*bb.borrow().statements.last().unwrap()).clone();
            let mut bb_mut = bb.borrow_mut();

            let update_arg = |label, bb_call: &mut ir::CallBB| {
                for arg in &cfg.get_node(&label).unwrap().borrow().args {
                    let version = self
                        .var_versions
                        .borrow()
                        .latest_version_in_bb(&arg.var, *l, cfg)
                        .unwrap();

                    bb_call.args.push(ir::VVar::new(arg.var.clone(), version));
                }
            };
            match s {
                ir::Statement::Br(ir::Branch::UnCon {
                    bb: ir::CallBB { label, args: _ },
                }) => match bb_mut.statements.last_mut().unwrap() {
                    ir::Statement::Br(ir::Branch::UnCon { bb }) => update_arg(label, bb),
                    _ => (),
                },
                ir::Statement::Br(ir::Branch::Con {
                    pred: _,
                    bb_true:
                        ir::CallBB {
                            label: l_true,
                            args: _,
                        },
                    bb_false:
                        ir::CallBB {
                            label: l_false,
                            args: _,
                        },
                }) => match bb_mut.statements.last_mut().unwrap() {
                    ir::Statement::Br(ir::Branch::Con {
                        pred: _,
                        bb_true,
                        bb_false,
                    }) => {
                        update_arg(l_true, bb_true);
                        update_arg(l_false, bb_false);
                    }
                    _ => (),
                },
                s => panic!("Terminal statement expected, but found: {s}"),
            }
        }
    }
}
