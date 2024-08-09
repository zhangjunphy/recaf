//! Process AST into a cfg with three-address code.
//! It is not a SSA yet. And basicblock parameters introduced by control flows are left empty.

use crate::ast;
use crate::cfg::def;
use crate::cfg::def::{Edge, CFG};
use crate::consts;
use crate::ir;
use crate::semantic;
use crate::source_pos::SrcSpan;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Label {
    Just(ir::Label),
    LoopPred,
    LoopExit,
    MethodEntry,
    MethodExit,
}

impl Label {
    pub fn get_label(&self) -> ir::Label {
        match &self {
            Label::Just(l) => *l,
            _ => panic!(),
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Label::Just(l) => write!(f, "{}", l),
            Label::LoopPred => f.write_str("loop_pred"),
            Label::LoopExit => f.write_str("loop_exit"),
            Label::MethodEntry => f.write_str("method_entry"),
            Label::MethodExit => f.write_str("method_exit"),
        }
    }
}

type PartialCFG = CFG<Label, ir::BasicBlock, Edge>;

pub struct Program {
    pub imports: Vec<String>,
    pub globals: Vec<Rc<ir::VVar>>,
    pub cfgs: HashMap<String, PartialCFG>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            imports: Vec::new(),
            globals: Vec::new(),
            cfgs: HashMap::new(),
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

pub struct VarCache {
    pub var_to_symbol: HashMap<Rc<ir::VVar>, (ast::Scope, String)>,
    pub symbol_to_var: HashMap<(ast::Scope, String), Rc<ir::VVar>>,
    pub vars: Vec<Rc<ir::VVar>>,

    pub next_var_id: usize,
}

impl VarCache {
    fn new() -> Self {
        VarCache {
            var_to_symbol: HashMap::new(),
            symbol_to_var: HashMap::new(),
            vars: Vec::new(),
            next_var_id: 0,
        }
    }

    fn lookup_var(&self, ast_scope: ast::Scope, symbol: &str) -> Option<&Rc<ir::VVar>> {
        self.symbol_to_var.get(&(ast_scope, symbol.to_string()))
    }

    fn new_var_id(&mut self) -> usize {
        let id = self.next_var_id;
        self.next_var_id += 1;
        id
    }

    fn new_local(
        &mut self,
        ast_scope: ast::Scope,
        ty: ast::Type,
        decl: Option<ast::FieldDecl>,
        span: Option<SrcSpan>,
    ) -> Rc<ir::VVar> {
        let id = self.new_var_id();
        let var = Rc::new(ir::VVar::new(
            ir::Var {
                id,
                ty,
                decl: decl.clone(),
                span,
                locality: ir::Locality::Local,
            },
            0,
        ));
        if let Some(d) = &decl {
            self.var_to_symbol
                .insert(var.clone(), (ast_scope, d.id.id.clone()));
            self.symbol_to_var
                .insert((ast_scope, d.id.id.clone()), var.clone());
        }
        self.vars.push(var.clone());
        var
    }

    fn new_global(
        &mut self,
        ty: ast::Type,
        decl: Option<ast::FieldDecl>,
        span: Option<SrcSpan>,
    ) -> Rc<ir::VVar> {
        let id = self.new_var_id();
        let var = Rc::new(ir::VVar::new(
            ir::Var {
                id,
                ty,
                decl: decl.clone(),
                span,
                locality: ir::Locality::Global,
            },
            0,
        ));
        if let Some(d) = &decl {
            self.var_to_symbol.insert(
                var.clone(),
                (ast::Scope::from(consts::ROOT_SCOPE_ID), d.id.id.clone()),
            );
            self.symbol_to_var.insert(
                (ast::Scope::from(consts::ROOT_SCOPE_ID), d.id.id.clone()),
                var.clone(),
            );
        }
        self.vars.push(var.clone());
        var
    }
}

pub struct BuildState {
    next_bb_id: usize,
    current_ast_scope: ast::Scope,
    current_cfg: RefCell<Option<PartialCFG>>,
    current_block: RefCell<Option<ir::BasicBlock>>,
    var_cache: VarCache,
}

pub struct CFGPartialBuild<'s> {
    pub symbols: &'s semantic::ProgramSymbols,
    pub state: RefCell<BuildState>,
    pub program: RefCell<Program>,
}

fn node_br(cfg: &PartialCFG, n: &Label) -> Option<ir::Branch> {
    let out_nodes = cfg.out_nodes(n);
    if out_nodes.len() == 1 {
        let bb_next = out_nodes[0].get_label();
        let edge = cfg.edge_data(n, out_nodes[0]).unwrap();
        assert!(matches!(*edge.borrow(), Edge::Continue));
        Some(ir::Branch::UnCon { label: bb_next })
    } else if out_nodes.len() == 2 {
        let mut t_label = None;
        let mut f_label = None;
        let mut t_var = None;
        let mut f_var = None;
        for o in &out_nodes {
            let edge = cfg.edge_data(n, o).unwrap();
            match &*edge.borrow() {
                Edge::JumpTrue(v) => {
                    t_label = Some(*n);
                    t_var = v.get_var();
                }
                Edge::JumpFalse(v) => {
                    f_label = Some(*n);
                    f_var = v.get_var();
                }
                _ => {
                    panic!("Invalid branching in CFG.")
                }
            }
        }
        if t_var != f_var {
            panic!("Branching in CFG has inconsistant variables.")
        }
        Some(ir::Branch::Con {
            pred: ir::Val::Var(t_var.unwrap()),
            label_true: t_label.unwrap().get_label(),
            label_false: f_label.unwrap().get_label(),
        })
    } else if out_nodes.len() > 2 {
        panic!("Basic block should have 2 out edges at most.")
    } else {
        None
    }
}

impl<'s> CFGPartialBuild<'s> {
    pub fn new(symbols: &'s semantic::ProgramSymbols) -> Self {
        CFGPartialBuild {
            symbols,
            state: RefCell::new(BuildState {
                next_bb_id: 1, // reserve block 0 for global variables
                current_ast_scope: ast::Scope::new(consts::ROOT_SCOPE_ID),
                current_cfg: RefCell::new(None),
                current_block: RefCell::new(None),
                var_cache: VarCache::new(),
            }),
            program: RefCell::new(Program::new()),
        }
    }

    pub fn build(&mut self, p: &ast::Program) -> def::Program {
        self.visit_program(p);
        let p = self.program.take();

        let mut res = def::Program {
            imports: p.imports,
            globals: p.globals,
            cfgs: HashMap::new(),
        };

        // To process p into the final cfg form, we
        // 1. Add Br statement to end of all basic blocks with outgoing edges
        // 2. Convert partial::Label to ir::Label directly as they should all have be filled.
        for (name, partial_cfg) in &p.cfgs {
            let mut full_cfg = CFG::<ir::Label, ir::BasicBlock, def::Edge>::new(
                partial_cfg.name.clone(),
                partial_cfg.entry.get_label(),
                partial_cfg.exit.get_label(),
            );

            for (label, bb) in partial_cfg.nodes() {
                let mut partial_bb = bb.take();
                if let Some(br) = node_br(partial_cfg, label) {
                    partial_bb.push_stmt(ir::Statement::Br(br));
                }
                full_cfg.insert_node(label.get_label(), Rc::new(RefCell::new(partial_bb)));
                for dst in partial_cfg.out_nodes(label) {
                    let edge = partial_cfg.edge_data(label, dst).unwrap();
                    full_cfg.insert_edge(label.get_label(), dst.get_label(), edge.clone());
                }
            }

            res.cfgs.insert(name.clone(), full_cfg);
        }

        res
    }

    fn new_block_id(&self) -> usize {
        let id = self.state.borrow().next_bb_id;
        self.state.borrow_mut().next_bb_id += 1;
        id
    }

    fn get_scope(&self) -> ast::Scope {
        self.state.borrow().current_ast_scope
    }

    fn query_cache<F, T>(&self, f: F) -> T
    where
        F: FnOnce(&VarCache) -> T,
    {
        let state = self.state.borrow();
        f(&state.var_cache)
    }

    fn mut_cache<F, T>(&self, f: F) -> T
    where
        F: FnOnce(&mut VarCache) -> T,
    {
        let mut state = self.state.borrow_mut();
        f(&mut state.var_cache)
    }

    fn mut_cfg<F>(&self, f: F)
    where
        F: FnOnce(&mut PartialCFG),
    {
        let state = self.state.borrow();
        let mut cfg = state.current_cfg.borrow_mut();
        f(cfg.as_mut().unwrap());
    }

    fn mut_block<F>(&self, f: F)
    where
        F: FnOnce(&mut ir::BasicBlock),
    {
        let state = self.state.borrow();
        let mut block = state.current_block.borrow_mut();
        f(block.as_mut().unwrap());
    }

    fn push_stmt(&self, stmt: ir::Statement) {
        self.mut_block(|b| {
            b.push_stmt(stmt);
        });
    }

    fn new_isolated_block(&self, args: Vec<Rc<ir::VVar>>) -> ir::Label {
        let label = self.start_block(args);
        self.finish_block();
        label
    }

    fn start_block(&self, args: Vec<Rc<ir::VVar>>) -> ir::Label {
        let id = self.new_block_id();
        assert!(self.state.borrow().current_block.borrow().is_none());
        let ast_scope = self.state.borrow().current_ast_scope;
        let block = RefCell::new(Some(ir::BasicBlock {
            label: ir::Label { id },
            args,
            statements: Vec::new(),
            ast_scope,
        }));
        self.state.borrow().current_block.swap(&block);
        ir::Label { id }
    }

    fn finish_block(&self) -> Rc<RefCell<ir::BasicBlock>> {
        let bb = self.state.borrow().current_block.take();
        let block = Rc::new(RefCell::new(bb.unwrap()));
        self.mut_cfg(|cfg| {
            cfg.insert_node(Label::Just(block.borrow().label), block.clone());
        });
        block
    }

    fn add_bb_edge(&self, src: ir::Label, dst: ir::Label, edge: Edge) {
        let src = Label::Just(src);
        let dst = Label::Just(dst);
        self.mut_cfg(|cfg| {
            cfg.insert_edge(src, dst, Rc::new(RefCell::new(edge)));
        });
    }

    fn add_dummy_edge(&self, src: Label, dst: Label, edge: Edge) {
        self.mut_cfg(|cfg| {
            cfg.insert_edge(src, dst, Rc::new(RefCell::new(edge)));
        });
    }

    // Replace Label::LoopPred and Label::LoopExit with actual labels.
    fn tie_dummy_loop_edges(&self, pred: ir::Label, exit: ir::Label) {
        self.replace_cfg_label(Label::LoopPred, Label::Just(pred));
        self.replace_cfg_label(Label::LoopExit, Label::Just(exit));
    }

    fn tie_dummy_method_exit(&self, exit: ir::Label) {
        self.replace_cfg_label(Label::MethodExit, Label::Just(exit));
    }

    fn replace_cfg_label(&self, replace: Label, with: Label) {
        self.mut_cfg(|cfg| {
            let in_nodes = cfg
                .in_nodes(&replace)
                .into_iter()
                .map(|n| *n)
                .collect::<Vec<_>>();
            for i in in_nodes {
                let ed = cfg.remove_edge(&i, &replace).unwrap();
                cfg.insert_edge(i, with, ed);
            }
            let out_nodes = cfg
                .out_nodes(&replace)
                .into_iter()
                .map(|n| *n)
                .collect::<Vec<_>>();
            for o in out_nodes {
                let ed = cfg.remove_edge(&replace, &o).unwrap();
                cfg.insert_edge(with, o, ed);
            }
        });
    }

    fn new_local(&self, fld: &ast::FieldDecl) -> Rc<ir::VVar> {
        let scope = self.state.borrow().current_ast_scope;
        self.mut_cache(|cache| cache.new_local(scope, fld.ty.clone(), Some(fld.clone()), fld.span))
    }

    fn new_global(&self, fld: &ast::FieldDecl) -> Rc<ir::VVar> {
        self.mut_cache(|cache| cache.new_global(fld.ty.clone(), Some(fld.clone()), fld.span))
    }

    fn new_temp(&self, ty: &ast::Type) -> Rc<ir::VVar> {
        let scope = self.state.borrow().current_ast_scope;
        self.mut_cache(|cache| cache.new_local(scope, ty.clone(), None, None))
    }

    fn lookup_id(&self, id: &ast::ID) -> Rc<ir::VVar> {
        let mut scope = self.state.borrow().current_ast_scope;
        loop {
            let var = self
                .query_cache(|cache| cache.lookup_var(scope, id.id.as_str()).map(|v| v.clone()));
            if let Some(v) = var {
                return v;
            } else {
                match self.symbols.find_parent_scope(&scope) {
                    Some(s) => scope = *s,
                    None => break,
                }
            }
        }
        panic!()
    }

    fn visit_program(&mut self, p: &ast::Program) {
        // Handle imports
        for imp in &p.imports {
            self.program.borrow_mut().imports.push(imp.id.id.clone());
        }

        // Handle globals
        for fld in &p.fields {
            self.program.borrow_mut().globals.push(self.new_global(fld));
        }

        // Build cfg for each mehtod
        for m in &p.methods {
            let cfg = self.visit_method(m);
            self.program.borrow_mut().cfgs.insert(m.id.id.clone(), cfg);
        }
    }

    fn visit_method(&mut self, m: &ast::MethodDecl) -> PartialCFG {
        self.state.borrow().current_cfg.replace(Some(CFG::new(
            m.id.id.clone(),
            Label::MethodEntry,
            Label::MethodExit,
        )));
        let (entry, exit) = self.visit_block(&m.block, &m.arguments);
        let landing_pad = self.new_isolated_block(Vec::new());

        self.add_bb_edge(exit, landing_pad, Edge::Continue);
        self.tie_dummy_method_exit(landing_pad);
        self.mut_cfg(|cfg| {
            cfg.entry = Label::Just(entry);
            cfg.exit = Label::Just(landing_pad);
        });
        self.state.borrow().current_cfg.take().unwrap()
    }

    fn visit_block(
        &mut self,
        b: &ast::Block,
        method_args: &Vec<ast::FieldDecl>,
    ) -> (ir::Label, ir::Label) {
        let parent_scope = self.state.borrow().current_ast_scope;
        self.state.borrow_mut().current_ast_scope = b.scope;

        // First build an entry block with method arguments.
        let args = method_args.into_iter().map(|a| self.new_local(a)).collect();
        let entry = self.start_block(args);

        // Handle variable declarations
        for fld in &b.fields {
            self.new_local(fld);
        }

        // Handle statements
        for s in &b.statements {
            self.visit_statement(s);
        }

        let exit = self.finish_block().borrow().label;

        self.state.borrow_mut().current_ast_scope = parent_scope;

        (entry, exit)
    }

    fn visit_statement(&mut self, s: &ast::Statement) {
        match &s.stmt {
            ast::Stmt_::Assign(a) => {
                let val = self.visit_expr(&a.expr);
                self.write_to_location(&a.location, val);
            }
            ast::Stmt_::MethodCall(c) => {
                let args = c.arguments.iter().map(|e| self.visit_expr(&e)).collect();
                self.push_stmt(ir::Statement::Call {
                    dst: None,
                    method: c.name.id.clone(),
                    arguments: args,
                });
            }
            ast::Stmt_::If(i) => {
                let pred_val = self.visit_expr(&i.pred);
                let head = self.finish_block().borrow().label;
                let (if_entry, if_exit) = self.visit_block(&i.if_block, &Vec::new());
                if let Some(else_block) = &i.else_block {
                    let (else_entry, else_exit) = self.visit_block(else_block, &Vec::new());
                    let landing_pad = self.start_block(Vec::new());
                    self.add_bb_edge(head, if_entry, Edge::JumpTrue(pred_val.clone()));
                    self.add_bb_edge(head, else_entry, Edge::JumpFalse(pred_val));
                    self.add_bb_edge(if_exit, landing_pad, Edge::Continue);
                    self.add_bb_edge(else_exit, landing_pad, Edge::Continue);
                } else {
                    let landing_pad = self.start_block(Vec::new());
                    self.add_bb_edge(head, if_entry, Edge::JumpTrue(pred_val));
                    self.add_bb_edge(if_exit, landing_pad, Edge::Continue);
                    self.add_bb_edge(head, landing_pad, Edge::Continue);
                }
            }
            ast::Stmt_::For(f) => {
                // Handle the init statement of this for-loop
                self.visit_statement(f.init.as_ref());

                // Then transform this for-loop into a while-loop
                let mut stmts = f.block.statements.clone();
                stmts.push(f.update.as_ref().clone());
                let w = ast::Statement {
                    stmt: ast::Stmt_::While(ast::While {
                        pred: f.pred.clone(),
                        block: ast::Block {
                            scope: f.block.scope,
                            fields: f.block.fields.clone(),
                            statements: stmts,
                            span: f.block.span,
                        },
                    }),
                    span: s.span,
                };

                self.visit_statement(&w);
            }
            ast::Stmt_::While(w) => {
                let head = self.finish_block().borrow().label;

                self.start_block(Vec::new());
                let pred_val = self.visit_expr(&w.pred);
                let pred_bb = self.finish_block().borrow().label;

                let (body_entry, body_exit) = self.visit_block(&w.block, &Vec::new());

                let landing_pad = self.start_block(Vec::new());

                self.add_bb_edge(head, pred_bb, Edge::Continue);
                self.add_bb_edge(pred_bb, body_entry, Edge::JumpTrue(pred_val.clone()));
                self.add_bb_edge(body_exit, pred_bb, Edge::Continue);
                self.add_bb_edge(pred_bb, landing_pad, Edge::JumpFalse(pred_val));

                self.tie_dummy_loop_edges(pred_bb, landing_pad);
            }
            ast::Stmt_::Return(e) => {
                let val = e.as_ref().map(|e| self.visit_expr(&e));
                self.push_stmt(ir::Statement::Return(val));
                let exit = self.finish_block().borrow().label;
                // Create a unreachable block.
                self.start_block(Vec::new());
                self.add_dummy_edge(Label::Just(exit), Label::MethodExit, Edge::Continue);
            }
            ast::Stmt_::Break => {
                let exit = self.finish_block().borrow().label;
                self.start_block(Vec::new());
                self.add_dummy_edge(Label::Just(exit), Label::LoopExit, Edge::Continue);
            }
            ast::Stmt_::Continue => {
                let exit = self.finish_block().borrow().label;
                self.start_block(Vec::new());
                self.add_dummy_edge(Label::Just(exit), Label::LoopPred, Edge::Continue);
            }
        }
    }

    fn visit_expr(&self, expr: &ast::Expr) -> ir::Val {
        match &expr.expr {
            ast::Expr_::Location(l) => self.read_from_location(l),
            ast::Expr_::MethodCall(c) => {
                let args = c.arguments.iter().map(|e| self.visit_expr(&e)).collect();
                let ty = match &self.symbols.find_method_decl(c.name.id.as_str()).unwrap() {
                    semantic::MethodOrImport::Method(decl) => Some(decl.ty.clone()),
                    _ => None,
                };
                assert!(ty.is_some());
                let local = self.new_temp(&ty.unwrap());
                self.push_stmt(ir::Statement::Call {
                    dst: Some(local.clone()),
                    method: c.name.id.clone(),
                    arguments: args,
                });
                ir::Val::Var(local)
            }
            ast::Expr_::Literal(l) => ir::Val::Imm(l.clone()),
            ast::Expr_::Len(id) => {
                let ty = &self
                    .symbols
                    .find_var_decl(&self.get_scope(), id.id.as_str())
                    .unwrap()
                    .ty;
                ir::Val::Imm(ast::Literal::Int(ty.array_len()))
            }
            ast::Expr_::Arith(l, op, r) => {
                let lval = self.visit_expr(l.as_ref());
                let rval = self.visit_expr(r.as_ref());
                let local = self.new_temp(&ast::Type::Int);
                self.push_stmt(ir::Statement::Arith {
                    dst: local.clone(),
                    op: *op,
                    l: lval,
                    r: rval,
                });
                ir::Val::Var(local)
            }
            ast::Expr_::Cmp(l, op, r) => {
                let lval = self.visit_expr(l.as_ref());
                let rval = self.visit_expr(r.as_ref());
                let local = self.new_temp(&ast::Type::Bool);
                self.push_stmt(ir::Statement::Cmp {
                    dst: local.clone(),
                    op: *op,
                    l: lval,
                    r: rval,
                });
                ir::Val::Var(local)
            }
            ast::Expr_::Cond(l, op, r) => {
                let lval = self.visit_expr(l.as_ref());
                let rval = self.visit_expr(r.as_ref());
                let local = self.new_temp(&ast::Type::Bool);
                self.push_stmt(ir::Statement::Cond {
                    dst: local.clone(),
                    op: *op,
                    l: lval,
                    r: rval,
                });
                ir::Val::Var(local)
            }
            ast::Expr_::NNeg(v) => {
                let val = self.visit_expr(v.as_ref());
                let local = self.new_temp(&v.ty);
                self.push_stmt(ir::Statement::NNeg {
                    dst: local.clone(),
                    val,
                });
                ir::Val::Var(local)
            }
            ast::Expr_::LNeg(v) => {
                let val = self.visit_expr(v.as_ref());
                let local = self.new_temp(&ast::Type::Bool);
                self.push_stmt(ir::Statement::LNeg {
                    dst: local.clone(),
                    val,
                });
                ir::Val::Var(local)
            }
            ast::Expr_::TernaryOp(pred, t, f) => {
                // Build a control flow for choice op
                let pred_val = self.visit_expr(&pred);
                let var = self.new_temp(&expr.ty);
                let head = self.finish_block().borrow().label;
                let br = |e| {
                    let entry = self.start_block(Vec::new());
                    let val = self.visit_expr(e);
                    self.push_stmt(ir::Statement::Assign {
                        dst: var.clone(),
                        src: val,
                    });
                    let exit = self.finish_block().borrow().label;
                    (entry, exit)
                };
                let (t_entry, t_exit) = br(t);
                let (f_entry, f_exit) = br(f);
                let landing_pad = self.start_block(Vec::new());
                self.add_bb_edge(head, t_entry, Edge::JumpTrue(pred_val.clone()));
                self.add_bb_edge(head, f_entry, Edge::JumpFalse(pred_val));
                self.add_bb_edge(t_exit, landing_pad, Edge::Continue);
                self.add_bb_edge(f_exit, landing_pad, Edge::Continue);
                ir::Val::Var(var)
            }
        }
    }

    fn array_element_ty(t: &ast::Type) -> Option<&ast::Type> {
        match t {
            ast::Type::Array(t, _) => Some(t.as_ref()),
            _ => None,
        }
    }

    fn vector_deref(&self, id: &ast::ID, expr: &ast::Expr) -> Rc<ir::VVar> {
        let var_vector = self.lookup_id(id);
        let ele_ty = Self::array_element_ty(&var_vector.var.ty);
        assert!(ele_ty.is_some());
        let ptr_ty = ast::Type::Ptr(Box::new(ele_ty.unwrap().clone()));

        let idx_var = self.visit_expr(expr);
        let offset_var = self.new_temp(&ptr_ty);
        self.push_stmt(ir::Statement::Arith {
            dst: offset_var.clone(),
            op: ast::ArithOp::Mul,
            l: ir::Val::Imm(ast::Literal::Int(ele_ty.unwrap().size())),
            r: idx_var,
        });

        let ele_ptr = self.new_temp(&ptr_ty);
        self.push_stmt(ir::Statement::Arith {
            dst: ele_ptr.clone(),
            op: ast::ArithOp::Add,
            l: ir::Val::Var(var_vector.clone()),
            r: ir::Val::Var(offset_var),
        });

        ele_ptr
    }

    fn read_from_location(&self, loc: &ast::Location) -> ir::Val {
        match &loc {
            ast::Location::Scalar(id) => {
                let vvar = self.lookup_id(id);
                match &vvar.var.locality {
                    ir::Locality::Local => return ir::Val::Var(vvar),
                    ir::Locality::Global => {
                        let dst = self.new_temp(&vvar.var.ty);
                        self.push_stmt(ir::Statement::Load {
                            dst: dst.clone(),
                            ptr: ir::Val::Var(vvar),
                        });
                        return ir::Val::Var(dst);
                    }
                }
            }
            ast::Location::Vector(id, idx_expr) => {
                let vvar = self.lookup_id(id);
                let ele_ptr = self.vector_deref(id, idx_expr);
                let dst = self.new_temp(Self::array_element_ty(&vvar.var.ty).unwrap());
                self.push_stmt(ir::Statement::Load {
                    dst: dst.clone(),
                    ptr: ir::Val::Var(ele_ptr),
                });
                return ir::Val::Var(dst);
            }
        }
    }

    fn write_to_location(&self, loc: &ast::Location, val: ir::Val) {
        match &loc {
            ast::Location::Scalar(id) => {
                let vvar = self.lookup_id(id);
                match &vvar.var.locality {
                    ir::Locality::Local => {
                        self.push_stmt(ir::Statement::Assign { dst: vvar, src: val });
                    }
                    ir::Locality::Global => {
                        self.push_stmt(ir::Statement::Store {
                            ptr: ir::Val::Var(vvar),
                            src: val,
                        });
                    }
                }
            }
            ast::Location::Vector(id, idx) => {
                let ele_ptr = self.vector_deref(id, idx);
                self.push_stmt(ir::Statement::Store {
                    ptr: ir::Val::Var(ele_ptr),
                    src: val,
                });
            }
        }
    }
}
