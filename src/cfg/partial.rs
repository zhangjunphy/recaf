use crate::ast;
use crate::cfg::def::CFG;
use crate::consts;
use crate::ir;
use crate::semantic;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct BasicBlock {
    pub label: ir::Label,
    pub ast_scope: ast::Scope,
    pub args: Vec<Rc<ast::FieldDecl>>,
    pub statements: Vec<ast::Statement>,
}

pub enum Edge {
    Continue,
    JumpTrue(ast::Expr),
    JumpFalse,
}

impl BasicBlock {
    fn push_stmt(&mut self, stmt: ast::Statement) {
        self.statements.push(stmt);
    }
}

pub struct Program {
    pub imports: Vec<String>,
    pub globals: Vec<Rc<ast::FieldDecl>>,
    pub cfgs: HashMap<String, CFG<BasicBlock, Edge>>,
}

pub struct BuildState {
    next_bb_id: usize,
    current_ast_scope: ast::Scope,
    current_cfg: RefCell<Option<CFG<BasicBlock, Edge>>>,
    current_block: RefCell<Option<BasicBlock>>,
    current_loop: Option<(Label, Label)>, // pred and landing_pad block
}

pub struct CFGBuild<'s> {
    pub symbols: &'s semantic::ProgramSymbols,
    pub state: RefCell<BuildState>,
    pub program: Program,
}

impl<'s> CFGBuild<'s> {
    pub fn new(symbols: &'s semantic::ProgramSymbols) -> Self {
        CFGBuild {
            symbols,
            state: RefCell::new(BuildState {
                next_bb_id: 1, // reserve block 0 for global variables
                current_ast_scope: ast::Scope::new(consts::ROOT_SCOPE_ID),
                current_cfg: RefCell::new(None),
                current_block: RefCell::new(None),
            }),
            program: Program {
                imports: Vec::new(),
                globals: Vec::new(),
                cfgs: HashMap::new(),
            },
        }
    }

    fn new_block_id(&self) -> usize {
        let id = self.state.borrow().next_bb_id;
        self.state.borrow_mut().next_bb_id += 1;
        id
    }

    fn push_stmt_to_block(&self, l: &ir::Label, stmt: ast::Statement) {
        self.state
            .borrow()
            .current_cfg
            .borrow_mut()
            .as_mut()
            .unwrap()
            .nodes
            .get(&l)
            .unwrap()
            .push_stmt(stmt);
    }
    fn push_stmt(&self, stmt: ast::Statement) {
        self.state
            .borrow_mut()
            .current_block
            .borrow_mut()
            .as_mut()
            .unwrap()
            .push_stmt(stmt)
    }

    fn new_block(&self, args: Vec<Rc<ast::FieldDecl>>) -> ir::Label {
        let id = self.new_block_id();
        assert!(self.state.borrow().current_block.borrow().is_none());
        let ast_scope = self.state.borrow().current_ast_scope;
        let block = RefCell::new(Some(BasicBlock {
            label: ir::Label { id },
            args,
            statements: Vec::new(),
            ast_scope,
        }));
        self.state.borrow().current_block.swap(&block);
        ir::Label { id }
    }

    fn pop_current_block(&self) -> Rc<BasicBlock> {
        let bb = self.state.borrow().current_block.take();
        let block = Rc::new(bb.unwrap());
        self.state
            .borrow()
            .current_cfg
            .borrow_mut()
            .as_mut()
            .unwrap()
            .nodes
            .insert(block.label, block.clone());
        block
    }

    fn add_bb_edge(&self, src: ir::Label, dst: ir::Label, edge: Edge) {
        let state = self.state.borrow();
        let mut cfg = state.current_cfg.borrow_mut();
        cfg.as_mut()
            .unwrap()
            .edges
            .entry(src)
            .or_insert(Vec::new())
            .push((dst, edge));
        cfg.as_mut()
            .unwrap()
            .redges
            .entry(dst)
            .or_insert(Vec::new())
            .push(src);
    }

    fn find_decl(&self, id: &str) -> Option<&ast::FieldDecl> {
        let scope = self.state.borrow().current_ast_scope;
        self.symbols.find_var_decl(&scope, id)
    }

    fn visit_program(&mut self, p: &ast::Program) {
        // Handle imports
        for imp in &p.imports {
            self.program.imports.push(imp.id.id.clone());
        }

        // Handle globals
        for fld in &p.fields {
            self.program.globals.push(Rc::new(fld.clone()));
        }

        // Build cfg for each mehtod
        for m in &p.methods {
            self.visit_method(m);
        }
    }

    fn visit_method(&mut self, m: &ast::MethodDecl) {
        self.visit_block(&m.block, &m.arguments);
    }

    fn visit_block(
        &mut self,
        b: &ast::Block,
        method_args: &Vec<ast::FieldDecl>,
    ) -> (ir::Label, ir::Label) {
        let parent_scope = self.state.borrow().current_ast_scope;
        self.state.borrow_mut().current_ast_scope = b.scope;

        // First build an entry block with method arguments.
        let mut args = Vec::new();
        method_args.into_iter().for_each(|arg| {
            args.push(Rc::new(arg.clone()));
        });
        let entry = self.new_block(args);

        // Handle statements
        for s in &b.statements {
            self.visit_statement(s);
        }

        let exit = self.pop_current_block().label;

        self.state.borrow_mut().current_ast_scope = parent_scope;

        (entry, exit)
    }

    fn visit_statement(&mut self, s: &ast::Statement) {
        match &s.stmt {
            ast::Stmt_::Assign(_) | ast::Stmt_::MethodCall(_) => {
                self.push_stmt(s.clone());
            }
            ast::Stmt_::If(i) => {
                let head = self.pop_current_block().label;
                let (if_entry, if_exit) = self.visit_block(&i.if_block, &Vec::new());
                if let Some(else_block) = &i.else_block {
                    let (else_entry, else_exit) = self.visit_block(else_block, &Vec::new());
                    let landing_pad = self.new_block(Vec::new());
                    self.add_bb_edge(head, if_entry, Edge::JumpTrue(i.pred.clone()));
                    self.add_bb_edge(head, else_entry, Edge::JumpFalse);
                    self.add_bb_edge(if_exit, landing_pad, Edge::Continue);
                    self.add_bb_edge(else_exit, landing_pad, Edge::Continue);
                } else {
                    let landing_pad = self.new_block(Vec::new());
                    self.add_bb_edge(head, if_entry, Edge::JumpTrue(i.pred.clone()));
                    self.add_bb_edge(if_exit, landing_pad, Edge::Continue);
                    self.add_bb_edge(head, landing_pad, Edge::Continue);
                }
            }
            ast::Stmt_::For(f) => {
                self.push_stmt((*f.init).clone());
                let head = self.pop_current_block().label;

                self.new_block(Vec::new());
                let pred = self.pop_current_block().label;

                let (body_entry, body_exit) = self.visit_block(&f.block, &Vec::new());
                self.push_stmt_to_block(&body_exit, (*f.update).clone());

                let landing_pad = self.new_block(Vec::new());

                self.add_bb_edge(head, pred, Edge::Continue);
                self.add_bb_edge(pred, body_entry, Edge::JumpTrue(f.pred.clone()));
                self.add_bb_edge(body_exit, pred, Edge::Continue);
                self.add_bb_edge(pred, landing_pad, Edge::JumpFalse);
            }
            ast::Stmt_::While(w) => {
                let head = self.pop_current_block().label;

                self.new_block(Vec::new());
                let pred = self.pop_current_block().label;

                let (body_entry, body_exit) = self.visit_block(&w.block, &Vec::new());

                let landing_pad = self.new_block(Vec::new());

                self.add_bb_edge(head, pred, Edge::Continue);
                self.add_bb_edge(pred, body_entry, Edge::JumpTrue(w.pred.clone()));
                self.add_bb_edge(body_exit, pred, Edge::Continue);
                self.add_bb_edge(pred, landing_pad, Edge::JumpFalse);
            }
            _ => (),
        }
    }

    fn visit_expr(&mut self, expr: &ast::Expr) -> ir::Val {
        ir::Val::Imm(ast::Literal::Int(0))
    }
}
