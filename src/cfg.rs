use crate::ast;
use crate::consts;
use crate::ir;
use crate::semantic;
use crate::source_pos::SrcSpan;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct CFG {
    pub name: String,
    pub nodes: HashMap<usize, Rc<ir::BasicBlock>>,
    pub edges: HashMap<usize, Vec<(usize, ir::Branch)>>,
    pub redges: HashMap<usize, Vec<usize>>,
}

pub struct Program {
    pub imports: Vec<String>,
    pub globals: Vec<Rc<ir::Var>>,
    pub cfgs: HashMap<String, CFG>,
}

pub struct VarMap {
    var_to_ast_scope: HashMap<usize, usize>,
    ast_symbol_to_var: HashMap<(usize, String), usize>,
    vars: Vec<Rc<ir::Var>>,
    next_var_id: usize,
}

impl VarMap {
    fn new() -> Self {
        VarMap {
            var_to_ast_scope: HashMap::new(),
            ast_symbol_to_var: HashMap::new(),
            vars: Vec::new(),
            next_var_id: 0,
        }
    }

    fn lookup_var(&self, ast_scope: usize, symbol: &str) -> Option<&Rc<ir::Var>> {
        self.ast_symbol_to_var
            .get(&(ast_scope, symbol.to_string()))
            .and_then(|idx| self.vars.get(*idx))
    }

    fn lookup_decl_and_scope(&self, v: &ir::Var) -> Option<&usize> {
        self.var_to_ast_scope.get(&v.id)
    }

    fn new_var_id(&mut self) -> usize {
        let id = self.next_var_id;
        self.next_var_id += 1;
        id
    }

    fn new_local(
        &mut self,
        ast_scope: ast::Scope,
        tpe: ast::Type,
        decl: Option<ast::FieldDecl>,
        span: Option<SrcSpan>,
    ) -> Rc<ir::Var> {
        let id = self.new_var_id();
        if let Some(d) = &decl {
            self.var_to_ast_scope.insert(id, ast_scope);
            self.ast_symbol_to_var
                .insert((ast_scope, d.id.id.clone()), id);
        }
        self.vars.push(Rc::new(ir::Var {
            id,
            tpe,
            decl,
            span,
            locality: ir::Locality::Local,
        }));
        self.vars.last().unwrap().clone()
    }

    fn new_global(
        &mut self,
        tpe: ast::Type,
        decl: Option<ast::FieldDecl>,
        span: Option<SrcSpan>,
    ) -> Rc<ir::Var> {
        let id = self.new_var_id();
        if let Some(d) = &decl {
            self.var_to_ast_scope.insert(id, consts::ROOT_SCOPE_ID);
            self.ast_symbol_to_var
                .insert((consts::ROOT_SCOPE_ID, d.id.id.clone()), id);
        }
        self.vars.push(Rc::new(ir::Var {
            id,
            tpe,
            decl,
            span,
            locality: ir::Locality::Global,
        }));
        self.vars.last().unwrap().clone()
    }
}

pub struct BuildState {
    next_block_id: usize,
    current_ast_scope: ast::Scope,
    var_map: VarMap,
    current_cfg: RefCell<Option<CFG>>,
    current_block: RefCell<Option<ir::BasicBlock>>,
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
                next_block_id: 1, // reserve block 0 for global variables
                current_ast_scope: ast::Scope::new(consts::ROOT_SCOPE_ID),
                var_map: VarMap::new(),
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

    fn new_block_id(&mut self) -> usize {
        let id = self.state.borrow().next_block_id;
        self.state.borrow_mut().next_block_id += 1;
        id
    }

    fn new_local(&self, fld: &ast::FieldDecl) -> Rc<ir::Var> {
        let scope = self.state.borrow().current_ast_scope;
        self.state.borrow_mut().var_map.new_local(
            scope,
            fld.tpe.clone(),
            Some(fld.clone()),
            fld.span,
        )
    }

    fn new_temp(&self, tpe: &ast::Type) -> Rc<ir::Var> {
        let scope = self.state.borrow().current_ast_scope;
        self.state
            .borrow_mut()
            .var_map
            .new_local(scope, tpe.clone(), None, None)
    }

    fn lookup_id(&self, id: &ast::ID) -> Rc<ir::Var> {
        let scope = self.state.borrow().current_ast_scope;
        self.state
            .borrow()
            .var_map
            .lookup_var(scope, id.id.as_str())
            .unwrap()
            .clone()
    }

    fn push_stmt(&self, stmt: ir::Statement) {
        self.state
            .borrow_mut()
            .current_block
            .borrow_mut()
            .as_ref()
            .unwrap()
            .push_stmt(stmt)
    }

    fn pop_current_block(&self) -> Rc<ir::BasicBlock> {
        let dummy = RefCell::new(None);
        self.state.borrow().current_block.swap(&dummy);
        let block = Rc::new(dummy.into_inner().unwrap());
        self.state
            .borrow()
            .current_cfg
            .borrow_mut()
            .as_ref()
            .unwrap()
            .nodes
            .insert(block.label.id, block.clone());
        block
    }

    fn push_current_block(&self, id: usize, args: Vec<Rc<ir::Var>>) {
        assert!(self.state.borrow().current_block.borrow().is_none());
        let ast_scope = self.state.borrow().current_ast_scope;
        let block = RefCell::new(Some(ir::BasicBlock {
            label: ir::Label { id },
            args,
            statements: Vec::new(),
            br: None,
            ast_scope,
        }));
        self.state.borrow().current_block.swap(&block);
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
            let var = self.state.borrow_mut().var_map.new_global(
                fld.tpe.clone(),
                Some(fld.clone()),
                fld.span,
            );
            self.program.globals.push(var);
        }

        // Build cfg for each mehtod
        for m in &p.methods {
            self.visit_method(m);
        }
    }

    fn visit_method(&mut self, m: &ast::MethodDecl) {
        self.visit_block(&m.block, &m.arguments);
    }

    fn visit_block(&mut self, b: &ast::Block, method_args: &Vec<ast::FieldDecl>) -> (usize, usize) {
        let parent_scope = self.state.borrow().current_ast_scope;
        self.state.borrow_mut().current_ast_scope = b.id;

        // First build an entry block with method arguments.
        let mut args = Vec::new();
        for arg in method_args {
            let var = self.new_local(arg);
            args.push(var.clone());
        }
        let entry = self.new_block_id();
        self.push_current_block(entry, args);

        // Handle local variables
        for fld in &b.fields {
            self.new_local(fld);
        }

        // Handle statements
        for s in &b.statements {
            self.visit_statement(s);
        }

        let exit = self.pop_current_block().label.id;

        self.state.borrow_mut().current_ast_scope = parent_scope;

        (entry, exit)
    }

    fn visit_statement(&mut self, s: &ast::Statement) {
        match &s.stmt {
            ast::Stmt_::Assign(assign) => {
                let val = self.visit_expr(&assign.expr);
                self.write_to_location(&assign.location, val);
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
                let pred_var = self.visit_expr(&i.pred);
                let prev_block = self.pop_current_block();
                let (if_entry, if_exit) = self.visit_block(&i.if_block, &Vec::new());
                if let Some(else_block) = &i.else_block {
                    let (else_entry, else_exit) = self.visit_block(else_block, &Vec::new());
                }
            }
            _ => (),
        }
    }

    fn visit_assign(&mut self, asn: &ast::Assign) {}

    fn visit_expr(&mut self, expr: &ast::Expr) -> ir::Val {}

    fn array_element_tpe(t: &ast::Type) -> Option<&ast::Type> {
        match t {
            ast::Type::Array(t, _) => Some(t.as_ref()),
            _ => None,
        }
    }

    fn ptr_underlying_tpe(t: &ast::Type) -> Option<&ast::Type> {
        match t {
            ast::Type::Ptr(t) => Some(t.as_ref()),
            _ => None,
        }
    }

    fn vector_deref(&mut self, id: &ast::ID, expr: &ast::Expr) -> Rc<ir::Var> {
        let var_vector = self.lookup_id(id);
        let ele_tpe = Self::array_element_tpe(&var_vector.tpe);
        assert!(ele_tpe.is_some());
        let ptr_tpe = ast::Type::Ptr(Box::new(ele_tpe.unwrap().clone()));

        let idx_var = self.visit_expr(expr);
        let offset_var = self.new_temp(&ptr_tpe);
        self.push_stmt(ir::Statement::Arith {
            dst: offset_var.clone(),
            op: ir::ArithOp::Mul,
            l: ir::Val::Imm(ast::Literal::Int(ele_tpe.unwrap().size())),
            r: idx_var,
        });

        let ele_ptr = self.new_temp(&ptr_tpe);
        self.push_stmt(ir::Statement::Arith {
            dst: ele_ptr.clone(),
            op: ir::ArithOp::Add,
            l: ir::Val::Var(var_vector.clone()),
            r: ir::Val::Var(offset_var),
        });

        ele_ptr
    }

    fn read_from_location(&mut self, loc: &ast::Location) -> Rc<ir::Val> {
        match &loc {
            ast::Location::Scalar(id) => {
                let var = self.lookup_id(id);
                match &var.locality {
                    ir::Locality::Local => return Rc::new(ir::Val::Var(var)),
                    ir::Locality::Global => {
                        let dst = self.new_temp(&var.tpe);
                        self.push_stmt(ir::Statement::Load {
                            dst: dst.clone(),
                            ptr: ir::Val::Var(var),
                        });
                        return Rc::new(ir::Val::Var(dst));
                    }
                }
            }
            ast::Location::Vector(id, idx_expr) => {
                let var = self.lookup_id(id);
                let ele_ptr = self.vector_deref(id, idx_expr);
                let dst = self.new_temp(Self::array_element_tpe(&var.tpe).unwrap());
                self.push_stmt(ir::Statement::Load {
                    dst: dst.clone(),
                    ptr: ir::Val::Var(ele_ptr),
                });
                return Rc::new(ir::Val::Var(dst));
            }
        }
    }

    fn write_to_location(&mut self, loc: &ast::Location, val: ir::Val) {
        match &loc {
            ast::Location::Scalar(id) => {
                let var = self.lookup_id(id);
                match &var.locality {
                    ir::Locality::Local => {
                        let decl = self.find_decl(id.id.as_str());
                        let dst = self.new_local(decl.unwrap());
                        self.push_stmt(ir::Statement::Assign { dst, src: val });
                    }
                    ir::Locality::Global => {
                        self.push_stmt(ir::Statement::Store {
                            ptr: ir::Val::Var(var),
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
