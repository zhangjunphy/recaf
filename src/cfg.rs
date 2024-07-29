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
    pub nodes: HashMap<usize, ir::BasicBlock>,
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
        ast_scope: usize,
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
            self.var_to_ast_scope.insert(id, consts::ROOT_BLOCK_ID);
            self.ast_symbol_to_var
                .insert((consts::ROOT_BLOCK_ID, d.id.id.clone()), id);
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
    current_ast_scope: usize,
    basic_block_to_ast_scope: HashMap<usize, usize>,
    var_map: VarMap,
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
                current_ast_scope: 0,
                basic_block_to_ast_scope: HashMap::new(),
                var_map: VarMap::new(),
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

    fn new_temp(&self, tpe: ast::Type) -> Rc<ir::Var> {
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
        let mut cfg = CFG {
            name: m.id.id.clone(),
            nodes: HashMap::new(),
        };
        self.visit_block(&mut cfg, &m.block, &m.arguments);
        self.program.cfgs.insert(m.id.id.clone(), cfg);
    }

    fn visit_block(&mut self, cfg: &mut CFG, b: &ast::Block, method_args: &Vec<ast::FieldDecl>) {
        let parent_scope = self.state.borrow().current_ast_scope;
        self.state.borrow_mut().current_ast_scope = b.id;

        // First build an entry block with method arguments.
        let label = ir::Label::new(self.new_block_id());
        let mut args = Vec::new();
        for arg in method_args {
            let var = self.new_local(arg);
            args.push(var.clone());
        }
        let entry = ir::BasicBlock {
            label,
            args,
            statements: Vec::new(),
            br: None,
        };

        // Handle local variables
        for fld in &b.fields {
            self.new_local(fld);
        }

        // Handle statements
        let block = RefCell::new(entry);
        for s in &b.statements {
            self.visit_statement(cfg, &block, s);
        }

        self.state.borrow_mut().current_ast_scope = parent_scope;
    }

    fn visit_statement(
        &mut self,
        cfg: &mut CFG,
        block: &RefCell<ir::BasicBlock>,
        s: &ast::Statement,
    ) {
        match &s.stmt {
            ast::Stmt_::Assign(assign) => (),
            _ => (),
        }
    }

    fn visit_assign(&mut self, block: &RefCell<ir::BasicBlock>, asn: &ast::Assign) {
        let dst = self.visit_location(&asn.location);
    }

    fn visit_expr(&mut self, block: &RefCell<ir::BasicBlock>, asn: &ast::Assign) -> Rc<ir::Val> {}

    fn read_location(
        &mut self,
        block: &RefCell<ir::BasicBlock>,
        loc: &ast::Location,
    ) -> Rc<ir::Val> {
        match &loc {
            ast::Location::Scalar(id) => {
                let var = self.lookup_id(id);
                match &var.locality {
                    ir::Locality::Local => return Rc::new(ir::Val::Var((*var).clone())),
                    ir::Locality::Global => {
                        let dst = self.new_temp(var.tpe);
                        let load = ir::Statements::Load {
                            dst: (*dst).clone(),
                            ptr: ir::Val::Var((*var).clone()),
                        };
                        block.borrow_mut().statements.push(load);
                    }
                }
            }
        }
        ()
    }
}
