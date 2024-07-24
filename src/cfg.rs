use crate::ast;
use crate::ir;
use crate::semantic;
use crate::source_pos::SrcSpan;
use std::collections::HashMap;
use std::rc::Rc;

pub struct CFG {
    pub nodes: HashMap<usize, Box<ir::BasicBlock>>,
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

    fn new_local(
        &mut self,
        ast_scope: usize,
        tpe: ast::Type,
        decl: Option<ast::FieldDecl>,
        span: Option<SrcSpan>,
    ) -> &Rc<ir::Var> {
        let id = self.next_var_id;
        self.next_var_id += 1;
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
        self.vars.last().unwrap()
    }
}

pub struct BuildState {
    next_block_id: usize,
    basic_block_to_ast_scope: HashMap<usize, usize>,
    statements: Box<Vec<ir::Statements>>,
    var_map: VarMap,
}

pub struct CFGBuild<'s> {
    pub symbols: &'s semantic::ProgramSymbols,
    pub state: BuildState,
}

impl<'s> CFGBuild<'s> {
    pub fn new(symbols: &'s semantic::ProgramSymbols) -> Self {
        CFGBuild {
            symbols,
            state: BuildState {
                next_block_id: 1, // reserve block 0 for global variables
                basic_block_to_ast_scope: HashMap::new(),
                statements: Box::new(Vec::new()),
                var_map: VarMap::new(),
            },
        }
    }

    pub fn build(&mut self, p: &ast::Program) -> HashMap<String, Box<CFG>> {
        let mut res = HashMap::new();
        for m in &p.methods {
            let name = m.id.id.clone();
            let cfg = self.build_method(m);
            res.insert(name, cfg);
        }
        res
    }

    fn next_block_id(&mut self) -> usize {
        let id = self.state.next_block_id;
        self.state.next_block_id += 1;
        id
    }

    fn build_method(&mut self, m: &ast::MethodDecl) {
        // First build an entry block with method arguments.
        let label = ir::Label::new(self.next_block_id());
        let mut args = Vec::new();
        for arg in &m.arguments {
            let var = self.state.var_map.new_local(
                m.block.id,
                arg.tpe.clone(),
                Some(arg.clone()),
                arg.span.clone(),
            );
            args.push(var);
        }
    }
}
