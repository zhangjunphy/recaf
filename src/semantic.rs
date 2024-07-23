use crate::ast::*;
use crate::consts;
use crate::error::Error;
use crate::source_pos::SrcSpan;
use crate::{err, err_span};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct VariableTable<'p> {
    pub block_id: usize, // Block this table belongs to.
    pub parent_id: Option<usize>,
    pub variables: HashMap<String, &'p FieldDecl>,
}

impl<'p> VariableTable<'p> {
    fn new(block_id: usize, parent_id: Option<usize>) -> Self {
        VariableTable {
            block_id,
            parent_id,
            variables: HashMap::new(),
        }
    }
}

pub enum MethodOrImport<'p> {
    Method(&'p MethodDecl),
    Import(&'p str),
}

#[derive(Debug)]
pub struct ProgramSymbols<'p> {
    pub imports: HashSet<String>,
    pub methods: HashMap<String, &'p MethodDecl>,
    pub variables: HashMap<usize, VariableTable<'p>>,
}

impl<'p> ProgramSymbols<'p> {
    fn new() -> Self {
        ProgramSymbols {
            imports: HashSet::new(),
            methods: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    fn find_parent(&self, block: usize) -> Option<&VariableTable> {
        let parent = self.variables.get(&block).and_then(|t| t.parent_id)?;
        self.variables.get(&parent)
    }

    fn find_mut_parent(&mut self, block: usize) -> Option<&'p mut VariableTable> {
        let parent = self.variables.get(&block).and_then(|t| t.parent_id)?;
        self.variables.get_mut(&parent)
    }

    fn find_var_decl(&self, block: usize, var: &str) -> Option<&FieldDecl> {
        let lookup_non_recur = |block: usize, var: &str| {
            let table = self.variables.get(&block)?;
            let decl = table.variables.get(var)?;
            Some(*decl)
        };
        let mut cur_block = Some(block);
        while cur_block.is_some() {
            let res = lookup_non_recur(cur_block.unwrap(), var);
            if res.is_some() {
                return res;
            } else {
                cur_block = self
                    .find_parent(cur_block.unwrap())
                    .and_then(|t| Some(t.block_id));
            }
        }
        None
    }

    fn find_method_decl(&self, name: &'p str) -> Option<MethodOrImport> {
        if let Some(method) = self.methods.get(name) {
            Some(MethodOrImport::Method(method))
        } else if self.imports.contains(name) {
            Some(MethodOrImport::Import(name))
        } else {
            None
        }
    }

    fn add_table(&mut self, block_id: usize, parent_id: Option<usize>) -> Result<(), Error> {
        let old = self
            .variables
            .insert(block_id, VariableTable::new(block_id, parent_id));
        if old.is_some() {
            return Err(err!("Symbol table present for block {block_id}"));
        }
        Ok(())
    }

    fn add_table_if_nonexist(&mut self, block_id: usize, parent_id: Option<usize>) {
        if !self.variables.contains_key(&block_id) {
            self.variables
                .insert(block_id, VariableTable::new(block_id, parent_id));
        }
    }

    fn add_import(&mut self, imp: &ImportDecl) -> Result<(), Error> {
        let is_new = self.imports.insert(imp.id.id.clone());
        if !is_new {
            Err(err_span!(
                imp.span,
                "Imported method {} is a duplicate.",
                imp.id.id
            ))
        } else {
            Ok(())
        }
    }

    fn add_method(&mut self, m: &'p MethodDecl) -> Result<(), Error> {
        let is_new = self.methods.insert(m.id.id.clone(), m);
        if is_new.is_some() {
            Err(err_span!(
                m.span,
                "Duplicate method declaration for {}.",
                m.id.id
            ))
        } else {
            Ok(())
        }
    }

    fn add_variable(&mut self, block: usize, var: &'p FieldDecl) -> Result<(), Error> {
        if let Some(table) = self.variables.get_mut(&block) {
            if let Some(old) = table.variables.insert(var.id.id.clone(), var) {
                if let Some(span) = old.span {
                    Err(err_span!(
                        var.span,
                        "Dupilcate declaration for {}, previous at {}",
                        var.id.id,
                        span
                    ))
                } else {
                    Err(err_span!(
                        var.span,
                        "Dupilcate declaration for {}, previous declation unlocatable",
                        var.id.id
                    ))
                }
            } else {
                Ok(())
            }
        } else {
            Err(err_span!(
                var.span,
                "Symbol table for block {block} does not exist!"
            ))
        }
    }
}

pub struct SymbolTableBuilder<'p> {
    symbols: ProgramSymbols<'p>,
}

impl<'p> SymbolTableBuilder<'p> {
    pub fn new() -> SymbolTableBuilder<'p> {
        SymbolTableBuilder {
            symbols: ProgramSymbols::new(),
        }
    }

    pub fn process(&mut self, program: &'p Program) -> Result<&ProgramSymbols, Error> {
        self.process_program(program)?;
        Ok(&self.symbols)
    }

    fn add_root(&mut self) -> Result<(), Error> {
        let old = self.symbols.variables.insert(
            consts::ROOT_BLOCK_ID,
            VariableTable::new(consts::ROOT_BLOCK_ID, None),
        );
        if old.is_some() {
            return Err(err!("Symbol table present for program root"));
        }
        Ok(())
    }

    fn process_program(&mut self, program: &'p Program) -> Result<(), Error> {
        self.add_root()?;
        for imp in &program.imports {
            self.symbols.add_import(imp)?;
        }
        for var in &program.fields {
            self.symbols.add_variable(consts::ROOT_BLOCK_ID, var)?;
        }
        for method in &program.methods {
            self.symbols.add_method(method)?;
            self.process_method(method)?;
        }
        Ok(())
    }

    fn process_method(&mut self, m: &'p MethodDecl) -> Result<(), Error> {
        self.symbols
            .add_table(m.block.id, Some(consts::ROOT_BLOCK_ID))?;
        for v in &m.arguments {
            self.symbols.add_variable(m.block.id, v)?;
        }
        self.process_block(&m.block, Some(consts::ROOT_BLOCK_ID))?;
        Ok(())
    }

    fn process_block(&mut self, b: &'p Block, parent: Option<usize>) -> Result<(), Error> {
        self.symbols.add_table_if_nonexist(b.id, parent);
        for v in &b.fields {
            self.symbols.add_variable(b.id, v)?;
        }
        for s in &b.statements {
            self.process_stmt(s, b.id)?;
        }
        Ok(())
    }

    fn process_stmt(&mut self, s: &'p Statement, block_id: usize) -> Result<(), Error> {
        let s_ = &s.stmt;
        match s_ {
            Stmt_::If(i) => {
                self.process_block(&i.if_block, Some(block_id))?;
                if let Some(else_block) = &i.else_block {
                    self.process_block(else_block, Some(block_id))?;
                }
            }
            Stmt_::While(w) => {
                self.process_block(&w.block, Some(block_id))?;
            }
            _ => (),
        }
        Ok(())
    }
}

// Check semantics. Annotate expression types.
pub struct SemanticChecker<'p> {
    symbols: &'p ProgramSymbols<'p>,
    current_method: RefCell<Option<String>>,
    current_block: RefCell<usize>,
    current_loop: RefCell<Option<usize>>,
    errors: RefCell<Vec<Error>>,
}

impl<'p> SemanticChecker<'p> {
    pub fn new(symbols: &'p ProgramSymbols<'p>) -> SemanticChecker<'p> {
        SemanticChecker {
            symbols,
            current_method: RefCell::new(None),
            current_block: RefCell::new(consts::ROOT_BLOCK_ID),
            current_loop: RefCell::new(None),
            errors: RefCell::new(Vec::new()),
        }
    }

    pub fn check(&self, program: &'p mut Program) -> &RefCell<Vec<Error>> {
        self.check_main();
        for m in &mut program.methods {
            self.check_method_decl(m);
        }
        &self.errors
    }

    fn check_main(&self) {
        let fmain = self.symbols.methods.get(consts::MAIN_FUNC_NAME);
        match fmain {
            None => self.err(err!("Method \"main\" does not exist.")),
            Some(f) => {
                if f.tpe != Type::Void {
                    self.err(err!("Method \"main\" does not return void."));
                } else if f.arguments.is_empty() {
                    self.err(err!("Method \"main\" should not have any arguments."));
                }
            }
        }
    }
    fn check_method_decl(&self, m: &mut MethodDecl) {
        *self.current_method.borrow_mut() = Some(m.id.id.clone());
        self.check_block(&mut m.block);
        *self.current_method.borrow_mut() = None;
    }

    fn check_block(&self, b: &mut Block) {
        let previous_block = *self.current_block.borrow();
        *self.current_block.borrow_mut() = b.id;
        for s in &mut b.statements {
            self.check_statement(s);
        }
        *self.current_block.borrow_mut() = previous_block;
    }

    fn check_statement(&self, s: &mut Statement) {
        match &mut s.stmt {
            Stmt_::Assign(assign) => {
                let loc_tpe = self.check_loc_access(&mut assign.location);
                let expr_tpe = self.check_expr(&mut assign.expr);
                if loc_tpe != expr_tpe {
                    self.err(err_span!(
                        s.span,
                        "{loc_tpe} type expected, but expression has {expr_tpe}.",
                    ));
                }
            }
            Stmt_::MethodCall(c) => {
                self.check_method_call(c, &s.span);
            }
            Stmt_::If(i) => {
                self.check_pred(&mut i.pred);
                self.check_block(&mut i.if_block);
                if let Some(else_block) = &mut i.else_block {
                    self.check_block(else_block);
                }
            }
            Stmt_::For(f) => {
                self.check_statement(&mut f.init);
                self.check_pred(&mut f.pred);
                self.check_statement(&mut f.update);
                self.check_loop_block(&mut f.block);
            }
            Stmt_::While(w) => {
                self.check_pred(&mut w.pred);
                self.check_loop_block(&mut w.block);
            }
            Stmt_::Return(r) => {
                self.check_return(r, &s.span);
            }
            Stmt_::Break => {
                if self.current_loop.borrow().is_none() {
                    self.err(err_span!(s.span, "break statement out of any loop."));
                }
            }
            Stmt_::Continue => {
                if self.current_loop.borrow().is_none() {
                    self.err(err_span!(s.span, "continue statement out of any loop."));
                }
            }
        }
    }

    fn check_return(&self, r: &mut Option<Expr>, span: &Option<SrcSpan>) {
        let method = self.current_method.borrow();
        if method.as_ref().is_none() {
            self.err(err_span!(
                *span,
                "Found return statement outside any methods."
            ));
        } else {
            let decl = self
                .symbols
                .find_method_decl(method.as_ref().unwrap().as_str());
            if let MethodOrImport::Method(m) = &decl.unwrap() {
                let return_tpe = match r {
                    None => Type::Void,
                    Some(e) => self.check_expr(e),
                };
                if return_tpe != m.tpe {
                    self.err(err_span!(
                        *span,
                        "Type {} returned, but method {} expects {}.",
                        return_tpe,
                        m.id.id,
                        m.tpe
                    ));
                }
            } else {
                self.err(err_span!(
                    *span,
                    "Cannot find declaration of method {}.",
                    method.as_ref().unwrap()
                ));
            }
        }
    }

    fn check_loop_block(&self, b: &mut Block) {
        let save_loop = *self.current_loop.borrow();
        *self.current_loop.borrow_mut() = Some(b.id);
        self.check_block(b);
        *self.current_loop.borrow_mut() = save_loop;
    }

    fn check_pred(&self, pred: &mut Expr) {
        let pred_tpe = self.check_expr(pred);
        if pred_tpe != Type::Bool {
            self.err(err_span!(
                pred.span,
                "Predicate of if statement is of type {pred_tpe}, but bool expected."
            ));
        }
    }

    fn check_method_call(&self, c: &mut MethodCall, span: &Option<SrcSpan>) -> Type {
        let method = self.symbols.find_method_decl(c.name.id.as_str());
        if method.is_none() {
            self.err(err_span!(
                *span,
                "Unable to find method declaration {}.",
                c.name.id
            ));
            return Type::Void;
        }
        if let Some(MethodOrImport::Method(decl)) = method {
            if decl.arguments.len() != c.arguments.len() {
                self.err(err_span!(
                    *span,
                    "{} arguments expected, but {} was given.",
                    decl.arguments.len(),
                    c.arguments.len()
                ));
            }
            for i in 0..decl.arguments.len() {
                let expr = c.arguments.get_mut(i).unwrap();
                let expected = &decl.arguments.get(i).unwrap().tpe;
                let tpe = self.check_expr(expr);
                if tpe != *expected {
                    self.err(err_span!(
                        expr.span,
                        "{expected} expected, but {tpe} given.",
                    ));
                }
            }
            return decl.tpe.clone();
        }
        Type::Void
    }

    fn check_loc_access(&self, loc: &mut Location) -> Type {
        let mut tpe = Type::Void;
        match loc {
            Location::Scalar(id) => {
                if let Some(d) = self.find_var_decl(id) {
                    tpe = d.tpe.clone();
                }
            }
            Location::Vector(id, expr) => {
                if let Some(d) = self.find_var_decl(id) {
                    match d.tpe.clone() {
                        Type::Array(ele_tpe, _) => tpe = *ele_tpe,
                        _ => {
                            self.err(err_span!(
                                id.span,
                                "Indexing non-array type variable {}",
                                id.id
                            ));
                        }
                    };
                    let tpe = self.check_expr(expr);
                    if tpe != Type::Int {
                        self.err(err_span!(
                            expr.span,
                            "Index of array access should be int, but found {tpe}."
                        ));
                    }
                }
            }
        }

        tpe
    }

    fn check_expr(&self, e: &mut Expr) -> Type {
        e.tpe = match &mut e.expr {
            Expr_::Location(l) => self.check_loc_access(l),
            Expr_::MethodCall(c) => self.check_method_call(c, &e.span),
            Expr_::Literal(l) => literal_type(l),
            Expr_::Len(id) => {
                if let Some(decl) = self.find_var_decl(id) {
                    match decl.tpe.clone() {
                        Type::Array(..) => Type::Int,
                        _ => {
                            self.err(err_span!(
                                e.span,
                                "Trying to get len of non-array {}",
                                id.id
                            ));
                            Type::Int
                        }
                    }
                } else {
                    Type::Int
                }
            }
            Expr_::BinOp(l, op, r) => self.check_bin_expr(*op, l, r),
            Expr_::NNeg(e) => {
                self.check_expr_tpe(e, &Type::Int);
                Type::Int
            }
            Expr_::LNeg(e) => {
                self.check_expr_tpe(e, &Type::Bool);
                Type::Bool
            }
            Expr_::TernaryOp(p, t, f) => {
                self.check_expr_tpe(p, &Type::Bool);
                let tpe_t = self.check_expr(&mut *t);
                let tpe_f = self.check_expr(&mut *f);
                if tpe_t != tpe_f {
                    self.err(err_span!(
                        e.span,
                        "Ternary expression has conflict types: {tpe_t} and {tpe_f}"
                    ))
                }
                tpe_t
            }
        };
        e.tpe.clone()
    }

    fn check_expr_tpe(&self, e: &mut Expr, t: &Type) {
        let tpe = self.check_expr(e);
        if tpe != *t {
            self.err(err_span!(
                e.span,
                "{t} expected, but expression evaluates to {tpe}"
            ));
        }
    }

    fn check_bin_expr(&self, op: BinOp, l: &mut Box<Expr>, r: &mut Box<Expr>) -> Type {
        let check = |l: &mut Box<Expr>, r: &mut Box<Expr>, t: &Type| {
            self.check_expr_tpe(&mut *l, t);
            self.check_expr_tpe(&mut *r, t);
        };
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                check(l, r, &Type::Int);
                Type::Int
            }
            BinOp::LT | BinOp::GT | BinOp::LE | BinOp::GE | BinOp::EQ | BinOp::NE => {
                check(l, r, &Type::Int);
                Type::Bool
            }
            BinOp::And | BinOp::Or => {
                check(l, r, &Type::Bool);
                Type::Bool
            }
        }
    }

    fn current_block(&self) -> usize {
        *self.current_block.borrow()
    }
    fn err(&self, err: Error) {
        self.errors.borrow_mut().push(err);
    }
    fn find_var_decl(&self, id: &ID) -> Option<&FieldDecl> {
        let arr_decl = self
            .symbols
            .find_var_decl(self.current_block(), id.id.as_str());
        if arr_decl.is_none() {
            self.err(err_span!(
                id.span,
                "Variable {} was not found in this scope.",
                id.id
            ));
            None
        } else {
            arr_decl
        }
    }
}
