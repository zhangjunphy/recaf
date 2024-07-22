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
            return err!("Symbol table present for block {block_id}",);
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
            err_span!(imp.span, "Imported method {} is a duplicate.", imp.id.id)
        } else {
            Ok(())
        }
    }

    fn add_method(&mut self, m: &'p MethodDecl) -> Result<(), Error> {
        let is_new = self.methods.insert(m.id.id.clone(), m);
        if is_new.is_some() {
            err_span!(m.span, "Duplicate method declaration for {}.", m.id.id)
        } else {
            Ok(())
        }
    }

    fn add_variable(&mut self, block: usize, var: &'p FieldDecl) -> Result<(), Error> {
        if let Some(table) = self.variables.get_mut(&block) {
            if let Some(old) = table.variables.insert(var.id.id.clone(), var) {
                if let Some(span) = old.span {
                    err_span!(
                        var.span,
                        "Dupilcate declaration for {}, previous at {}",
                        var.id.id,
                        span
                    )
                } else {
                    err_span!(
                        var.span,
                        "Dupilcate declaration for {}, previous declation unlocatable",
                        var.id.id
                    )
                }
            } else {
                Ok(())
            }
        } else {
            err_span!(var.span, "Symbol table for block {block} does not exist!")
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
            return err!("Symbol table present for program root");
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

    pub fn check(&self, program: &'p mut Program) {
        self.check_main();
        for m in &mut program.methods {
            self.check_method_decl(m);
        }
    }

    fn current_block(&self) -> usize {
        *self.current_block.borrow()
    }
    fn err(&self, msg: &str) {
        self.errors.borrow_mut().push(Error::new(None, msg));
    }
    fn err_span(&self, span: &Option<SrcSpan>, msg: &str) {
        self.errors.borrow_mut().push(Error::new(*span, msg));
    }

    fn check_main(&self) {
        let fmain = self.symbols.methods.get(consts::MAIN_FUNC_NAME);
        match fmain {
            None => self.err("Method \"main\" does not exist."),
            Some(f) => {
                if f.tpe != Type::Void {
                    self.err("Method \"main\" does not return void.");
                } else if f.arguments.is_empty() {
                    self.err("Method \"main\" should not have any arguments.");
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
                self.check_loc_access(&mut assign.location);
                self.check_expr(&mut assign.expr);
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
            _ => (),
        }
    }

    fn check_return(&self, r: &mut Option<Expr>, span: &Option<SrcSpan>) {
        let method = self.current_method.borrow();
        if method.as_ref().is_none() {
            self.err_span(span, "Found return statement outside any methods.")
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
                    self.err_span(
                        span,
                        format!(
                            "Type {} returned, but method {} expects {}.",
                            return_tpe, m.id.id, m.tpe
                        )
                        .as_str(),
                    );
                }
            } else {
                self.err_span(
                    span,
                    format!(
                        "Cannot find proper declaration of method {}.",
                        method.as_ref().unwrap()
                    )
                    .as_str(),
                );
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
            self.err_span(
                &pred.span,
                format!(
                    "Predicate of if statement is of type {}, but bool expected.",
                    pred_tpe
                )
                .as_str(),
            );
        }
    }

    fn check_method_call(&self, c: &mut MethodCall, span: &Option<SrcSpan>) -> Type {
        let method = self.symbols.find_method_decl(c.name.id.as_str());
        if method.is_none() {
            self.err_span(
                span,
                format!("Unable to find method declaration {}.", c.name.id).as_str(),
            );
            return Type::Void;
        }
        if let Some(MethodOrImport::Method(decl)) = method {
            if decl.arguments.len() != c.arguments.len() {
                self.err_span(
                    span,
                    format!(
                        "{} arguments expected, but {} was given.",
                        decl.arguments.len(),
                        c.arguments.len()
                    )
                    .as_str(),
                );
            }
            for i in 0..decl.arguments.len() {
                let expr = c.arguments.get_mut(i).unwrap();
                let expected = &decl.arguments.get(i).unwrap().tpe;
                let tpe = self.check_expr(expr);
                if tpe != *expected {
                    self.err_span(
                        &expr.span,
                        format!("{} expected, but {} given.", expected, tpe).as_str(),
                    );
                }
            }
            return decl.tpe.clone();
        }
        Type::Void
    }

    fn check_loc_access(&self, loc: &mut Location) {
        let block = self.current_block();
        match loc {
            Location::Scalar(id) => {
                if self.symbols.find_var_decl(block, id.id.as_str()).is_none() {
                    self.err_span(
                        &id.span,
                        format!("Variable {} was not declared in this scope", id.id).as_str(),
                    );
                }
            }
            Location::Vector(id, expr) => {
                let decl = self.symbols.find_var_decl(block, id.id.as_str());
                if decl.is_none() {
                    self.err_span(
                        &id.span,
                        format!("Variable {} was not declared in this scope", id.id).as_str(),
                    );
                }
                match decl.unwrap().tpe {
                    Type::Array(_, _) => (),
                    _ => self.err_span(
                        &id.span,
                        format!("Indexing non-array type variable {}", id.id).as_str(),
                    ),
                };
                let tpe = self.check_expr(expr);
                if tpe != Type::Int {
                    self.err_span(
                        &expr.span,
                        format!("Index of array access should be int, but found {}.", tpe).as_str(),
                    );
                }
            }
        }
    }

    fn check_expr(&self, e: &mut Expr) -> Type {
        Type::Void
    }
}
