use crate::ast::*;
use crate::consts;
use crate::error::Error;
use crate::{err, err_span};
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
    program: &'p Program,
    symbols: ProgramSymbols<'p>,
}

impl<'p> SymbolTableBuilder<'p> {
    pub fn new(program: &'p Program) -> SymbolTableBuilder<'p> {
        SymbolTableBuilder {
            program,
            symbols: ProgramSymbols::new(),
        }
    }

    pub fn process(&mut self) -> Result<&ProgramSymbols, Error> {
        self.process_program()?;
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

    fn process_program(&mut self) -> Result<(), Error> {
        self.add_root()?;
        for imp in &self.program.imports {
            self.symbols.add_import(imp)?;
        }
        for var in &self.program.fields {
            self.symbols.add_variable(consts::ROOT_BLOCK_ID, var)?;
        }
        for method in &self.program.methods {
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

pub struct SemanticChecker<'p> {
    program: &'p Program,
    symbols: &'p ProgramSymbols<'p>,
    inside_loop: Option<usize>,
}

impl<'p> SemanticChecker<'p> {
    pub fn new(program: &'p Program, symbols: &'p ProgramSymbols<'p>) -> SemanticChecker<'p> {
        SemanticChecker {
            program,
            symbols,
            inside_loop: None,
        }
    }

    pub fn check(&self) -> Result<(), Error> {
        self.check_main()?;
        for m in &self.program.methods {
            self.check_method(m)?;
        }
        Ok(())
    }

    pub fn check_main(&self) -> Result<(), Error> {
        let fmain = self.symbols.methods.get(consts::MAIN_FUNC_NAME);
        match fmain {
            None => return err!("Method \"main\" does not exist."),
            Some(f) => {
                if f.tpe != Type::Void {
                    return err!("Method \"main\" does not return void.");
                } else if f.arguments.is_empty() {
                    return err!("Method \"main\" should not have any arguments.");
                } else {
                    return Ok(());
                }
            }
        }
    }
    pub fn check_method(&self, m: &MethodDecl) -> Result<(), Error> {
        for s in &m.block.statements {
            self.check_statement(s)?;
        }
        Ok(())
    }

    pub fn check_statement(&self, s: &Statement) -> Result<(), Error> {
        Ok(())
    }

    pub fn check_expr(&self, s: &Statement) -> Result<(), Error> {
        Ok(())
    }
}
