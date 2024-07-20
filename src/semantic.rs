use crate::ast::*;
use crate::consts;
use crate::error::Error;
use crate::{err, err_span};
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
struct SymbolTable<'p> {
    block_id: usize, // Block this table belongs to.
    parent_id: Option<usize>,
    variables: HashMap<String, &'p FieldDecl>,
}

impl<'p> SymbolTable<'p> {
    fn new(block_id: usize, parent_id: Option<usize>) -> Self {
        SymbolTable {
            block_id,
            parent_id,
            variables: HashMap::new(),
        }
    }
}

#[derive(Clone)]
pub struct ProgramTables<'p> {
    imports: HashSet<String>,
    methods: HashMap<String, &'p MethodDecl>,
    variables: HashMap<usize, SymbolTable<'p>>,
}

impl<'p> ProgramTables<'p> {
    fn new() -> Self {
        ProgramTables {
            imports: HashSet::new(),
            methods: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    fn add_table(&mut self, block_id: usize, parent_id: Option<usize>) -> Result<(), Error> {
        let old = self
            .variables
            .insert(block_id, SymbolTable::new(block_id, parent_id));
        if old.is_some() {
            return err!("Symbol table present for block {block_id}",);
        }
        Ok(())
    }

    fn add_table_if_nonexist(&mut self, block_id: usize, parent_id: Option<usize>) {
        if !self.variables.contains_key(&block_id) {
            self.variables
                .insert(block_id, SymbolTable::new(block_id, parent_id));
        }
    }

    fn find_parent(&self, block: usize) -> Option<&SymbolTable> {
        let parent = self.variables.get(&block).and_then(|t| t.parent_id)?;
        self.variables.get(&parent)
    }

    fn find_mut_parent(&mut self, block: usize) -> Option<&'p mut SymbolTable> {
        let parent = self.variables.get(&block).and_then(|t| t.parent_id)?;
        self.variables.get_mut(&parent)
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

pub struct SemanticChecker<'p> {
    program: &'p Program,
    tables: ProgramTables<'p>,
}

impl<'p> SemanticChecker<'p> {
    pub fn new(program: &'p Program) -> Self {
        SemanticChecker {
            program,
            tables: ProgramTables::new(),
        }
    }

    pub fn process(&'p mut self) -> Result<(), Error> {
        self.process_program()
    }

    pub fn get_symbol_table(&'p self) -> &ProgramTables {
        &self.tables
    }

    fn add_root(&mut self) -> Result<(), Error> {
        let old = self.tables.variables.insert(
            consts::ROOT_BLOCK_ID,
            SymbolTable::new(consts::ROOT_BLOCK_ID, None),
        );
        if old.is_some() {
            return err!("Symbol table present for program root");
        }
        Ok(())
    }

    fn process_program(&'p mut self) -> Result<(), Error> {
        self.add_root()?;
        for imp in &self.program.imports {
            self.tables.add_import(imp)?;
        }
        for var in &self.program.fields {
            self.tables.add_variable(consts::ROOT_BLOCK_ID, var)?;
        }
        for method in &self.program.methods {
            self.tables.add_method(method)?;
            self.process_method(method)?;
        }
        Ok(())
    }

    fn process_method(&mut self, m: &'p MethodDecl) -> Result<(), Error> {
        let method_block = m.block.id;
        self.tables
            .add_table(m.block.id, Some(consts::ROOT_BLOCK_ID))?;
        for v in &m.arguments {
            self.tables.add_variable(method_block, v)?;
        }
        self.process_block(&m.block, Some(consts::ROOT_BLOCK_ID))?;
        Ok(())
    }

    fn process_block(&mut self, b: &'p Block, parent: Option<usize>) -> Result<(), Error> {
        self.tables.add_table_if_nonexist(b.id, parent);
        for v in &b.fields {
            self.tables.add_variable(b.id, v)?;
        }
        for s in &b.statements {
            self.process_stmt(s, b.id)?;
        }
        Ok(())
    }

    fn process_stmt(&mut self, s: &'p Statement, block_id: usize) -> Result<(), Error> {
        let s_ = &s.statement;
        match s_ {
            Statement_::If(i) => {
                self.process_block(&i.if_block, Some(block_id))?;
                if let Some(else_block) = &i.else_block {
                    self.process_block(else_block, Some(block_id))?;
                }
            },
            Statement_::While(w) => {
                self.process_block(&w.block, Some(block_id))?;
            },
            _ => (),
        }
        Ok(())
    }
}
