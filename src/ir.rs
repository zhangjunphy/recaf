use crate::ast;
use crate::source_pos::SrcSpan;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Locality {
    Global,
    Local,
}

#[derive(Clone)]
pub struct Var {
    pub id: usize,
    pub ty: ast::Type,
    pub decl: Option<ast::FieldDecl>,
    pub span: Option<SrcSpan>,
    pub locality: Locality,
}

impl Hash for Var {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.id.hash(state);
    }
}

impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Var {}

#[derive(Clone)]
pub enum Val {
    Var(Rc<Var>),
    Imm(ast::Literal),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct Label {
    pub id: usize,
}

impl Label {
    pub fn new(id: usize) -> Self {
        Label { id }
    }
}

impl From<usize> for Label {
    fn from(id: usize) -> Self {
        Label { id }
    }
}

pub enum Branch {
    UnCon {
        label: Label,
    },
    Con {
        pred: Val,
        label_true: Label,
        label_false: Label,
    },
}

pub enum Statement {
    Assign {
        dst: Rc<Var>,
        src: Val,
    },
    Call {
        dst: Option<Rc<Var>>,
        method: String,
        arguments: Vec<Val>,
    },
    Return(Option<Val>),
    Alloca {
        dst: Rc<Var>,
        ty: ast::Type,
        size: Option<usize>,
    },
    Load {
        dst: Rc<Var>,
        ptr: Val,
    },
    Store {
        ptr: Val,
        src: Val,
    },
    Arith {
        dst: Rc<Var>,
        op: ast::ArithOp,
        l: Val,
        r: Val,
    },
    Cmp {
        dst: Rc<Var>,
        op: ast::CmpOp,
        l: Val,
        r: Val,
    },
    Cond {
        dst: Rc<Var>,
        op: ast::CondOp,
        l: Val,
        r: Val,
    },
    NNeg {
        dst: Rc<Var>,
        val: Val,
    },
    LNeg {
        dst: Rc<Var>,
        val: Val,
    },
    Br(Branch),
}

pub struct BasicBlock {
    pub label: Label,
    pub ast_scope: ast::Scope,
    pub args: Vec<Rc<Var>>,
    pub statements: Vec<Statement>,
}

impl BasicBlock {
    pub fn push_stmt(&mut self, stmt: Statement) {
        self.statements.push(stmt)
    }
}

pub struct Function {
    pub name: String,
    pub args: Vec<Rc<Var>>,
    pub ty: ast::Type,
    pub body: Vec<BasicBlock>,
}
