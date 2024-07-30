use crate::ast;
use crate::source_pos::SrcSpan;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Locality {
    Global,
    Local,
}

#[derive(Clone)]
pub struct Var {
    pub id: usize,
    pub tpe: ast::Type,
    pub decl: Option<ast::FieldDecl>,
    pub span: Option<SrcSpan>,
    pub locality: Locality,
}

pub enum Val {
    Var(Rc<Var>),
    Imm(ast::Literal),
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum ArithOp {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum RelOp {
    LT,
    GT,
    LE,
    GE,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum CondOp {
    And,
    Or,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum EqOp {
    EQ,
    NE,
}

pub struct Label {
    pub id: usize,
}

impl Label {
    pub fn new(id: usize) -> Self {
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
        tpe: ast::Type,
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
        op: ArithOp,
        l: Val,
        r: Val,
    },
    Rel {
        dst: Rc<Var>,
        op: RelOp,
        l: Val,
        r: Val,
    },
    Cond {
        dst: Rc<Var>,
        op: CondOp,
        l: Val,
        r: Val,
    },
    Eq {
        dst: Rc<Var>,
        op: EqOp,
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
    pub args: Vec<Rc<Var>>,
    pub statements: Vec<Statement>,
    pub br: Option<Branch>,
    pub ast_scope: ast::Scope,
}

impl BasicBlock {
    pub fn push_stmt(&mut self, stmt: Statement) {
        self.statements.push(stmt)
    }
}

pub struct Function {
    pub name: String,
    pub args: Vec<Rc<Var>>,
    pub tpe: ast::Type,
    pub body: Vec<BasicBlock>,
}
