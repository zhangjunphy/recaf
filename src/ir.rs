use crate::ast;
use crate::source_pos::SrcSpan;

pub enum Locality {
    Global,
    Local,
}

pub struct Var {
    pub id: usize,
    pub tpe: ast::Type,
    pub decl: ast::FieldDecl,
    pub span: Option<SrcSpan>,
    pub locality: Locality,
}

pub enum Val {
    Var(Var),
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
    pub id: String,
}

pub enum IR {
    Assign {
        dst: Var,
        src: Val,
    },
    Call {
        dst: Var,
        method: String,
        arguments: Vec<Val>,
    },
    Return(Option<Val>),
    Alloca {
        dst: Var,
        tpe: ast::Type,
        size: Option<usize>,
    },
    Load {
        dst: Var,
        ptr: Val,
    },
    Store {
        ptr: Val,
        src: Val,
    },
    Arith {
        dst: Var,
        op: ArithOp,
        l: Val,
        r: Val,
    },
    Rel {
        dst: Var,
        op: RelOp,
        l: Val,
        r: Val,
    },
    Cond {
        dst: Var,
        op: CondOp,
        l: Val,
        r: Val,
    },
    Eq {
        dst: Var,
        op: EqOp,
        l: Val,
        r: Val,
    },
    NNeg {
        dst: Var,
        v: Val,
    },
    LNeg {
        dst: Var,
        v: Val,
    },
    BrUncon {
        l: Label,
    },
    BrCon {
        pred: Val,
        label_true: Label,
        label_false: Label,
    },
}
