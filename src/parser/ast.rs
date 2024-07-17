use crate::source_pos::SrcSpan;
use std::fmt;

// AST Root
#[derive(Debug)]
pub struct Program {
    pub imports: Vec<ImportDecl>,
    pub fields: Vec<FieldDecl>,
    pub methods: Vec<MethodDecl>,
    pub span: Option<SrcSpan>,
}

#[derive(Debug)]
pub struct ImportDecl {
    pub id: ID,
    pub span: Option<SrcSpan>,
}

#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub id: ID,
    pub tpe: Type,
    pub span: Option<SrcSpan>,
}

#[derive(Debug)]
pub struct MethodDecl {
    pub id: ID,
    pub tpe: Type,
    pub arguments: Vec<FieldDecl>,
    pub block: Block,
    pub span: Option<SrcSpan>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub fields: Vec<FieldDecl>,
    pub statements: Vec<Statement>,
    pub span: Option<SrcSpan>,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub statement: Statement_,
    pub span: Option<SrcSpan>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Expr {
    pub expr: Expr_,
    pub span: Option<SrcSpan>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ID {
    pub id: String,
    pub span: Option<SrcSpan>,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub location: Location,
    pub expr: AssignExpr,
}

#[derive(Debug, Clone)]
pub enum AssignExpr {
    Assign(Expr),
    AssignAdd(Expr),
    AssignSub(Expr),
    Inc,
    Dec,
}

#[derive(Debug, Clone)]
pub struct If {
    pub pred: Expr,
    pub if_block: Block,
    pub else_block: Option<Block>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Argument {
    Expr(Expr),
    StringLiteral(String),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct MethodCall {
    pub name: ID,
    pub arguments: Vec<Argument>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub init: Box<Statement>,
    pub pred: Expr,
    pub update: Box<Statement>,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct While {
    pub pred: Expr,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub enum Statement_ {
    Assign(Assign),
    MethodCall(MethodCall),
    If(If),
    For(For),
    While(While),
    Return(Option<Expr>),
    Break,
    Continue,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Int(i64),
    Char(char),
    Bool(bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(v) => write!(f, "{}", v),
            Literal::Char(v) => write!(f, "{}", v),
            Literal::Bool(v) => write!(f, "{}", v),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expr_ {
    Location(Location),
    MethodCall(MethodCall),
    Literal(Literal),
    Len(ID),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    NNeg(Box<Expr>), // Numerical negation
    LNeg(Box<Expr>), // Logical negation
    TernaryOp(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Location {
    Scalar(ID),
    Vector(ID, Box<Expr>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Void,
    Int,
    Bool,
    Char,
    Ptr(Box<Type>),
    Array(Box<Type>, usize),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Field {
    Scalar(ID),
    Vector(ID, usize),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum BinOp {
    Mul,
    Div,
    Add,
    Sub,
    Mod,

    LT,
    GT,
    LE,
    GE,
    EQ,
    NE,

    And,
    Or,
}
