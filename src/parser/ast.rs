use crate::source_pos::SrcSpan;
use std::fmt;

// AST Root
pub struct Program {
    pub imports: Vec<ImportDecl>,
    pub fields: Vec<FieldDecl>,
    pub methods: Vec<MethodDecl>,
}

pub struct ImportDecl {
    pub id: ID,
    pub range: Option<SrcSpan>,
}

pub struct FieldDecl {
    pub typ: Type,
    pub field: Field,
    pub range: Option<SrcSpan>,
}

pub struct MethodDecl {
    pub typ: ReturnType,
    pub arguments: (Type, ID),
    pub block: Block,
    pub range: Option<SrcSpan>,
}

pub struct Block {
    pub fields: Vec<FieldDecl>,
    pub statements: Vec<Statement>,
    pub range: Option<SrcSpan>,
}

pub struct Statement {
    pub statement: StatementBare,
    pub range: Option<SrcSpan>,
}

pub struct Expr {
    pub expr: ExprBare,
    pub range: Option<SrcSpan>,
}

pub struct ID {
    pub id: String,
    pub range: Option<SrcSpan>,
}

pub struct Assign {
    pub location: Location,
    pub expr: Expr,
}

pub struct If {
    pub pred: Expr,
    pub if_block: Block,
    pub else_block: Option<Block>,
}

pub enum Argument {
    Expr(Expr),
    StringLiteral(String),
}

pub struct MethodCall {
    pub name: ID,
    pub arguments: Vec<Argument>,
}

pub struct For {
    pub id: ID,
    pub init: Expr,
    pub pred: Expr,
    pub update: Assign,
    pub block: Block,
}

pub struct While {
    pub pred: Expr,
    pub block: Block,
}

pub enum StatementBare {
    Assign(Assign),
    MethodCall(MethodCall),
    If(If),
    For(For),
    While(While),
    Return(Option<Expr>),
    Break,
    Continue,
}

pub enum Literal {
    Int(i64),
    Char(u8),
    Bool(bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(v) => write!(f, "{}", v),
            Literal::Char(v) => write!(f, "{}", v),
            Literal::Bool(v) => write!(f, "{}", v)
        }
    }
}

pub enum ExprBare {
    Location(Location),
    MethodCall(MethodCall),
    Literal(Literal),
    Len(ID),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    Neg(Box<Expr>),
    Not(Box<Expr>),
    TernaryOp(Box<Expr>, Box<Expr>, Box<Expr>),
}

pub enum Location {
    Scalar(ID),
    Vector(ID, Box<Expr>),
}

pub enum Type {
    Int,
    Bool,
}

pub enum Field {
    Scalar(ID),
    Vector(ID, usize),
}

pub enum ReturnType {
    Void,
    Return(Type),
}

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

#[macro_export]
macro_rules! expr {
    ($ii:ident, $s:expr, $e:expr $(, $x:ident )*) => {
        Expr {expr: ExprBare::$ii ( $( $x ),* ), range: SrcSpan {start: $s, end: $e} }
    }
}
