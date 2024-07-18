use crate::source_pos::SrcSpan;
use std::fmt;
use std::io;
use std::ops::Fn;

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

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(v) => write!(f, "{}", v),
            Literal::Char(v) => write!(f, "{}", v),
            Literal::Bool(v) => write!(f, "{}", v),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mod => write!(f, "%"),
            BinOp::LT => write!(f, "<"),
            BinOp::GT => write!(f, ">"),
            BinOp::LE => write!(f, "<="),
            BinOp::GE => write!(f, ">="),
            BinOp::EQ => write!(f, "=="),
            BinOp::NE => write!(f, "!="),
            BinOp::And => write!(f, "&&"),
            BinOp::Or => write!(f, "||"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Ptr(tpe) => write!(f, "ptr {}", *tpe),
            Type::Array(tpe, size) => write!(f, "{}x{}", size, *tpe),
        }
    }
}

impl fmt::Display for ImportDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "import {};", self.id.id.as_str())
    }
}

impl fmt::Display for FieldDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {};", self.tpe, self.id.id.as_str())
    }
}

impl fmt::Display for ID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Scalar(id) => write!(f, "{}", id),
            Self::Vector(id, expr) => write!(f, "{}[{}]", id, *expr)
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Scalar(id) => write!(f, "{}", id),
            Self::Vector(id, expr) => write!(f, "{}[{}]", id, *expr)
        }
    }
}

pub struct ASTPrinter<'buf, T>
where
    T: io::Write,
{
    indent: usize,
    buf: &'buf mut T,
    depth: usize,
}

impl<'buf, T> ASTPrinter<'buf, T>
where
    T: io::Write,
{
    pub fn new(buf: &'buf mut T, indent: usize) -> Self {
        ASTPrinter {
            indent,
            buf,
            depth: 0,
        }
    }
    pub fn print(&mut self, p: &Program) -> io::Result<usize> {
        let mut size = 0;
        for imp in &p.imports {
            size += self.indented_write(format!("{}", imp).as_str())?;
        }
        for fld in &p.fields {
            size += self.indented_write(format!("{}", fld).as_str())?;
        }
        for method in &p.methods {
            size += self.print_method(method)?;
        }
        Ok(size)
    }
    fn print_method(&mut self, m: &MethodDecl) -> io::Result<usize> {
        let mut size = 0;
        let args_str: Vec<String> = m
            .arguments
            .clone()
            .into_iter()
            .map(|arg| format!("{}", arg))
            .collect();
        size += self.indented_write(
            format!("{} {} ({}) {{", m.tpe, m.id.id, args_str.join(", ")).as_str(),
        )?;
        Ok(size)
    }
    fn print_block(&mut self, b: &Block) -> io::Result<usize> {
        self.indent();
        let mut size = 0;
        for fld in &b.fields {
            size += self.indented_write(format!("{}", fld).as_str())?;
        }
        for stmt in &b.statements {
            size += self.print_stmt(stmt)?;
        }
        self.unindent();
        Ok(size)
    }
    fn print_stmt(&mut self, s: &Statement) -> io::Result<usize> {
        match s.statement {
            Statement_::Assign(_) => _,
        }
    }
    fn indented_write(&mut self, s: &str) -> io::Result<usize> {
        let mut size = 0;
        size += self
            .buf
            .write(" ".repeat(self.depth * self.indent).as_bytes())?;
        size += self.buf.write(s.as_bytes())?;
        size += self.buf.write("\n".as_bytes())?;
        Ok(size)
    }

    fn indent(&mut self) {
        self.depth += 1;
    }
    fn unindent(&mut self) {
        self.depth -= 1
    }
}
