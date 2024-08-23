use crate::source_pos::SrcSpan;
use std::fmt;
use std::io;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope {
    pub id: u64,
}

impl Scope {
    pub fn new(id: u64) -> Scope {
        Scope { id }
    }
}

impl From<u64> for Scope {
    fn from(id: u64) -> Self {
        Scope { id }
    }
}

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
    pub ty: Type,
    pub span: Option<SrcSpan>,
}

#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub id: ID,
    pub ty: Type,
    pub arguments: Vec<FieldDecl>,
    pub block: Block,
    pub span: Option<SrcSpan>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub scope: Scope,
    pub fields: Vec<FieldDecl>,
    pub statements: Vec<Statement>,
    pub span: Option<SrcSpan>,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub stmt: Stmt_,
    pub span: Option<SrcSpan>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Expr {
    pub expr: Expr_,
    pub ty: Type,
    pub span: Option<SrcSpan>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ID {
    pub str: String,
    pub span: Option<SrcSpan>,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub location: Location,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct If {
    pub pred: Expr,
    pub if_block: Block,
    pub else_block: Option<Block>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct MethodCall {
    pub name: ID,
    pub arguments: Vec<Expr>,
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
pub enum Stmt_ {
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
    String(String),
}

impl Literal {
    pub fn ty(&self) -> Type {
        match self {
            Literal::Int(_) => Type::Int,
            Literal::Char(_) => Type::Char,
            Literal::Bool(_) => Type::Bool,
            Literal::String(s) => str_type(s),
        }
    }

    pub fn str_lit(&self) -> Option<String> {
        match &self {
            Literal::String(s) => Some(s.clone()),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expr_ {
    Location(Location),
    MethodCall(MethodCall),
    Literal(Literal),
    Len(ID),
    Arith(Box<Expr>, ArithOp, Box<Expr>),
    Cmp(Box<Expr>, CmpOp, Box<Expr>),
    Cond(Box<Expr>, CondOp, Box<Expr>),
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
    Array(Box<Type>, u64),
}

impl Type {
    pub fn size(&self) -> u64 {
        match self {
            Type::Void => 0,
            Type::Int => 8,
            Type::Bool => 1,
            Type::Char => 1,
            Type::Ptr(_) => 8,
            Type::Array(ty, n) => ty.size() * n,
        }
    }

    pub fn array_len(&self) -> u64 {
        match self {
            Type::Array(_, n) => *n,
            _ => panic!(),
        }
    }

    pub fn ptr_ele_ty(&self) -> Option<&Type> {
        match self {
            Type::Ptr(t) => Some(t.as_ref()),
            _ => None,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Type::Array(e, _) => match e.as_ref() {
                Type::Char => true,
                _ => false,
            },
            _ => false,
        }
    }
}

pub fn str_type(s: &String) -> Type {
    Type::Array(Box::new(Type::Char), s.len() as u64)
}

#[derive(PartialEq, Eq, Debug)]
pub enum Field {
    Scalar(ID),
    Vector(ID, usize),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum ArithOp {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum CmpOp {
    LT,
    GT,
    LE,
    GE,

    EQ,
    NE,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum CondOp {
    And,
    Or,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::parser::util::escape_string_literal;
        match self {
            Literal::Int(v) => write!(f, "{}", v),
            Literal::Char(v) => write!(f, "\'{}\'", v),
            Literal::Bool(v) => write!(f, "{}", v),
            Literal::String(v) => write!(f, "\"{}\"", escape_string_literal(v)),
        }
    }
}

impl fmt::Display for ArithOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArithOp::Mul => write!(f, "*"),
            ArithOp::Div => write!(f, "/"),
            ArithOp::Add => write!(f, "+"),
            ArithOp::Sub => write!(f, "-"),
            ArithOp::Mod => write!(f, "%"),
        }
    }
}

impl fmt::Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CmpOp::LT => write!(f, "<"),
            CmpOp::GT => write!(f, ">"),
            CmpOp::LE => write!(f, "<="),
            CmpOp::GE => write!(f, ">="),
            CmpOp::EQ => write!(f, "=="),
            CmpOp::NE => write!(f, "!="),
        }
    }
}

impl fmt::Display for CondOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CondOp::And => write!(f, "&&"),
            CondOp::Or => write!(f, "||"),
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
            Type::Ptr(ty) => write!(f, "ptr {}", *ty),
            Type::Array(ty, size) => write!(f, "{}x{}", size, *ty),
        }
    }
}

impl fmt::Display for ImportDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "import {};", self.id.str.as_str())
    }
}

impl fmt::Display for FieldDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {};", self.ty, self.id.str.as_str())
    }
}

impl fmt::Display for ID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.str)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Scalar(id) => write!(f, "{}", id),
            Self::Vector(id, expr) => write!(f, "{}[{}]", id, *expr),
        }
    }
}

impl fmt::Display for MethodCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let args_str: Vec<String> = self
            .arguments
            .clone()
            .into_iter()
            .map(|arg| format!("{}", arg))
            .collect();
        write!(f, "{}({})", self.name, args_str.join(", "))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.expr {
            Expr_::Location(l) => write!(f, "{}", l),
            Expr_::MethodCall(c) => write!(f, "{}", c),
            Expr_::Literal(l) => write!(f, "{}", l),
            Expr_::Len(id) => write!(f, "len({})", id),
            Expr_::Arith(l, op, r) => write!(f, "{} {} {}", *l, op, *r),
            Expr_::Cmp(l, op, r) => write!(f, "{} {} {}", *l, op, *r),
            Expr_::Cond(l, op, r) => write!(f, "{} {} {}", *l, op, *r),
            Expr_::NNeg(e) => write!(f, "-{}", *e),
            Expr_::LNeg(e) => write!(f, "!{}", *e),
            Expr_::TernaryOp(p, e1, e2) => write!(f, "{} ? {} : {}", *p, *e1, *e2),
        }
    }
}

pub mod visit {
    use super::*;

    pub trait Visitor<T> {
        fn visit_program(&mut self, p: &Program) -> T;
        fn visit_import(&mut self, i: &ImportDecl) -> T;
        fn visit_field(&mut self, f: &FieldDecl) -> T;
        fn visit_method(&mut self, m: &MethodDecl) -> T;
        fn visit_block(&mut self, b: &Block) -> T;
        fn visit_stmt(&mut self, s: &Statement) -> T;
        fn visit_expr(&mut self, e: &Expr) -> T;
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

impl<'buf, T> visit::Visitor<io::Result<usize>> for ASTPrinter<'buf, T>
where
    T: io::Write,
{
    fn visit_program(&mut self, p: &Program) -> io::Result<usize> {
        let mut size = 0;
        for imp in &p.imports {
            size += self.visit_import(imp)?;
        }
        for fld in &p.fields {
            size += self.visit_field(fld)?;
        }
        for method in &p.methods {
            size += self.visit_method(method)?;
        }
        Ok(size)
    }

    fn visit_import(&mut self, i: &ImportDecl) -> io::Result<usize> {
        self.indented_write(format!("{}", i).as_str())
    }

    fn visit_field(&mut self, f: &FieldDecl) -> io::Result<usize> {
        self.indented_write(format!("{}", f).as_str())
    }

    fn visit_method(&mut self, m: &MethodDecl) -> io::Result<usize> {
        let mut size = 0;
        let args_str: Vec<String> = m
            .arguments
            .clone()
            .into_iter()
            .map(|arg| format!("{}", arg))
            .collect();
        size += self.indented_write(
            format!("{} {} ({}) {{", m.ty, m.id.str, args_str.join(", ")).as_str(),
        )?;
        size += self.visit_block(&m.block)?;
        size += self.indented_write("}")?;
        Ok(size)
    }

    fn visit_block(&mut self, b: &Block) -> io::Result<usize> {
        self.indent();
        let mut size = 0;
        size += self.indented_write(format!("[block_id: {}]", b.scope.id).as_str())?;
        for fld in &b.fields {
            size += self.indented_write(format!("{}", fld).as_str())?;
        }
        for stmt in &b.statements {
            size += self.visit_stmt(stmt)?;
        }
        self.unindent();
        Ok(size)
    }

    fn visit_stmt(&mut self, s: &Statement) -> io::Result<usize> {
        let mut size = 0;
        match &s.stmt {
            Stmt_::Assign(l) => {
                size += self.indented_write(format!("{} = {};", l.location, l.expr).as_str())?
            }
            Stmt_::MethodCall(c) => size += self.indented_write(format!("{c};").as_str())?,
            Stmt_::If(i) => {
                size += self.indented_write(format!("if ({}) {{", i.pred).as_str())?;
                size += self.visit_block(&i.if_block)?;
                match &i.else_block {
                    None => size += self.indented_write("}")?,
                    Some(b) => {
                        size += self.indented_write("} else {")?;
                        size += self.visit_block(&b)?;
                    }
                };
                size += self.indented_write("}")?;
            }
            Stmt_::For(f) => {
                size += self.indented_write(
                    format!(
                        "for ({}; {}; {}) {{",
                        Self::format_single_line_stmt(&f.init),
                        f.pred,
                        Self::format_single_line_stmt(&f.update),
                    )
                    .as_str(),
                )?;
                size += self.visit_block(&f.block)?;
                size += self.indented_write("}")?;
            }
            Stmt_::While(w) => {
                size += self.indented_write(format!("while ({}) {{", w.pred).as_str())?;
                size += self.visit_block(&w.block)?;
                size += self.indented_write("}")?;
            }
            Stmt_::Return(e) => match e {
                None => size += self.indented_write("return;")?,
                Some(e) => size += self.indented_write(format!("return {};", e).as_str())?,
            },
            Stmt_::Break => size += self.indented_write("break")?,
            Stmt_::Continue => size += self.indented_write("continue")?,
        }
        Ok(size)
    }

    fn visit_expr(&mut self, e: &Expr) -> io::Result<usize> {
        self.buf.write(format!("{}", e).as_bytes())
    }
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
        use visit::Visitor;
        self.visit_program(p)
    }

    fn format_single_line_stmt(s: &Statement) -> String {
        match &s.stmt {
            Stmt_::Assign(l) => format!("{} = {}", l.location, l.expr),
            Stmt_::MethodCall(c) => format!("{c}"),
            _ => panic!("Not single line statement!"),
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
