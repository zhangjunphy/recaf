use crate::ast;
use crate::source_pos::SrcSpan;
use std::fmt;
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

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "v{}", self.id)
    }
}

impl Eq for Var {}

#[derive(Clone)]
pub enum Val {
    Var(Rc<Var>),
    Imm(ast::Literal),
}

impl Val {
    pub fn ty(&self) -> ast::Type {
        match self {
            Val::Var(v) => v.ty.clone(),
            Val::Imm(l) => l.ty(),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Var(v) => f.write_str(v.as_ref().to_string().as_str()),
            Val::Imm(l) => f.write_str(l.to_string().as_str()),
        }
    }
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

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.id.to_string().as_str())
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

impl fmt::Display for Branch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Branch::UnCon { label } => write!(f, "br {}", label),
            Branch::Con {
                pred,
                label_true,
                label_false,
            } => write!(f, "br {}, {}, {}", pred, label_true, label_false),
        }
    }
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

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Assign { dst, src } => write!(f, "{} = {}", dst, src),
            Statement::Call {
                dst,
                method,
                arguments,
            } => {
                match &dst {
                    Some(d) => write!(f, "{} = ", d)?,
                    _ => (),
                }
                let args = arguments
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}({})", method, args)
            }
            Statement::Return(Some(v)) => write!(f, "return {}", v),
            Statement::Return(None) => write!(f, "return"),
            Statement::Alloca {
                dst,
                ty,
                size: None,
            } => write!(f, "{} = alloca {}", dst, ty),
            Statement::Alloca {
                dst,
                ty,
                size: Some(s),
            } => write!(f, "{} = alloca {}x{}", dst, ty, s),
            Statement::Load { dst, ptr } => {
                write!(f, "{} = load {} {}", dst, ptr.ty(), ptr)
            }
            Statement::Store { ptr, src } => write!(f, "store {} {}, {}", ptr.ty(), src, ptr),
            Statement::Arith { dst, op, l, r } => write!(f, "{} = {} {} {}", dst, l, op, r),
            Statement::Cmp { dst, op, l, r } => write!(f, "{} = {} {} {}", dst, l, op, r),
            Statement::Cond { dst, op, l, r } => write!(f, "{} = {} {} {}", dst, l, op, r),
            Statement::NNeg { dst, val } => write!(f, "{} = -{}", dst, val),
            Statement::LNeg { dst, val } => write!(f, "{} = !{}", dst, val),
            Statement::Br(b) => write!(f, "{}", b),
        }
    }
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

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<id: {}>\n", self.label)?;
        for arg in &self.args {
            write!(f, "declare {};\n", arg.as_ref())?;
        }
        for stmt in &self.statements {
            write!(f, "{};\n", stmt)?;
        }
        Ok(())
    }
}

pub struct Function {
    pub name: String,
    pub args: Vec<Rc<Var>>,
    pub ty: ast::Type,
    pub body: Vec<BasicBlock>,
}
