use crate::ast;
use crate::source_pos::SrcSpan;
use std::cell::Cell;
use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Locality {
    Global,
    Local,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum VarDomain {
    Normal,
    StrLit,
}

impl fmt::Display for VarDomain {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VarDomain::Normal => write!(f, ""),
            VarDomain::StrLit => write!(f, "str"),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum VarID {
    Name(String),
    Num(u64),
}

impl fmt::Display for VarID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VarID::Name(n) => write!(f, "{}", n),
            VarID::Num(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub domain: VarDomain,
    pub id: VarID,
    pub ty: ast::Type,
    pub decl: Option<ast::FieldDecl>,
    pub span: Option<SrcSpan>,
    pub locality: Locality,
}

impl Var {
    pub fn from_decl(
        domain: VarDomain,
        id: String,
        ty: ast::Type,
        decl: ast::FieldDecl,
        locality: Locality,
    ) -> Self {
        Var {
            domain,
            id: VarID::Name(id),
            ty,
            span: decl.span,
            decl: Some(decl),
            locality,
        }
    }

    pub fn numbered(
        domain: VarDomain,
        id: u64,
        ty: ast::Type,
        decl: Option<ast::FieldDecl>,
        span: Option<SrcSpan>,
        locality: Locality,
    ) -> Self {
        Var {
            domain,
            id: VarID::Num(id),
            ty,
            decl,
            span,
            locality,
        }
    }
}

impl Hash for Var {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.domain.hash(state);
        self.id.hash(state);
    }
}

impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.domain == other.domain && self.id == other.id
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "v@{}@{}", self.domain, self.id)
    }
}

impl Eq for Var {}

/// Versioned Var.
#[derive(Debug, Clone)]
pub struct VVar {
    pub var: Rc<Var>,
    pub version: Cell<u64>,
}

impl VVar {
    pub fn new(var: Rc<Var>, version: u64) -> Self {
        VVar {
            var,
            version: Cell::new(version),
        }
    }
}

impl Hash for VVar {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.var.id.hash(state);
        self.version.get().hash(state);
    }
}

impl PartialEq for VVar {
    fn eq(&self, other: &Self) -> bool {
        self.var.id == other.var.id && self.version == other.version
    }
}

impl fmt::Display for VVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.var, self.version.get())
    }
}

impl Eq for VVar {}

#[derive(Clone, PartialEq, Eq)]
pub enum Val {
    Var(VVar),
    Imm(ast::Literal),
}

impl Val {
    pub fn ty(&self) -> ast::Type {
        match self {
            Val::Var(v) => v.var.ty.clone(),
            Val::Imm(l) => l.ty(),
        }
    }

    pub fn get_var(&self) -> Option<&VVar> {
        match self {
            Val::Var(v) => Some(v),
            _ => None,
        }
    }

    pub fn get_string_literal(&self) -> Option<&str> {
        match self {
            Val::Imm(ast::Literal::String(s)) => Some(s.as_str()),
            _ => None,
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Var(v) => f.write_str(v.to_string().as_str()),
            Val::Imm(l) => f.write_str(l.to_string().as_str()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct Label {
    pub id: u64,
}

impl Label {
    pub fn new(id: u64) -> Self {
        Label { id }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.id.to_string().as_str())
    }
}

impl From<u64> for Label {
    fn from(id: u64) -> Self {
        Label { id }
    }
}

#[derive(Clone)]
pub struct CallBB {
    pub label: Label,
    pub args: Vec<VVar>,
}

impl CallBB {
    pub fn new(label: Label, args: Vec<VVar>) -> Self {
        CallBB { label, args }
    }
}

impl fmt::Display for CallBB {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.label,
            self.args
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Clone)]
pub enum Branch {
    UnCon {
        bb: CallBB,
    },
    Con {
        pred: Val,
        bb_true: CallBB,
        bb_false: CallBB,
    },
}

impl fmt::Display for Branch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Branch::UnCon { bb } => write!(f, "br {}", bb),
            Branch::Con {
                pred,
                bb_true,
                bb_false,
            } => write!(f, "br {}, {}, {}", pred, bb_true, bb_false),
        }
    }
}

#[derive(Clone)]
pub enum Statement {
    Assign {
        dst: VVar,
        src: Val,
    },
    Call {
        dst: Option<VVar>,
        method: String,
        arguments: Vec<Val>,
    },
    Alloca {
        dst: VVar,
        ty: ast::Type,
        size: Option<u64>,
    },
    Load {
        dst: VVar,
        ptr: Val,
    },
    Store {
        ptr: Val,
        src: Val,
    },
    Arith {
        dst: VVar,
        op: ast::ArithOp,
        l: Val,
        r: Val,
    },
    Cmp {
        dst: VVar,
        op: ast::CmpOp,
        l: Val,
        r: Val,
    },
    Cond {
        dst: VVar,
        op: ast::CondOp,
        l: Val,
        r: Val,
    },
    NNeg {
        dst: VVar,
        val: Val,
    },
    LNeg {
        dst: VVar,
        val: Val,
    },
    Br(Branch),
    Return(Option<Val>),
}

impl Statement {
    pub fn read_vars<'s>(&'s self) -> Vec<&VVar> {
        let reads = |val: &'s Val| val.get_var().into_iter().collect();
        match &self {
            Statement::Assign { dst: _, src } => reads(src),
            Statement::Call {
                dst: _,
                method: _,
                arguments,
            } => arguments.into_iter().flat_map(|a| a.get_var()).collect(),
            Statement::Return(r) => r.into_iter().flat_map(|v| v.get_var()).collect(),
            Statement::Alloca {
                dst: _,
                ty: _,
                size: _,
            } => Vec::new(),
            Statement::Load { dst: _, ptr } => reads(ptr),
            Statement::Store { ptr: _, src } => reads(src),
            Statement::Arith {
                dst: _,
                op: _,
                l,
                r,
            } => {
                let mut lreads = reads(l);
                let mut rreads = reads(r);
                lreads.append(&mut rreads);
                lreads
            }
            Statement::Cmp {
                dst: _,
                op: _,
                l,
                r,
            } => {
                let mut lreads = reads(l);
                let mut rreads = reads(r);
                lreads.append(&mut rreads);
                lreads
            }
            Statement::Cond {
                dst: _,
                op: _,
                l,
                r,
            } => {
                let mut lreads = reads(l);
                let mut rreads = reads(r);
                lreads.append(&mut rreads);
                lreads
            }
            Statement::NNeg { dst: _, val } => reads(val),
            Statement::LNeg { dst: _, val } => reads(val),
            Statement::Br(Branch::UnCon { bb: _ }) => Vec::new(),
            Statement::Br(Branch::Con {
                pred,
                bb_true: _,
                bb_false: _,
            }) => reads(pred),
        }
    }
    pub fn write_to_var(&self) -> Option<&VVar> {
        match &self {
            Statement::Assign { dst, src: _ } => Some(dst),
            Statement::Call {
                dst,
                method: _,
                arguments: _,
            } => dst.as_ref(),
            Statement::Return(_) => None,
            Statement::Alloca {
                dst,
                ty: _,
                size: _,
            } => Some(dst),
            Statement::Load { dst, ptr: _ } => Some(dst),
            Statement::Store { ptr: _, src: _ } => None,
            Statement::Arith {
                dst,
                op: _,
                l: _,
                r: _,
            } => Some(dst),
            Statement::Cmp {
                dst,
                op: _,
                l: _,
                r: _,
            } => Some(dst),
            Statement::Cond {
                dst,
                op: _,
                l: _,
                r: _,
            } => Some(dst),
            Statement::NNeg { dst, val: _ } => Some(dst),
            Statement::LNeg { dst, val: _ } => Some(dst),
            Statement::Br(_) => None,
        }
    }
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
    pub args: Vec<VVar>,
    pub statements: Vec<Statement>,
}

impl BasicBlock {
    pub fn new(label: Label, ast_scope: ast::Scope, args: Vec<VVar>) -> Self {
        BasicBlock {
            label,
            ast_scope,
            args,
            statements: Vec::new(),
        }
    }

    pub fn push_stmt(&mut self, stmt: Statement) {
        self.statements.push(stmt)
    }

    pub fn read_vars(&self) -> HashSet<&VVar> {
        self.statements.iter().flat_map(|s| s.read_vars()).collect()
    }
    pub fn write_vars(&self) -> HashSet<&VVar> {
        self.statements
            .iter()
            .flat_map(|s| s.write_to_var())
            .collect()
    }
}

impl Default for BasicBlock {
    fn default() -> Self {
        BasicBlock::new(Label::new(0), ast::Scope::new(0), vec![])
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "<label: {}, args: ({})>\n",
            self.label,
            self.args
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        for stmt in &self.statements {
            write!(f, "{};\n", stmt)?;
        }
        Ok(())
    }
}

pub struct Function {
    pub name: String,
    pub args: Vec<VVar>,
    pub ty: ast::Type,
    pub body: Vec<BasicBlock>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "define {} {}({}):\n",
            self.ty,
            self.name,
            self.args
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        for bb in &self.body {
            write!(f, "{}\n", bb)?;
        }
        Ok(())
    }
}

pub struct Module {
    pub imports: Vec<String>,
    pub globals: Vec<(VVar, Option<ast::Literal>)>,
    pub functions: Vec<Function>,
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in &self.imports {
            write!(f, "import {}\n", i)?;
        }
        write!(f, "\n")?;

        for (g, val) in &self.globals {
            if val.is_some() {
                write!(f, "{} = global {} {}\n", g, g.var.ty, val.as_ref().unwrap())?;
            } else {
                write!(f, "{} = global {}\n", g, g.var.ty)?;
            }
        }
        write!(f, "\n")?;

        for func in &self.functions {
            write!(f, "{}", func)?;
        }
        Ok(())
    }
}

pub trait IRTransform {
    fn run(&mut self, module: &mut Module);
}

struct StringLiteralVisitor {
    literals: Vec<(VVar, String)>,
    count: u64,
}

impl StringLiteralVisitor {
    fn new() -> Self {
        StringLiteralVisitor {
            literals: Vec::new(),
            count: 0,
        }
    }

    fn run(&mut self, m: &mut Module) {
        for f in &mut m.functions {
            for b in &mut f.body {
                for s in &mut b.statements {
                    self.visit_statement(s);
                }
            }
        }
    }

    fn visit_statement(&mut self, s: &mut Statement) {
        match s {
            Statement::Call {
                dst: _,
                method: _,
                arguments,
            } => {
                for a in arguments {
                    if a.ty().is_string() {
                        let s = a.get_string_literal().unwrap();
                        let var = self.new_literal_var(s, None);
                        self.literals.push((var.clone(), s.to_string()));
                        *a = Val::Var(var);
                    }
                }
            }
            _ => (),
        }
    }

    fn new_literal_var(&mut self, s: &str, span: Option<SrcSpan>) -> VVar {
        let str_ty = ast::Type::Array(Box::new(ast::Type::Char), s.len() as u64);
        let count = self.count;
        self.count += 1;
        VVar::new(
            Rc::new(Var::numbered(
                VarDomain::StrLit,
                count,
                str_ty,
                None,
                span,
                Locality::Global,
            )),
            0,
        )
    }

    fn literals(self) -> Vec<(VVar, String)> {
        self.literals
    }
}

pub struct HoistStringLiteral {}

impl IRTransform for HoistStringLiteral {
    fn run(&mut self, module: &mut Module) {
        let mut sl_visitor = StringLiteralVisitor::new();
        sl_visitor.run(module);
        for (var, s) in sl_visitor.literals() {
            module.globals.push((var, Some(ast::Literal::String(s))))
        }
    }
}
