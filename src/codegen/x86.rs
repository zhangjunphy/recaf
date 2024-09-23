use std::fmt;

pub enum Mem {
    Imm(u64),
    RegOffset(Reg, i64),
    Label(Label),
}

pub enum Reg {
    RAX,
    RBX,
    RCX,
    RDX,
    RSP,
    RBP,
    RSI,
    RDI,
    R(u64),
}

impl From<u64> for Reg {
    fn from(i: u64) -> Self {
        match i {
            0 => Self::RAX,
            1 => Self::RBX,
            2 => Self::RCX,
            3 => Self::RDX,
            4 => Self::RSP,
            5 => Self::RBP,
            6 => Self::RSI,
            7 => Self::RDI,
            i if i > 7 && i < 16 => Self::R(i),
            _ => panic!("Register idx out of range: {}", i),
        }
    }
}

impl Into<u64> for Reg {
    fn into(self) -> u64 {
        match self {
            Self::RAX => 0,
            Self::RBX => 1,
            Self::RCX => 2,
            Self::RDX => 3,
            Self::RSP => 4,
            Self::RBP => 5,
            Self::RSI => 6,
            Self::RDI => 7,
            Self::R(i) => i,
        }
    }
}

pub enum Src {
    Reg(Reg),
    Mem(Mem),
    Imm(i64),
}

pub enum Dest {
    Reg(Reg),
    Mem(Mem),
}

#[derive(Clone)]
pub struct Label {
    pub str: String,
}

impl Label {
    pub fn new(str: &str) -> Self {
        Label {
            str: str.to_string(),
        }
    }
}

pub enum AsmX86 {
    // Stack
    Enter(u64),
    Leave,
    Push(Src),
    Pop(Dest),
    // Control flow
    Ret,
    Call(Label),
    Jmp(Label),
    Je(Label),
    Jne(Label),
    // Copying
    MovQ { src: Src, dest: Dest },
    CMovE { src: Src, dest: Dest },
    CMovNe { src: Src, dest: Dest },
    CMovG { src: Src, dest: Dest },
    CMovL { src: Src, dest: Dest },
    CMovGe { src: Src, dest: Dest },
    CMovLe { src: Src, dest: Dest },
    // Arith/Logic
    Add { src: Src, dest: Dest },
    Sub { src: Src, dest: Dest },
    IMul { src: Src, dest: Dest },
    IDiv { div: Src },
    Shr { reg: Reg },
    Shl { reg: Reg },
    Ror { src: Src, dest: Dest },
    Cmp { src: Src, dest: Dest },
    // Data
    String(String),
    Zero(u64),
}

pub struct Block {
    pub label: Label,
    pub asms: Vec<AsmX86>,
}

pub enum SectionKind {
    Data,
    Text,
}

pub struct Section {
    pub kind: SectionKind,
    pub blocks: Vec<Block>,
}

pub struct Assembly {
    pub sections: Vec<Section>,
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.str)
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg::RAX => write!(f, "%rax"),
            Reg::RBX => write!(f, "%rbx"),
            Reg::RCX => write!(f, "%rcx"),
            Reg::RDX => write!(f, "%rdx"),
            Reg::RSP => write!(f, "%rsp"),
            Reg::RBP => write!(f, "%rbp"),
            Reg::RSI => write!(f, "%rsi"),
            Reg::RDI => write!(f, "%rdi"),
            Reg::R(i) => write!(f, "%r{}", i),
        }
    }
}

impl fmt::Display for Src {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Src::Imm(i) => write!(f, "${}", i),
            Src::Mem(Mem::Imm(i)) => write!(f, "{:#x}", i),
            Src::Mem(Mem::RegOffset(r, offset)) => write!(f, "{}({})", offset, r),
            Src::Reg(r) => write!(f, "{}", r),
        }
    }
}

impl fmt::Display for Dest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Dest::Mem(Mem::Imm(i)) => write!(f, "{:#x}", i),
            Dest::Mem(Mem::RegOffset(r, offset)) => write!(f, "{}({})", offset, r),
            Dest::Reg(r) => write!(f, "{}", r),
        }
    }
}

impl fmt::Display for AsmX86 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmX86::Enter(size) => write!(f, "enter ${}, $0", size),
            AsmX86::Leave => write!(f, "leave"),
            AsmX86::Push(src) => write!(f, "push {}", src),
            AsmX86::Pop(dest) => write!(f, "pop {}", dest),
            AsmX86::Ret => write!(f, "ret"),
            AsmX86::Call(l) => write!(f, "call {}", l),
            AsmX86::Jmp(l) => write!(f, "jmp {}", l),
            AsmX86::Je(l) => write!(f, "je {}", l),
            AsmX86::Jne(l) => write!(f, "jne {}", l),
            AsmX86::MovQ { src, dest } => write!(f, "movq {}, {}", src, dest),
            AsmX86::CMovE { src, dest } => write!(f, "cmove {}, {}", src, dest),
            AsmX86::CMovNe { src, dest } => write!(f, "cmovne {}, {}", src, dest),
            AsmX86::CMovG { src, dest } => write!(f, "cmovg {}, {}", src, dest),
            AsmX86::CMovL { src, dest } => write!(f, "cmovl {}, {}", src, dest),
            AsmX86::CMovGe { src, dest } => write!(f, "cmovge {}, {}", src, dest),
            AsmX86::CMovLe { src, dest } => write!(f, "cmovle {}, {}", src, dest),
            AsmX86::Add { src, dest } => write!(f, "add {}, {}", src, dest),
            AsmX86::Sub { src, dest } => write!(f, "sub {}, {}", src, dest),
            AsmX86::IMul { src, dest } => write!(f, "imul {}, {}", src, dest),
            AsmX86::IDiv { div } => write!(f, "idiv {}", div),
            AsmX86::Shr { reg } => write!(f, "shr {}", reg),
            AsmX86::Shl { reg } => write!(f, "shl {}", reg),
            AsmX86::Ror { src, dest } => write!(f, "ror {}, {}", src, dest),
            AsmX86::Cmp { src, dest } => write!(f, "cmp {}, {}", src, dest),
            AsmX86::String(s) => write!(
                f,
                ".string \"{}\"",
                crate::parser::util::escape_string_literal(s)
            ),
            AsmX86::Zero(size) => write!(f, ".zero {}", size),
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:\n", self.label)?;
        for asm in &self.asms {
            write!(f, "{}\n", asm)?;
        }
        Ok(())
    }
}

impl fmt::Display for SectionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SectionKind::Data => write!(f, ".data"),
            SectionKind::Text => write!(f, ".text"),
        }
    }
}

impl fmt::Display for Section {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n", self.kind)?;
        for block in &self.blocks {
            write!(f, "{}", block)?;
        }
        Ok(())
    }
}

impl fmt::Display for Assembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for sec in &self.sections {
            write!(f, "{}\n", sec)?;
        }
        Ok(())
    }
}
