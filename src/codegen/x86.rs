pub struct Imm(u64);
pub struct Mem(u64);
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
    Imm(Imm),
}

pub enum Dest {
    Reg(Reg),
    Mem(Mem),
}

pub struct Label {
    pub str: String,
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
    MoveQ { src: Src, dest: Dest },
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
    // String
    String(String),
}

pub struct Block {
    pub label: Label,
    pub asms: Vec<AsmX86>,
}
