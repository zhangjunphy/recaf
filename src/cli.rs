use clap::{Parser, ValueEnum};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Stage {
    Lex,
    Parse,
    Cfg,
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Args {
    pub file: Option<String>,

    #[arg(value_enum, short='t', long, default_value_t=Stage::Parse)]
    pub stage: Stage,
}
