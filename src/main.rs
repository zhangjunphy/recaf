use clap::Parser;
use recaf::cli::{Args, Stage};
use recaf::parser::lexer::Lexer;
use std::fs::File;
use std::io::prelude::*;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub grammar, "/parser/grammar.rs");

fn main() {
    let args = Args::parse();
    if args.file.is_none() {
        eprintln!("Decaf source file not provided.")
    }

    let file = args.file.unwrap();
    match args.stage {
        Stage::Lex => lex(&file),
    }
}

fn lex(file: &String) {
    let mut content = String::new();
    let mut f = match File::open(file) {
        Err(msg) => panic!("Could not open {}: {}", file, msg),
        Ok(file) => file,
    };
    if let Err(msg) = f.read_to_string(&mut content) {
        panic!("Error reading {}: {}", file, msg);
    }

    let mut lexer = Lexer::new(file);

    while let Some(tok_or_err) = lexer.next() {
        match tok_or_err {
            Ok((_, tok, _)) => println!("{:?}", tok),
            Err(err) => println!("{}", err.msg),
        }
    }
}

#[test]
fn grammar() {
    let sample = String::from("22");
    let res = grammar::IntLiteralParser::new().parse(sample.as_str());
    println!("{}", res.expect(""));
    assert!(grammar::IntLiteralParser::new()
        .parse(sample.as_str())
        .is_ok());
}
