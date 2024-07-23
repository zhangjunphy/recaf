use clap::Parser;
use recaf::ast::ASTPrinter;
use recaf::cli::{Args, Stage};
use recaf::parser::lexer::Lexer;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let args = Args::parse();
    if args.file.is_none() {
        eprintln!("Decaf source file not provided.")
    }

    let file = args.file.unwrap();
    match args.stage {
        Stage::Lex => lex(&file),
        Stage::Parse => parse(&file),
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

    let mut lexer = Lexer::new(&content);
    while let Some(tok_or_err) = lexer.next() {
        match tok_or_err {
            Ok((_, tok, _)) => println!("{:?}", tok),
            Err(err) => eprintln!("{}", err.msg),
        }
    }
}

fn parse(file: &String) {
    let mut content = String::new();
    let mut f = match File::open(file) {
        Err(msg) => panic!("Could not open {}: {}", file, msg),
        Ok(file) => file,
    };
    if let Err(msg) = f.read_to_string(&mut content) {
        panic!("Error reading {}: {}", file, msg);
    }

    let stdout = &mut std::io::stdout();
    let mut printer = ASTPrinter::new(stdout, 2);

    match recaf::parser::parse(&content) {
        Ok(program) => {
            printer.print(&program).expect("Unable to print ast.");
        }
        Err(err) => eprintln!("{}", err),
    }
}
