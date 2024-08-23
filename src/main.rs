use clap::Parser;
use recaf::ast::ASTPrinter;
use recaf::cfg::{build, draw, optimize, optimize::CFGOptimizer};
use recaf::cli::{Args, Stage};
use recaf::codegen::genx86;
use recaf::ir;
use recaf::parser::lexer::Lexer;
use recaf::semantic;
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
        Stage::Cfg => cfg(&file),
        Stage::Ir => ir(&file),
        Stage::Asm => asm(&file),
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
        Ok(mut program) => {
            println!("AST:");
            printer.print(&program).expect("Unable to print ast.");
            if let Err(errors) = semantic::check(&mut program) {
                for e in &errors {
                    eprintln!("{}", e);
                }
            }
        }
        Err(err) => eprintln!("{}", err),
    }
}

fn cfg(file: &String) {
    let mut content = String::new();
    let mut f = match File::open(file) {
        Err(msg) => panic!("Could not open {}: {}", file, msg),
        Ok(file) => file,
    };
    if let Err(msg) = f.read_to_string(&mut content) {
        panic!("Error reading {}: {}", file, msg);
    }

    let mut program = match recaf::parser::parse(&content) {
        Ok(program) => program,
        Err(err) => {
            panic!("{}", err)
        }
    };

    let symbols = match semantic::check(&mut program) {
        Err(errors) => panic!("{:?}", errors),
        Ok(s) => s,
    };

    let build = build::CFGBuild::new(&symbols);
    let mut p = build.build(&program);
    for (_, cfg) in &mut p.cfgs {
        let mut optimizer = optimize::RemoveEmptyNodes {};
        optimizer.run(cfg);
    }

    let dot_str = draw::draw_program(p);
    println!("{}", dot_str);
}

fn ir(file: &String) {
    let mut content = String::new();
    let mut f = match File::open(file) {
        Err(msg) => panic!("Could not open {}: {}", file, msg),
        Ok(file) => file,
    };
    if let Err(msg) = f.read_to_string(&mut content) {
        panic!("Error reading {}: {}", file, msg);
    }

    let mut program = match recaf::parser::parse(&content) {
        Ok(program) => program,
        Err(err) => {
            panic!("{}", err)
        }
    };

    let symbols = match semantic::check(&mut program) {
        Err(errors) => panic!("{:?}", errors),
        Ok(s) => s,
    };

    let build = build::CFGBuild::new(&symbols);
    let mut p = build.build(&program);
    for (_, cfg) in &mut p.cfgs {
        let mut optimizer = optimize::RemoveEmptyNodes {};
        optimizer.run(cfg);
    }

    let mut ir = p.linearize(&program);

    use ir::IRTransform;
    let mut hoister_str = ir::HoistStringLiteral {};
    hoister_str.run(&mut ir);
    println!("{}", ir);
}

fn asm(file: &String) {
    let mut content = String::new();
    let mut f = match File::open(file) {
        Err(msg) => panic!("Could not open {}: {}", file, msg),
        Ok(file) => file,
    };
    if let Err(msg) = f.read_to_string(&mut content) {
        panic!("Error reading {}: {}", file, msg);
    }

    let mut program = match recaf::parser::parse(&content) {
        Ok(program) => program,
        Err(err) => {
            panic!("{}", err)
        }
    };

    let symbols = match semantic::check(&mut program) {
        Err(errors) => panic!("{:?}", errors),
        Ok(s) => s,
    };

    let build = build::CFGBuild::new(&symbols);
    let mut p = build.build(&program);
    for (_, cfg) in &mut p.cfgs {
        let mut optimizer = optimize::RemoveEmptyNodes {};
        optimizer.run(cfg);
    }

    let mut ir = p.linearize(&program);

    use ir::IRTransform;
    let mut hoister_str = ir::HoistStringLiteral {};
    hoister_str.run(&mut ir);

    let mut gen = genx86::CodeGenX86::new();
    let asm = gen.run(ir);
    println!("{}", asm);
}
