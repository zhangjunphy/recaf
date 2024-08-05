use recaf::ast::*;
use recaf::parser;
use recaf::semantic;

mod common;

mod tests {
    use super::*;

    #[test]
    fn test_symbol_table() {
        let content = common::read_resource_file("resources/snippets/arith.dcf").unwrap();
        let mut program = parser::parse(&content).unwrap();
        if let Ok(symbols) = semantic::check(&mut program) {
            assert!(symbols.imports.contains("printf"));
            assert!(symbols.methods.contains_key("main"));
            let symbols_0 = symbols.variables.get(&Scope::new(0)).unwrap();
            assert!(symbols_0.variables.contains_key("a"));
            let symbols_1 = symbols.variables.get(&Scope::new(1)).unwrap();
            assert!(symbols_1.variables.contains_key("c"));
        }
    }

    #[test]
    fn test_semantic_checker() {
        let content =
            common::read_resource_file("resources/snippets/semantic/no_main.dcf").unwrap();
        let mut program = parser::parse(&content).unwrap();

        match semantic::check(&mut program) {
            Ok(_) => panic!(),
            Err(errors) => assert!(errors.len() == 1),
        }
    }

    #[test]
    fn test_type_inference() {
        let content = common::read_resource_file("resources/snippets/arith.dcf").unwrap();
        let mut program = parser::parse(&content).unwrap();

        match semantic::check(&mut program) {
            Ok(_) => {
                let f = program.methods.get(0).unwrap();
                match &f.block.statements.get(2).unwrap().stmt {
                    Stmt_::Assign(assign) => {
                        assert_eq!(assign.expr.ty, Type::Int);
                    }
                    _ => panic!(),
                }
            },
            Err(errors) => panic!("{:?}", errors),
        }
    }
}
