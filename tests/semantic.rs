use recaf::parser::grammar;
use recaf::parser::lexer::Lexer;
use recaf::parser::ParserState;
use recaf::semantic;

mod common;

mod tests {
    use super::*;

    #[test]
    fn test_symbol_table() {
        let content = common::read_resource_file("resources/snippets/arith.dcf").unwrap();
        let lexer = Lexer::new(content.as_str());
        let state = ParserState::new();
        let mut program = grammar::ProgramParser::new().parse(&state, lexer).unwrap();
        let mut se = semantic::SymbolTableBuilder::new();
        let table = se.process(&mut program).unwrap();
        assert!(table.imports.contains("printf"));
        assert!(table.methods.contains_key("main"));
        let table_0 = table.variables.get(&0).unwrap();
        assert!(table_0.variables.contains_key("a"));
        let table_1 = table.variables.get(&1).unwrap();
        assert!(table_1.variables.contains_key("c"));
    }
}
