use recaf::parser::grammar;
use recaf::parser::lexer::Lexer;
use recaf::parser::ParserState;
use recaf::semantic;

mod common;

mod tests {
    use super::*;

    #[test]
    fn test_symbol_table() {
        let content = common::read_resource_file("resources/snippets/hello_world.dcf").unwrap();
        let lexer = Lexer::new(content.as_str());
        let state = ParserState::new();
        let program = grammar::ProgramParser::new().parse(&state, lexer).unwrap();
        let mut se = semantic::SemanticChecker::new(&program);
        se.process().unwrap();
        se.get_symbol_table();
    }
}
