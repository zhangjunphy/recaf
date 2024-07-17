use recaf::parser::ast;
use recaf::parser::grammar;
use recaf::parser::lexer::Lexer;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_int() {
        let sample = "22";
        let lexer = Lexer::new(sample);
        let res = grammar::LiteralParser::new().parse(lexer);
        assert!(res.is_ok() && res.unwrap() == ast::Literal::Int(22));
    }
}
