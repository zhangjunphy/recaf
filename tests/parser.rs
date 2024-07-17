use recaf::parser::ast;
use recaf::parser::grammar;
use recaf::parser::lexer::Lexer;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_int() {
        let mut sample = "22";
        let mut lexer = Lexer::new(sample);
        let mut res = grammar::LiteralParser::new().parse(lexer);
        assert!(res.is_ok() && res.unwrap() == ast::Literal::Int(22));

        sample = "0x2F";
        lexer = Lexer::new(sample);
        res = grammar::LiteralParser::new().parse(lexer);
        assert!(res.is_ok() && res.unwrap() == ast::Literal::Int(0x2f));
    }
}
