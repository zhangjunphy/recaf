use recaf::parser::lexer::{Lexer, Tok};
use recaf::parser::grammar;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let mut lexer = Lexer::new("import");
        assert_eq!(lexer.next().unwrap().unwrap().1, Tok::Import);
    }


    #[test]
    fn test_grammar() {
        let sample = "22";
        assert!(grammar::IntLiteralParser::new()
                .parse(sample)
                .is_ok());
    }
}
