use recaf::parser::lexer::{Lexer, Tok};
use recaf::parser::grammar;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_keywords() {
        let mut lexer: Lexer;

        lexer = Lexer::new("import");
        assert_eq!(lexer.next().unwrap().unwrap().1, Tok::Import);

        lexer = Lexer::new("for");
        assert_eq!(lexer.next().unwrap().unwrap().1, Tok::For);
    }

    #[test]
    fn test_tokenize_numbers() {
        let mut lexer: Lexer;

        lexer = Lexer::new("22");
        assert_eq!(lexer.next().unwrap().unwrap().1, Tok::DecLiteral("22".to_string()));

        lexer = Lexer::new("0x22");
        assert_eq!(lexer.next().unwrap().unwrap().1, Tok::HexLiteral("0x22".to_string()));
    }

    #[test]
    fn test_tokenize_escapes() {
        let mut lexer: Lexer;

        lexer = Lexer::new(r#""\\n""#);
        assert_eq!(lexer.next().unwrap().unwrap().1, Tok::String("\n".to_string()));

        lexer = Lexer::new(r#""\\\\""#);
        assert_eq!(lexer.next().unwrap().unwrap().1, Tok::String("\\".to_string()));
    }

    #[test]
    fn test_tokenize_comments() {
        let mut lexer: Lexer;

        lexer = Lexer::new(r#"// this is a comment"#);
        assert!(lexer.next().is_none());

        lexer = Lexer::new(r#"/* /* level 2 comment */ */ import"#);
        assert_eq!(lexer.next().unwrap().unwrap().1, Tok::Import);
    }

    #[test]
    fn test_grammar() {
        let sample = "22";
        let lexer = Lexer::new(sample);
        let res = grammar::IntLiteralParser::new()
            .parse(lexer);
        println!("{:#?}", res);
        assert!(res.is_ok());
    }
}
