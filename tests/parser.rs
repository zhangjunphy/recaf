use recaf::ast::*;
use recaf::parser::grammar;
use recaf::parser::lexer::Lexer;

fn parse_expr (s: &str) -> Expr {
    let lexer = Lexer::new(s);
    let res = grammar::ExprParser::new().parse(lexer);
    res.unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_int() {
        let samples = vec![("22", 22), ("0x2f", 0x2f), ("0x2F", 0x2F)];

        for (sample, expected) in samples {
            let lexer = Lexer::new(sample);
            let res = grammar::LiteralParser::new().parse(lexer);
            assert!(res.is_ok() && res.unwrap() == Literal::Int(expected));
        }
    }

    #[test]
    fn test_parse_expr() {
        match parse_expr("22 + 0x3f").expr {
            Expr_::BinOp(l, op, r) => {
                assert_eq!(l.expr, Expr_::Literal(Literal::Int(22)));
                assert_eq!(op, BinOp::Add);
                assert_eq!(r.expr, Expr_::Literal(Literal::Int(0x3f)));
            }
            _ => panic!(),
        }

        match parse_expr("- 0x3f + 33").expr {
            Expr_::BinOp(l, op, r) => {
                match l.expr {
                    Expr_::NNeg(e) => match e.expr {
                        Expr_::Literal(Literal::Int(0x3f)) => (),
                        _ => panic!(),
                    },
                    _ => panic!(),
                };
                assert_eq!(op, BinOp::Add);
                assert_eq!(r.expr, Expr_::Literal(Literal::Int(33)));
            }
            _ => panic!(),
        }

        match parse_expr("f(a)").expr {
            Expr_::MethodCall(MethodCall { name, arguments }) => {
                assert_eq!(name.id, "f");
                match &arguments[0] {
                    Argument::Expr(e) => match &e.expr {
                        Expr_::Location(Location::Scalar(id)) => assert_eq!(id.id, "a"),
                        _ => panic!(),
                    },
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_ternary_expr_assoc() {
        match parse_expr("true ? 1 : false ? 2 : 3").expr {
            Expr_::TernaryOp(p, t, f) => {
                match p.expr {
                    Expr_::Literal(Literal::Bool(true)) => (),
                    _ => panic!(),
                };
                match t.expr {
                    Expr_::Literal(Literal::Int(1)) => (),
                    _ => panic!(),
                };
                match f.expr {
                    Expr_::TernaryOp(_, _, _) => (),
                    _ => panic!(),
                };
            },
            _ => panic!(),
        }
    }
}
