fn main() {
    let mut v : Vec<_> = vec![1, 2, 3];
    let f = &v[0];
    println!("element: {}", f);
    println!("elements: {:?}", v);
}

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar, "/parser/grammar.rs");

#[test]
fn grammar() {
    assert!(grammar::ExprParser::new().parse("22").is_ok());
}
