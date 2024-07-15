
fn main() {
    let v : Vec<_> = vec![1, 2, 3];
    let f = &v[0];
    println!("element: {}", f);
    println!("elements: {:?}", v);
}


#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar, "/parser/grammar.rs");

#[test]
fn grammar() {
    let sample = String::from("22");
    let res = grammar::IntLiteralParser::new().parse(sample.as_str());
    println!("{}", res.expect(""));
    assert!(grammar::IntLiteralParser::new().parse(sample.as_str()).is_ok());
}
