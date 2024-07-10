fn main() {
    let v : Vec<_> = vec![1, 2, 3];
    let f = &v[0];
    println!("element: {}", f);
    println!("elements: {:?}", v);
}

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar, "/parser/grammar.rs");

#[cfg(test)]
use recaf::parser::source_pos::SourcePosMap;

#[test]
fn grammar() {
    let sample = String::from("22");
    let source_map = SourcePosMap::new(&sample).expect("Source map failed to build.");
    let res = grammar::IntLiteralParser::new().parse(&source_map, sample.as_str());
    println!("{}", res.expect(""));
    assert!(grammar::IntLiteralParser::new().parse(&source_map, sample.as_str()).is_ok());
}
