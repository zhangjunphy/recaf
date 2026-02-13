use std::collections::HashMap;

pub fn escape_sequence_map() -> Vec<(&'static str, char)> {
    vec![
        (r#"\n"#, '\n'),
        (r#"\\"#, '\\'),
        (r#"\t"#, '\t'),
        (r#"\'"#, '\''),
        (r#"\""#, '"'),
    ]
}

pub fn escape_string_literal(s: &str) -> String {
    let mut res = String::new();
    let esc_map: HashMap<char, &'static str> = escape_sequence_map()
        .into_iter()
        .map(|(seq, c)| (c, seq))
        .collect();
    for c in s.chars() {
        if let Some(seq) = esc_map.get(&c) {
            res.push_str(seq);
        } else {
            res.push(c);
        }
    }
    res
}
