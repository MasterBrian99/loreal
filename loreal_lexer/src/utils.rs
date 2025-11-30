use smol_str::SmolStr;

pub fn is_keyword(ident: &str) -> bool {
    matches!(
        ident,
        "module"
            | "def"
            | "do"
            | "end"
            | "let"
            | "if"
            | "else"
            | "match"
            | "import"
            | "export"
            | "protocol"
            | "impl"
            | "struct"
            | "true"
            | "false"
            | "nil"
            | "fn"
            | "loop"
            | "next"
            | "break"
            | "then"
            | "case"
            | "and"
            | "or"
    )
}

pub fn escape_string(s: &str) -> String {
    let mut result = String::new();
    for ch in s.chars() {
        match ch {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            _ => result.push(ch),
        }
    }
    result
}
