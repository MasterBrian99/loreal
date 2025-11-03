use loreal_ast::span::Span;
use smol_str::SmolStr;


#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Types of tokens in the Loreal language
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Module,
    Def,
    Do,
    End,
    Let,
    If,
    Else,
    Match,
    Import,
    Export,
    Protocol,
    Impl,
    Struct,
    True,
    False,
    Nil,
    Fn,
    Loop,
    Next,
    Break,
    Then,
    Case,
    Match,

    // Identifiers and Literals
    Identifier(SmolStr),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(SmolStr),
    CharLiteral(char),

    // Operators
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %

    // Comparison
    EqEq, // ==
    Ne,   // !=
    Lt,   // <
    Le,   // <=
    Gt,   // >
    Ge,   // >=

    // Logical
    And, // and (keyword-based)
    Or,  // or (keyword-based)
    Not, // !

    // Assignment and Arrows
    Eq,       // =
    Arrow,    // ->
    FatArrow, // =>
    Pipe,     // |>

    // Delimiters
    LParen,    // (
    RParen,    // )
    LBracket,  // [
    RBracket,  // ]
    LBrace,    // {
    RBrace,    // }
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;
    Dot,       // .
    Bar,       // |

    // Special
    Eof,
}
