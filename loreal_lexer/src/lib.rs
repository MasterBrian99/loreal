#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// Byte offset of the start of the span (inclusive)
    pub start: usize,
    /// Byte offset of the end of the span (exclusive)
    pub end: usize,
}

impl Span {
    /// Create a new span from start and end byte offsets
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end, "Span start must be <= end");
        Self { start, end }
    }

    /// Merge two spans into a single span covering both
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// Get the length of the span in bytes
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Check if the span is empty
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}


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
