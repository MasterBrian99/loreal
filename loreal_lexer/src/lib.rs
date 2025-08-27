use std::iter::Peekable;
use std::str::CharIndices;
use thiserror::Error;

mod token;
mod  span;

#[derive(Error, Debug, Clone)]
pub enum LexError {
    #[error("Unexpected character '{ch}' at position {pos}")]
    UnexpectedCharacter { ch: char, pos: usize },

    #[error("Unterminated string literal starting at position {start}")]
    UnterminatedString { start: usize },

    #[error("Invalid escape sequence '\\{seq}' at position {pos}")]
    InvalidEscapeSequence { seq: String, pos: usize },

    #[error("Invalid number format '{text}' at position {pos}")]
    InvalidNumber { text: String, pos: usize },

    #[error("Unterminated block comment starting at position {start}")]
    UnterminatedBlockComment { start: usize },

    #[error("Unterminated character literal at position {pos}")]
    UnterminatedCharLiteral { pos: usize },
}

pub struct Lexer<'source> {
    _source: &'source str,
    /// Iterator over chars with their byte positions
    chars: Peekable<CharIndices<'source>>,
    /// Current byte position
    current_pos: usize,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            _source: source,
            chars: source.char_indices().peekable(),
            current_pos: 0,
        }
    }
}
