use loreal_ast::*;
use loreal_lexer::{LexError, Token, TokenKind};
use std::iter::Peekable;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Unexpected token: expected {expected}, found {found:?}")]
    UnexpectedToken { expected: String, found: Token },

    #[error("Unexpected end of file: expected {expected}")]
    UnexpectedEof { expected: String },

    #[error("Lexical error: {0}")]
    LexError(#[from] LexError),

    #[error("Invalid pattern at {token:?}")]
    InvalidPattern { token: Token },

    #[error("Invalid type expression at {token:?}")]
    InvalidType { token: Token },
}

pub struct Parser<I: Iterator<Item = Result<Token, LexError>>> {
    tokens: Peekable<I>,
    errors: Vec<ParseError>,
    current: Option<Token>,
}

impl<I: Iterator<Item = Result<Token, LexError>>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        let mut iter = tokens.peekable();
        let current = Self::get_next_token(&mut iter);

        Self {
            tokens: iter,
            errors: Vec::new(),
            current,
        }
    }

    fn get_next_token(iter: &mut Peekable<I>) -> Option<Token> {
        loop {
            match iter.next()? {
                Ok(token) => return Some(token),
                Err(_) => continue,
            }
        }
    }

    fn peek(&self) -> Option<&TokenKind> {
        self.current.as_ref().map(|t| &t.kind)
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if let Some(current) = &self.current {
            std::mem::discriminant(&current.kind) == std::mem::discriminant(kind)
        } else {
            false
        }
    }

    fn advance(&mut self) -> Option<Token> {
        let prev = self.current.take();
        self.current = Self::get_next_token(&mut self.tokens);
        prev
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, ParseError> {
        if let Some(token) = &self.current {
            if self.check(&expected) {
                Ok(self.advance().unwrap())
            } else {
                Err(ParseError::UnexpectedToken {
                    expected: format!("{:?}", expected),
                    found: token.clone(),
                })
            }
        } else {
            Err(ParseError::UnexpectedEof {
                expected: format!("{:?}", expected),
            })
        }
    }
}
