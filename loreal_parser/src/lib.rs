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

    pub fn parse(&mut self) -> Result<Module, Vec<ParseError>> {
        let mut declarations = Vec::new();

        while self.peek().is_some() {
            if let Some(decl) = self.parse_declaration() {
                declarations.push(decl);
            } else {
                self.advance();
            }
        }

        Ok(Module {
            declarations,
            span: Span::new(0, 0),
        })
    }

    fn parse_declaration(&mut self) -> Option<Declaration> {
        match self.peek() {
            Some(TokenKind::Fn) => self.parse_function(),
            Some(TokenKind::Struct) => self.parse_struct(),
            _ => {
                self.errors.push(ParseError::UnexpectedToken {
                    expected: "fn or struct".to_string(),
                    found: self.current.clone().unwrap_or(Token::new(TokenKind::Eof, Span::new(0, 0))),
                });
                None
            }
        }
    }

    fn parse_function(&mut self) -> Option<Declaration> {
        self.expect(TokenKind::Fn).ok()?;
        let name = self.expect_identifier()?;

        self.expect(TokenKind::LParen).ok()?;
        let params = self.parse_params()?;
        self.expect(TokenKind::RParen).ok()?;

        let return_type = if self.check(&TokenKind::Arrow) {
            self.expect(TokenKind::Arrow).ok()?;
            self.parse_type()
        } else {
            Type::Named {
                name: "Nil".into(),
                span: Span::new(0, 0),
            }
        };

        self.expect(TokenKind::Do).ok()?;
        let body = self.parse_expr()?;
        self.expect(TokenKind::End).ok()?;

        Some(Declaration::Function(FunctionDef {
            name,
            params,
            return_type,
            body,
            span: Span::new(0, 0),
        }))
    }

    fn parse_struct(&mut self) -> Option<Declaration> {
        self.expect(TokenKind::Struct).ok()?;
        let name = self.expect_identifier()?;
        self.expect(TokenKind::LBrace).ok()?;

        let mut fields = Vec::new();
        while !self.check(&TokenKind::RBrace) && self.peek().is_some() {
            let field_name = self.expect_identifier()?;
            self.expect(TokenKind::Colon).ok()?;
            let field_type = self.parse_type();
            fields.push((field_name, field_type));

            if !self.check(&TokenKind::RBrace) {
                self.expect(TokenKind::Comma).ok();
            }
        }

        self.expect(TokenKind::RBrace).ok()?;

        Some(Declaration::Struct(StructDef {
            name,
            fields,
            span: Span::new(0, 0),
        }))
    }

    fn expect_identifier(&mut self) -> Option<SmolStr> {
        match self.current.as_ref() {
            Some(Token { kind: TokenKind::Identifier(name), .. }) => {
                let name = name.clone();
                self.advance();
                Some(name)
            }
            _ => None,
        }
    }

    fn parse_params(&mut self) -> Option<Vec<(Pattern, Type)>> {
        let mut params = Vec::new();

        while !self.check(&TokenKind::RParen) && self.peek().is_some() {
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::Colon).ok()?;
            let ty = self.parse_type();
            params.push((pattern, ty));

            if !self.check(&TokenKind::RParen) {
                self.expect(TokenKind::Comma).ok();
            }
        }

        Some(params)
    }
}
