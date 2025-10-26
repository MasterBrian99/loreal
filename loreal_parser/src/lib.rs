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
                    found: self
                        .current
                        .clone()
                        .unwrap_or(Token::new(TokenKind::Eof, Span::new(0, 0))),
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
            Some(Token {
                kind: TokenKind::Identifier(name),
                ..
            }) => {
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

    fn parse_type(&mut self) -> Type {
        match self.peek() {
            Some(TokenKind::Identifier(_)) => {
                let name = self.expect_identifier().unwrap();
                Type::Named {
                    name,
                    span: Span::new(0, 0),
                }
            }
            Some(TokenKind::LBracket) => {
                self.expect(TokenKind::LBracket).ok();
                let elem_type = self.parse_type();
                self.expect(TokenKind::RBracket).ok();
                Type::List {
                    element_type: Box::new(elem_type),
                    span: Span::new(0, 0),
                }
            }
            Some(TokenKind::LParen) => {
                self.expect(TokenKind::LParen).ok();
                let mut types = Vec::new();
                while !self.check(&TokenKind::RParen) && self.peek().is_some() {
                    types.push(self.parse_type());
                    if !self.check(&TokenKind::RParen) {
                        self.expect(TokenKind::Comma).ok();
                    }
                }
                self.expect(TokenKind::RParen).ok();
                Type::Tuple {
                    types,
                    span: Span::new(0, 0),
                }
            }
            _ => Type::Named {
                name: "Unknown".into(),
                span: Span::new(0, 0),
            },
        }
    }

    fn parse_pattern(&mut self) -> Option<Pattern> {
        match self.peek() {
            Some(TokenKind::Identifier(_)) => {
                let name = self.expect_identifier()?;
                Some(Pattern::Identifier {
                    name,
                    span: Span::new(0, 0),
                })
            }
            _ => None,
        }
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_binary_op(0)
    }

    fn parse_binary_op(&mut self, precedence: u8) -> Option<Expr> {
        let mut left = self.parse_unary_op()?;

        loop {
            let op_prec = self.peek_precedence();
            if op_prec < precedence {
                break;
            }

            let op = match self.peek() {
                Some(TokenKind::Plus) => BinOp::Add,
                Some(TokenKind::Minus) => BinOp::Sub,
                Some(TokenKind::Star) => BinOp::Mul,
                Some(TokenKind::Slash) => BinOp::Div,
                Some(TokenKind::Percent) => BinOp::Mod,
                Some(TokenKind::EqEq) => BinOp::Eq,
                Some(TokenKind::Neq) => BinOp::Ne,
                Some(TokenKind::Lt) => BinOp::Lt,
                Some(TokenKind::Le) => BinOp::Le,
                Some(TokenKind::Gt) => BinOp::Gt,
                Some(TokenKind::Ge) => BinOp::Ge,
                Some(TokenKind::And) => BinOp::And,
                Some(TokenKind::Or) => BinOp::Or,
                Some(TokenKind::Pipe) => BinOp::Pipe,
                _ => break,
            };

            self.advance().ok()?;
            let right = self.parse_binary_op(op_prec + 1)?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span: Span::new(0, 0),
            };
        }

        Some(left)
    }

    fn peek_precedence(&self) -> u8 {
        match self.peek() {
            Some(TokenKind::Or) => 1,
            Some(TokenKind::And) => 2,
            Some(TokenKind::EqEq) | Some(TokenKind::Neq) => 3,
            Some(TokenKind::Lt) | Some(TokenKind::Le) | Some(TokenKind::Gt) | Some(TokenKind::Ge) => 4,
            Some(TokenKind::Plus) | Some(TokenKind::Minus) => 5,
            Some(TokenKind::Star) | Some(TokenKind::Slash) | Some(TokenKind::Percent) => 6,
            Some(TokenKind::Pipe) => 7,
            _ => 0,
        }
    }

    fn parse_unary_op(&mut self) -> Option<Expr> {
        if self.check(&TokenKind::Minus) {
            self.advance().ok()?;
            let operand = self.parse_unary_op()?;
            Some(Expr::UnaryOp {
                op: UnOp::Neg,
                operand: Box::new(operand),
                span: Span::new(0, 0),
            })
        } else if self.check(&TokenKind::Not) {
            self.advance().ok()?;
            let operand = self.parse_unary_op()?;
            Some(Expr::UnaryOp {
                op: UnOp::Not,
                operand: Box::new(operand),
                span: Span::new(0, 0),
            })
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        match self.peek() {
            Some(TokenKind::IntLiteral(value)) => {
                let value = *value;
                self.advance().ok()?;
                Some(Expr::IntLiteral { value, span: Span::new(0, 0) })
            }
            Some(TokenKind::FloatLiteral(value)) => {
                let value = *value;
                self.advance().ok()?;
                Some(Expr::FloatLiteral { value, span: Span::new(0, 0) })
            }
            Some(TokenKind::BoolLiteral(value)) => {
                let value = *value;
                self.advance().ok()?;
                Some(Expr::BoolLiteral { value, span: Span::new(0, 0) })
            }
            Some(TokenKind::CharLiteral(value)) => {
                let value = *value;
                self.advance().ok()?;
                Some(Expr::CharLiteral { value, span: Span::new(0, 0) })
            }
            Some(TokenKind::StringLiteral(value)) => {
                let value = value.clone();
                self.advance().ok()?;
                Some(Expr::StringLiteral { value, span: Span::new(0, 0) })
            }
            Some(TokenKind::Identifier(_)) => {
                let name = self.expect_identifier()?;
                Some(Expr::Identifier { name, span: Span::new(0, 0) })
            }
            Some(TokenKind::If) => self.parse_if(),
            Some(TokenKind::Do) => self.parse_block(),
            Some(TokenKind::LParen) => self.parse_tuple(),
            Some(TokenKind::LBracket) => self.parse_list(),
            Some(TokenKind::Loop) => self.parse_loop(),
            _ => None,
        }
    }
}
