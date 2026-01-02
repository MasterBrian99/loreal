use loreal_ast::span::Span;
use smol_str::SmolStr;
use std::iter::Peekable;
use std::str::CharIndices;
use thiserror::Error;

mod utils;
pub use utils::{escape_string, is_keyword};

pub mod token;
pub use token::{Token, TokenKind};

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
    /// Create a new lexer for the given source code
    pub fn new(source: &'source str) -> Self {
        Self {
            _source: source,
            chars: source.char_indices().peekable(),
            current_pos: 0,
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, ch)| *ch)
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        if let Some((pos, ch)) = self.chars.next() {
            self.current_pos = pos + ch.len_utf8();
            Some((pos, ch))
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_line_comment(&mut self) {
        // Skip until newline or EOF
        while let Some(ch) = self.peek_char() {
            self.advance();
            if ch == '\n' {
                break;
            }
        }
    }

    fn skip_block_comment(&mut self, start: usize) -> Result<(), LexError> {
        // Look for the closing """

        let mut quote_count = 0;
        while let Some((_, ch)) = self.advance() {
            if ch == '"' {
                quote_count += 1;
                if quote_count == 3 {
                    return Ok(());
                }
            } else {
                quote_count = 0;
            }
        }

        Err(LexError::UnterminatedBlockComment { start })
    }

    fn scan_identifier(&mut self, start: usize, first_char: char) -> Token {
        let mut ident = String::from(first_char);

        while let Some(ch) = self.peek_char() {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
                ident.push(ch);
            } else {
                break;
            }
        }

        let span = Span::new(start, self.current_pos);

        let kind = match ident.as_str() {
            "module" => TokenKind::Module,
            "def" => TokenKind::Def,
            "do" => TokenKind::Do,
            "end" => TokenKind::End,
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "match" => TokenKind::Match,
            "import" => TokenKind::Import,
            "export" => TokenKind::Export,
            "protocol" => TokenKind::Protocol,
            "impl" => TokenKind::Impl,
            "struct" => TokenKind::Struct,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "nil" => TokenKind::Nil,
            "fn" => TokenKind::Fn,
            "loop" => TokenKind::Loop,
            "next" => TokenKind::Next,
            "break" => TokenKind::Break,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            _ => TokenKind::Identifier(SmolStr::from(ident)),
        };

        Token::new(kind, span)
    }

    /// Supports: 123, -456, 3.14, -2.5
    fn scan_number(&mut self, start: usize, first_char: char) -> Result<Token, LexError> {
        let mut num_str = String::from(first_char);
        let mut is_float = false;

        // Scan digits
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                self.advance();
                num_str.push(ch);
            } else if ch == '.' && !is_float {
                self.advance();
                if let Some(next_ch) = self.peek_char() {
                    if next_ch.is_ascii_digit() {
                        is_float = true;
                        num_str.push('.');
                    } else {
                        // Back up by putting the dot back
                        self.current_pos -= 1;
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let span = Span::new(start, self.current_pos);

        if is_float {
            num_str
                .parse::<f64>()
                .map(|val| Token::new(TokenKind::FloatLiteral(val), span))
                .map_err(|_| LexError::InvalidNumber {
                    text: num_str,
                    pos: start,
                })
        } else {
            num_str
                .parse::<i64>()
                .map(|val| Token::new(TokenKind::IntLiteral(val), span))
                .map_err(|_| LexError::InvalidNumber {
                    text: num_str,
                    pos: start,
                })
        }
    }

    /// Supports: \"hello\", \"line1\nline2\", \"quote: \\"\"
    fn scan_string(&mut self, start: usize) -> Result<Token, LexError> {
        let mut value = String::new();

        while let Some((pos, ch)) = self.advance() {
            match ch {
                '"' => {
                    let span = Span::new(start, self.current_pos);
                    return Ok(Token::new(
                        TokenKind::StringLiteral(SmolStr::from(value)),
                        span,
                    ));
                }
                '\\' => {
                    if let Some((_, escaped)) = self.advance() {
                        match escaped {
                            'n' => value.push('\n'),
                            'r' => value.push('\r'),
                            't' => value.push('\t'),
                            '\\' => value.push('\\'),
                            '"' => value.push('"'),
                            _ => {
                                return Err(LexError::InvalidEscapeSequence {
                                    seq: escaped.to_string(),
                                    pos,
                                });
                            }
                        }
                    } else {
                        return Err(LexError::UnterminatedString { start });
                    }
                }
                _ => value.push(ch),
            }
        }

        Err(LexError::UnterminatedString { start })
    }

    /// Supports: 'a', '\n', '\t'
    fn scan_char(&mut self, start: usize) -> Result<Token, LexError> {
        if let Some((pos, ch)) = self.advance() {
            let value = if ch == '\\' {
                // Escape sequence
                if let Some((_, escaped)) = self.advance() {
                    match escaped {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '\'' => '\'',
                        _ => {
                            return Err(LexError::InvalidEscapeSequence {
                                seq: escaped.to_string(),
                                pos,
                            });
                        }
                    }
                } else {
                    return Err(LexError::UnterminatedCharLiteral { pos: start });
                }
            } else {
                ch
            };

            // Expect closing quote
            if let Some((_, closing)) = self.advance() {
                if closing == '\'' {
                    let span = Span::new(start, self.current_pos);
                    return Ok(Token::new(TokenKind::CharLiteral(value), span));
                }
            }
        }

        Err(LexError::UnterminatedCharLiteral { pos: start })
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        // Skip whitespace and comments
        loop {
            self.skip_whitespace();

            // Check for comments
            if self.peek_char() == Some('#') {
                self.skip_line_comment();
                continue;
            }

            if self.peek_char() == Some('"') {
                let start = self.current_pos;
                self.advance(); // First quote
                if self.peek_char() == Some('"') {
                    self.advance(); // Second quote
                    if self.peek_char() == Some('"') {
                        self.advance(); // Third quote
                                        // This is a block comment
                        self.skip_block_comment(start)?;
                        continue;
                    } else {
                        // Two quotes - empty string
                        let span = Span::new(start, self.current_pos);
                        return Ok(Token::new(
                            TokenKind::StringLiteral(SmolStr::default()),
                            span,
                        ));
                    }
                } else {
                    return self.scan_string(start);
                }
            }

            break;
        }

        // Get the next character
        if let Some((start, ch)) = self.advance() {
            let token = match ch {
                // Single-character tokens
                '(' => Token::new(TokenKind::LParen, Span::new(start, self.current_pos)),
                ')' => Token::new(TokenKind::RParen, Span::new(start, self.current_pos)),
                '[' => Token::new(TokenKind::LBracket, Span::new(start, self.current_pos)),
                ']' => Token::new(TokenKind::RBracket, Span::new(start, self.current_pos)),
                '{' => Token::new(TokenKind::LBrace, Span::new(start, self.current_pos)),
                '}' => Token::new(TokenKind::RBrace, Span::new(start, self.current_pos)),
                ',' => Token::new(TokenKind::Comma, Span::new(start, self.current_pos)),
                ':' => Token::new(TokenKind::Colon, Span::new(start, self.current_pos)),
                ';' => Token::new(TokenKind::Semicolon, Span::new(start, self.current_pos)),
                '.' => Token::new(TokenKind::Dot, Span::new(start, self.current_pos)),
                '+' => Token::new(TokenKind::Plus, Span::new(start, self.current_pos)),
                '*' => Token::new(TokenKind::Star, Span::new(start, self.current_pos)),
                '/' => Token::new(TokenKind::Slash, Span::new(start, self.current_pos)),
                '%' => Token::new(TokenKind::Percent, Span::new(start, self.current_pos)),

                // Multi-character operators
                '-' => {
                    if self.peek_char() == Some('>') {
                        self.advance();
                        Token::new(TokenKind::Arrow, Span::new(start, self.current_pos))
                    } else {
                        Token::new(TokenKind::Minus, Span::new(start, self.current_pos))
                    }
                }

                '=' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::EqEq, Span::new(start, self.current_pos))
                    } else if self.peek_char() == Some('>') {
                        self.advance();
                        Token::new(TokenKind::FatArrow, Span::new(start, self.current_pos))
                    } else {
                        Token::new(TokenKind::Eq, Span::new(start, self.current_pos))
                    }
                }

                '!' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::Ne, Span::new(start, self.current_pos))
                    } else {
                        Token::new(TokenKind::Not, Span::new(start, self.current_pos))
                    }
                }

                '<' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::Le, Span::new(start, self.current_pos))
                    } else {
                        Token::new(TokenKind::Lt, Span::new(start, self.current_pos))
                    }
                }

                '>' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::Ge, Span::new(start, self.current_pos))
                    } else {
                        Token::new(TokenKind::Gt, Span::new(start, self.current_pos))
                    }
                }

                '|' => {
                    if self.peek_char() == Some('>') {
                        self.advance();
                        Token::new(TokenKind::Pipe, Span::new(start, self.current_pos))
                    } else {
                        Token::new(TokenKind::Bar, Span::new(start, self.current_pos))
                    }
                }

                '"' => return self.scan_string(start),

                '\'' => return self.scan_char(start),

                _ if ch.is_alphabetic() || ch == '_' => self.scan_identifier(start, ch),

                _ if ch.is_ascii_digit() => {
                    return self.scan_number(start, ch);
                }

                _ => {
                    return Err(LexError::UnexpectedCharacter { ch, pos: start });
                }
            };

            Ok(token)
        } else {
            // End of file
            Ok(Token::new(
                TokenKind::Eof,
                Span::new(self.current_pos, self.current_pos),
            ))
        }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token) if token.kind == TokenKind::Eof => None,
            result => Some(result),
        }
    }
}
