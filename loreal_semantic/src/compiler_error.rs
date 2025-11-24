use loreal_ast::Span;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum CompilerError {
    #[error("Lexical error: {0}")]
    Lexical(String),

    #[error("Parse error: {0}")]
    Parse(String),

    #[error("Type error: {0}")]
    Type(String),

    #[error("Semantic error: {0}")]
    Semantic(String),

    #[error("Code generation error: {0}")]
    Codegen(String),

    #[error("IO error at {location:?}: {source}")]
    Io { source: String, location: Span },
}
