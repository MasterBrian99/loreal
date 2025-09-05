use loreal_ast::*;
use smol_str::SmolStr;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum SemanticError {
    #[error("Undefined variable '{name}' at {span:?}")]
    UndefinedVariable { name: SmolStr, span: Span },

    #[error("Undefined function '{name}' at {span:?}")]
    UndefinedFunction { name: SmolStr, span: Span },

    #[error("Type mismatch: expected {expected}, found {found} at {span:?}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    #[error(
        "Duplicate definition of '{name}' at {duplicate:?}, originally defined at {original:?}"
    )]
    DuplicateDefinition {
        name: SmolStr,
        original: Span,
        duplicate: Span,
    },

    #[error("Cannot infer type for expression at {span:?}")]
    CannotInferType { span: Span },

    #[error("Invalid binary operation: {op:?} on types {left} and {right} at {span:?}")]
    InvalidBinaryOp {
        op: BinOp,
        left: String,
        right: String,
        span: Span,
    },

    #[error("Undefined field '{field}' on type '{ty}' at {span:?}")]
    UndefinedField {
        field: SmolStr,
        ty: String,
        span: Span,
    },

    #[error("Undefined type '{name}' at {span:?}")]
    UndefinedType { name: SmolStr, span: Span },
}
