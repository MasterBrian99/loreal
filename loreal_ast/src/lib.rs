pub mod span;

pub use span::Span;

use smol_str::SmolStr;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: SmolStr,
    pub exports: Vec<SmolStr>,
    pub imports: Vec<Import>,
    pub declarations: Vec<Declaration>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Pipe,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        pattern: Pattern,
        type_annotation: Option<Type>,
        value: Expr,
        span: Span,
    },
    Expr { expr: Expr, span: Span },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard { span: Span },
    Identifier { name: SmolStr, span: Span },
    Literal { expr: Expr, span: Span },
    Tuple { patterns: Vec<Pattern>, span: Span },
    Struct {
        name: SmolStr,
        fields: Vec<(SmolStr, Pattern)>,
        span: Span,
    },
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard { span } => *span,
            Pattern::Identifier { span, .. } => *span,
            Pattern::Literal { expr, .. } => expr.span(),
            Pattern::Tuple { span, .. } => *span,
            Pattern::Struct { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Named { name: SmolStr, span: Span },
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
        span: Span,
    },
    List { element_type: Box<Type>, span: Span },
    Tuple { types: Vec<Type>, span: Span },
    Map {
        key_type: Box<Type>,
        value_type: Box<Type>,
        span: Span,
    },
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Named { span, .. } => *span,
            Type::Function { span, .. } => *span,
            Type::List { span, .. } => *span,
            Type::Tuple { span, .. } => *span,
            Type::Map { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Function(FunctionDef),
    Struct(StructDef),
    Protocol(ProtocolDef),
    Impl(ImplDef),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef {
    pub name: SmolStr,
    pub params: Vec<(Pattern, Type)>,
    pub return_type: Type,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: SmolStr,
    pub fields: Vec<(SmolStr, Type)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolDef {
    pub name: SmolStr,
    pub methods: Vec<FunctionSignature>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub name: SmolStr,
    pub params: Vec<(SmolStr, Type)>,
    pub return_type: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDef {
    pub protocol: SmolStr,
    pub target_type: SmolStr,
    pub methods: Vec<FunctionDef>,
    pub span: Span,
}
