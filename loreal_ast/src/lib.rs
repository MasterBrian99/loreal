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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    IntLiteral { value: i64, span: Span },
    FloatLiteral { value: f64, span: Span },
    BoolLiteral { value: bool, span: Span },
    CharLiteral { value: char, span: Span },
    StringLiteral { value: SmolStr, span: Span },
    NilLiteral { span: Span },
    Identifier { name: SmolStr, span: Span },
    FunctionCall { func: Box<Expr>, args: Vec<Expr>, span: Span },
    Lambda {
        params: Vec<Pattern>,
        return_type: Option<Type>,
        body: Box<Expr>,
        span: Span,
    },
    BinaryOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    UnaryOp { op: UnOp, operand: Box<Expr>, span: Span },
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
        span: Span,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    List { elements: Vec<Expr>, span: Span },
    Tuple { elements: Vec<Expr>, span: Span },
    Map { pairs: Vec<(Expr, Expr)>, span: Span },
    StructLiteral {
        name: SmolStr,
        fields: Vec<(SmolStr, Expr)>,
        span: Span,
    },
    FieldAccess { object: Box<Expr>, field: SmolStr, span: Span },
    IndexAccess { object: Box<Expr>, index: Box<Expr>, span: Span },
    Pipe { left: Box<Expr>, right: Box<Expr>, span: Span },
    Block {
        statements: Vec<Statement>,
        result: Box<Expr>,
        span: Span,
    },
    Loop {
        init_values: Vec<Expr>,
        loop_vars: Vec<SmolStr>,
        body: Box<Expr>,
        span: Span,
    },
    Next { values: Vec<Expr>, span: Span },
    Break { value: Box<Expr>, span: Span },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::IntLiteral { span, .. } => *span,
            Expr::FloatLiteral { span, .. } => *span,
            Expr::BoolLiteral { span, .. } => *span,
            Expr::CharLiteral { span, .. } => *span,
            Expr::StringLiteral { span, .. } => *span,
            Expr::NilLiteral { span } => *span,
            Expr::Identifier { span, .. } => *span,
            Expr::FunctionCall { span, .. } => *span,
            Expr::Lambda { span, .. } => *span,
            Expr::BinaryOp { span, .. } => *span,
            Expr::UnaryOp { span, .. } => *span,
            Expr::If { span, .. } => *span,
            Expr::Match { span, .. } => *span,
            Expr::List { span, .. } => *span,
            Expr::Tuple { span, .. } => *span,
            Expr::Map { span, .. } => *span,
            Expr::StructLiteral { span, .. } => *span,
            Expr::FieldAccess { span, .. } => *span,
            Expr::IndexAccess { span, .. } => *span,
            Expr::Pipe { span, .. } => *span,
            Expr::Block { span, .. } => *span,
            Expr::Loop { span, .. } => *span,
            Expr::Next { span, .. } => *span,
            Expr::Break { span, .. } => *span,
        }
    }
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
    Expr {
        expr: Expr,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard {
        span: Span,
    },
    Identifier {
        name: SmolStr,
        span: Span,
    },
    Literal {
        expr: Expr,
        span: Span,
    },
    Tuple {
        patterns: Vec<Pattern>,
        span: Span,
    },
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
    Named {
        name: SmolStr,
        span: Span,
    },
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
        span: Span,
    },
    List {
        element_type: Box<Type>,
        span: Span,
    },
    Tuple {
        types: Vec<Type>,
        span: Span,
    },
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
