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
pub struct Import {
    pub module_path: SmolStr,
    pub items: Option<Vec<SmolStr>>,
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
