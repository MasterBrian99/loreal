
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
