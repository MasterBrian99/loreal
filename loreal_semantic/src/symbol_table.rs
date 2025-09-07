use crate::types::ConcreteType;
use crate::error::SemanticError;
use loreal_ast::Span;
use smol_str::SmolStr;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: SmolStr,
    pub ty: ConcreteType,
    pub span: Span,
}

pub struct SymbolTable {
    scopes: Vec<HashMap<SmolStr, SymbolInfo>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: SmolStr, info: SymbolInfo) -> Result<(), SemanticError> {
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(existing) = scope.get(&name) {
                return Err(SemanticError::DuplicateDefinition {
                    name,
                    original: existing.span,
                    duplicate: info.span,
                });
            }
            scope.insert(name, info);
        }
        Ok(())
    }

    pub fn lookup(&self, name: &SmolStr) -> Option<&SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
