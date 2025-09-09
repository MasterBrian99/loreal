use crate::error::SemanticError;
use crate::types::ConcreteType;
use crate::symbol_table::{SymbolTable, SymbolInfo};
use loreal_ast::*;
use smol_str::SmolStr;
use std::collections::HashMap;

pub struct TypeChecker {
    symbol_table: SymbolTable,
    errors: Vec<SemanticError>,
    pub struct_types: HashMap<SmolStr, StructDef>,
    pub variable_types: HashMap<(SmolStr, SmolStr), ConcreteType>,
    pub current_func: Option<SmolStr>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
            struct_types: HashMap::new(),
            variable_types: HashMap::new(),
            current_func: None,
        }
    }

    pub fn check_module(&mut self, module: &Module) -> Result<(), Vec<SemanticError>> {
        for decl in &module.declarations {
            if let Declaration::Struct(struct_def) = decl {
                self.struct_types
                    .insert(struct_def.name.clone(), struct_def.clone());
            }
        }

        for decl in &module.declarations {
            if let Declaration::Function(func) = decl {
                let param_types: Vec<ConcreteType> = func
                    .params
                    .iter()
                    .map(|(_, ty)| ConcreteType::from_ast_type(ty))
                    .collect();

                let return_type = ConcreteType::from_ast_type(&func.return_type);

                let func_type = ConcreteType::Function {
                    params: param_types,
                    return_type: Box::new(return_type),
                };

                if let Err(e) = self.symbol_table.insert(
                    func.name.clone(),
                    SymbolInfo {
                        name: func.name.clone(),
                        ty: func_type,
                        span: func.span,
                    },
                ) {
                    self.errors.push(e);
                }
            }
        }

        for decl in &module.declarations {
            if let Declaration::Function(func) = decl {
                self.check_function(func);
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
