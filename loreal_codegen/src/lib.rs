use loreal_ast::StructDef;
use loreal_mir::MirFunction;
use smol_str::SmolStr;
use thiserror::Error;
use std::fmt;

pub struct CodeGenerator {
    pub module_name: SmolStr,
    pub structs: Vec<StructDef>,
}

#[derive(Debug)]
pub enum CodegenError {
    GenerationError(String),
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CodegenError::GenerationError(msg) => write!(f, "Failed to generate code: {}", msg),
        }
    }
}

impl std::error::Error for CodegenError {}

impl CodeGenerator {
    pub fn new(module_name: &str, structs: Vec<StructDef>) -> Self {
        Self {
            module_name: module_name.into(),
            structs,
        }
    }

    pub fn emit_obj(&self, functions: &[MirFunction], output: &str) -> Result<(), CodegenError> {
        Err(CodegenError::GenerationError("Not implemented".into()))
    }

    pub fn emit_llvm_ir(
        &self,
        functions: &[MirFunction],
        output: &str,
    ) -> Result<(), CodegenError> {
        Err(CodegenError::GenerationError("Not implemented".into()))
    }
}
