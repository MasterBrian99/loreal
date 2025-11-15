use loreal_ast::StructDef;
use loreal_mir::MirFunction;
use smol_str::SmolStr;

pub struct CodeGenerator {
    pub module_name: SmolStr,
    pub structs: Vec<StructDef>,
}

#[derive(Debug)]
pub enum CodegenError {
    #[error("Failed to generate code for function: {0}")]
    GenerationError(String),
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
