use loreal_ast::StructDef;
use loreal_mir::MirFunction;
use smol_str::SmolStr;

pub struct CodeGenerator {
    pub module_name: SmolStr,
    pub structs: Vec<StructDef>,
}

impl CodeGenerator {
    pub fn new(module_name: &str, structs: Vec<StructDef>) -> Self {
        Self {
            module_name: module_name.into(),
            structs,
        }
    }
}
