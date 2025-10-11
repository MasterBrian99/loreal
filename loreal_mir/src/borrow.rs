use crate::{Instruction, MirFunction};
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ownership {
    Owned,
    Borrowed,
}

pub struct BorrowInferencer {
    pub param_ownership: HashMap<SmolStr, HashMap<SmolStr, Ownership>>,
}

impl BorrowInferencer {
    pub fn new() -> Self {
        Self {
            param_ownership: HashMap::new(),
        }
    }

    pub fn analyze(&mut self, func: &MirFunction) {
        let mut param_usage: HashMap<SmolStr, bool> = HashMap::new();

        for (param_name, _) in &func.params {
            param_usage.insert(param_name.clone(), false);
        }

        for block in func.cfg.node_weights() {
            for instr in &block.instructions {
                match instr {
                    Instruction::Assign { value, .. } => {
                        self.check_value(value, &mut param_usage);
                    }
                    Instruction::BinaryOp { left, right, .. } => {
                        self.check_value(left, &mut param_usage);
                        self.check_value(right, &mut param_usage);
                    }
                    Instruction::UnaryOp { operand, .. } => {
                        self.check_value(operand, &mut param_usage);
                    }
                    Instruction::Call { args, .. } => {
                        for arg in args {
                            self.check_value(arg, &mut param_usage);
                        }
                    }
                    Instruction::FieldLoad { object, .. } => {
                        self.check_value(object, &mut param_usage);
                    }
                    Instruction::IndexLoad { object, index, .. } => {
                        self.check_value(object, &mut param_usage);
                        self.check_value(index, &mut param_usage);
                    }
                    _ => {}
                }
            }
        }

        let mut func_ownership = HashMap::new();
        for (param_name, is_used) in &param_usage {
            let ownership = if *is_used { Ownership::Borrowed } else { Ownership::Owned };
            func_ownership.insert(param_name.clone(), ownership);
        }
        self.param_ownership.insert(func.name.clone(), func_ownership);
    }

    fn check_value(&self, val: &crate::Value, param_usage: &mut HashMap<SmolStr, bool>) {
        if let crate::Value::Var(name) = val {
            if param_usage.contains_key(name) {
                param_usage.insert(name.clone(), true);
            }
        }
    }
}

impl Default for BorrowInferencer {
    fn default() -> Self {
        Self::new()
    }
}
