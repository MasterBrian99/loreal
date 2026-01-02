use crate::{BasicBlock, ControlFlow, Instruction, MirFunction, Type, Value};
use petgraph::graph::{Graph, NodeIndex};
use smol_str::SmolStr;
use std::collections::HashMap;

pub struct RcInserter {
    types: HashMap<SmolStr, Type>,
}

impl RcInserter {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    pub fn insert_rc_ops(&mut self, func: &mut MirFunction) {
        for block in func.cfg.node_weights_mut() {
            self.insert_rc_ops_in_block(block);
        }
    }

    fn insert_rc_ops_in_block(&mut self, block: &mut BasicBlock) {
        let mut new_instructions = Vec::new();

        for instr in &block.instructions {
            match instr {
                Instruction::Call { func, args, target } => {
                    for arg in args {
                        self.insert_inc_for_value(arg, &mut new_instructions);
                    }

                    new_instructions.push(instr.clone());
                }
                Instruction::Assign { target, value } => {
                    self.insert_dec_if_needed(value, &mut new_instructions);
                    new_instructions.push(instr.clone());
                }
                _ => {
                    new_instructions.push(instr.clone());
                }
            }
        }

        block.instructions = new_instructions;
    }

    fn insert_inc_for_value(&self, val: &Value, instrs: &mut Vec<Instruction>) {
        if let Value::Var(name) = val {
            if let Some(ty) = self.types.get(name) {
                if self.is_rc_type(ty) {
                    instrs.push(Instruction::Inc { var: name.clone() });
                }
            }
        }
    }

    fn insert_dec_if_needed(&self, val: &Value, instrs: &mut Vec<Instruction>) {
        if let Value::Var(name) = val {
            if let Some(ty) = self.types.get(name) {
                if self.is_rc_type(ty) {
                    instrs.push(Instruction::Dec { var: name.clone() });
                }
            }
        }
    }

    fn is_rc_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Named { name, .. } => {
                name.as_str() != "Int"
                    && name.as_str() != "Float"
                    && name.as_str() != "Bool"
                    && name.as_str() != "Char"
            }
            Type::List { .. } => true,
            Type::Tuple { .. } => true,
            Type::Map { .. } => true,
            Type::Function { .. } => false,
        }
    }
}
