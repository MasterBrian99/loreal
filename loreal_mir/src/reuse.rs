use crate::{BasicBlock, Instruction, Type};
use petgraph::graph::{Graph, NodeIndex};
use smol_str::SmolStr;
use std::collections::HashMap;

pub struct ReuseAnalyzer {
    pub reuse_opportunities: HashMap<SmolStr, ReuseInfo>,
    type_info: HashMap<SmolStr, Type>,
}

#[derive(Debug, Clone)]
pub struct ReuseInfo {
    pub old_var: SmolStr,
    pub drop_point: usize,
    pub alloc_point: usize,
    pub type_compatible: bool,
    pub new_type: Type,
}

impl ReuseAnalyzer {
    pub fn new() -> Self {
        Self {
            reuse_opportunities: HashMap::new(),
            type_info: HashMap::new(),
        }
    }

    pub fn analyze(
        &mut self,
        cfg: &Graph<BasicBlock, crate::ControlFlow>,
        type_info: HashMap<SmolStr, Type>,
    ) {
        self.type_info = type_info;

        for node_idx in cfg.node_indices() {
            if let Some(block) = cfg.node_weight(node_idx) {
                self.analyze_block(node_idx, block);
            }
        }
    }

    fn analyze_block(&mut self, _node_idx: NodeIndex, block: &BasicBlock) {
        for (idx, instr) in block.instructions.iter().enumerate() {
            if let Instruction::Alloc { target, ty } = instr {
                let new_type = ty.clone();
                let type_compatible = true;

                let reuse_info = ReuseInfo {
                    old_var: SmolStr::new(""),
                    drop_point: 0,
                    alloc_point: idx,
                    type_compatible,
                    new_type,
                };

                self.reuse_opportunities.insert(target.clone(), reuse_info);
            }
        }
    }

    pub fn apply_reuse(&self, block: &BasicBlock) -> Vec<Instruction> {
        let mut new_instructions = Vec::new();

        for instr in &block.instructions {
            if let Instruction::Alloc { target, ty } = instr {
                if let Some(reuse_info) = self.reuse_opportunities.get(target) {
                    if reuse_info.type_compatible {
                        new_instructions.push(Instruction::Reuse {
                            old: reuse_info.old_var.clone(),
                            new: target.clone(),
                        });
                    } else {
                        new_instructions.push(instr.clone());
                    }
                } else {
                    new_instructions.push(instr.clone());
                }
            } else {
                new_instructions.push(instr.clone());
            }
        }

        new_instructions
    }
}

impl Default for ReuseAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}
