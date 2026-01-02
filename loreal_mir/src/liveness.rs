use crate::{BasicBlock, ControlFlow, Instruction, Value};
use petgraph::graph::{Graph, NodeIndex};
use petgraph::Direction;
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};

pub struct LivenessAnalyzer {
    pub live_in: HashMap<NodeIndex, HashSet<SmolStr>>,
    pub live_out: HashMap<NodeIndex, HashSet<SmolStr>>,
    pub kill_points: HashMap<NodeIndex, HashMap<SmolStr, usize>>,
}

impl LivenessAnalyzer {
    pub fn new() -> Self {
        Self {
            live_in: HashMap::new(),
            live_out: HashMap::new(),
            kill_points: HashMap::new(),
        }
    }

    pub fn analyze(&mut self, cfg: &Graph<BasicBlock, ControlFlow>) -> bool {
        for node in cfg.node_indices() {
            self.live_in.insert(node, HashSet::new());
            self.live_out.insert(node, HashSet::new());
        }

        let mut changed = true;
        let mut iterations = 0;
        const MAX_ITERATIONS: usize = 100;

        while changed && iterations < MAX_ITERATIONS {
            changed = false;
            iterations += 1;

            let nodes: Vec<NodeIndex> = cfg.node_indices().rev().collect();

            for node_idx in nodes {
                let old_live_in = self.live_in[&node_idx].clone();

                let mut live_out = HashSet::new();
                for successor in cfg.neighbors_directed(node_idx, Direction::Outgoing) {
                    live_out.extend(self.live_in[&successor].clone());
                }
                self.live_out.insert(node_idx, live_out.clone());

                if let Some(block) = cfg.node_weight(node_idx) {
                    let (use_set, def_set) = self.compute_use_def(block);

                    let mut live_in = use_set.clone();
                    for var in &live_out {
                        if !def_set.contains(var) {
                            live_in.insert(var.clone());
                        }
                    }

                    if live_in != old_live_in {
                        changed = true;
                        self.live_in.insert(node_idx, live_in);
                    }

                    self.compute_kill_points(node_idx, block, &live_out);
                }
            }
        }

        changed
    }

    pub fn insert_drops(&mut self, cfg: &Graph<BasicBlock, crate::ControlFlow>) -> HashMap<NodeIndex, Vec<Instruction>> {
        let mut modified_blocks = HashMap::new();

        for (node, kill_points) in &self.kill_points {
            if let Some(block) = cfg.node_weight(*node) {
                let mut new_instructions = block.instructions.clone();

                for (idx, instr) in block.instructions.iter().enumerate() {
                    if let Some(var) = instr_to_var(instr) {
                        if let Some(kill_idx) = kill_points.get(&var) {
                            if *kill_idx == idx {
                                new_instructions.insert(
                                    idx + 1,
                                    Instruction::Dec {
                                        var: var_from_instr(instr),
                                    },
                                );
                            }
                        }
                    }
                }

                modified_blocks.insert(*node, new_instructions);
            }
        }

        modified_blocks
    }

    fn compute_use_def(&self, block: &BasicBlock) -> (HashSet<SmolStr>, HashSet<SmolStr>) {
        let mut use_set = HashSet::new();
        let mut def_set = HashSet::new();

        for instr in &block.instructions {
            match instr {
                Instruction::Assign { target, value } => {
                    def_set.insert(target.clone());
                    self.collect_vars_from_value(value, &mut use_set);
                }
                Instruction::BinaryOp { target, left, right, .. } => {
                    def_set.insert(target.clone());
                    self.collect_vars_from_value(left, &mut use_set);
                    self.collect_vars_from_value(right, &mut use_set);
                }
                Instruction::UnaryOp { target, operand, .. } => {
                    def_set.insert(target.clone());
                    self.collect_vars_from_value(operand, &mut use_set);
                }
                Instruction::FieldLoad { target, object, .. } => {
                    def_set.insert(target.clone());
                    self.collect_vars_from_value(object, &mut use_set);
                }
                Instruction::IndexLoad { target, object, index, .. } => {
                    def_set.insert(target.clone());
                    self.collect_vars_from_value(object, &mut use_set);
                    self.collect_vars_from_value(index, &mut use_set);
                }
                Instruction::Call { target, func, args } => {
                    if let Some(t) = target {
                        def_set.insert(t.clone());
                    }
                    self.collect_vars_from_value(&Value::Var(func.clone()), &mut use_set);
                    for arg in args {
                        self.collect_vars_from_value(arg, &mut use_set);
                    }
                }
                Instruction::Alloc { .. } | Instruction::FieldStore { .. } | Instruction::IndexStore { .. } | Instruction::AllocList { .. } => {}
                Instruction::Dec { .. } | Instruction::Inc { .. } | Instruction::Drop { .. } | Instruction::Dup { .. } | Instruction::Reuse { .. } => {}
            }
        }

        (use_set, def_set)
    }

    fn compute_kill_points(&mut self, node_idx: NodeIndex, block: &BasicBlock, live_out: &HashSet<SmolStr>) {
        let mut kill_map = HashMap::new();

        for (idx, instr) in block.instructions.iter().enumerate() {
            if let Some(var) = instr_to_var(instr) {
                if live_out.contains(&var) {
                    kill_map.insert(var, idx);
                }
            }
        }

        self.kill_points.insert(node_idx, kill_map);
    }

    fn collect_vars_from_value(&self, value: &Value, use_set: &mut HashSet<SmolStr>) {
        match value {
            Value::Var(name) => {
                use_set.insert(name.clone());
            }
            _ => {}
        }
    }
}

fn instr_to_var(instr: &Instruction) -> Option<SmolStr> {
    match instr {
        Instruction::Assign { target, .. } => Some(target.clone()),
        Instruction::BinaryOp { target, .. } => Some(target.clone()),
        Instruction::UnaryOp { target, .. } => Some(target.clone()),
        Instruction::FieldLoad { target, .. } => Some(target.clone()),
        Instruction::IndexLoad { target, .. } => Some(target.clone()),
        Instruction::Call { target: Some(t), .. } => Some(t.clone()),
        _ => None,
    }
}

fn var_from_instr(instr: &Instruction) -> SmolStr {
    match instr {
        Instruction::Assign { target, .. } => target.clone(),
        Instruction::BinaryOp { target, .. } => target.clone(),
        Instruction::UnaryOp { target, .. } => target.clone(),
        Instruction::FieldLoad { target, .. } => target.clone(),
        Instruction::IndexLoad { target, .. } => target.clone(),
        Instruction::Call { target: Some(t), .. } => t.clone(),
        _ => SmolStr::new(""),
    }
}

impl Default for LivenessAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}
