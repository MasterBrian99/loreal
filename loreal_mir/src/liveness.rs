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

    fn compute_use_def(&self, block: &BasicBlock) -> (HashSet<SmolStr>, HashSet<SmolStr>) {
        let mut use_set = HashSet::new();
        let mut def_set = HashSet::new();

        for instr in &block.instructions {
            match instr {
                Instruction::Assign { value, .. } => {
                    self.collect_variables(value, &mut use_set, &mut def_set);
                }
                Instruction::BinaryOp { left, right, target, .. } => {
                    self.collect_variables(left, &mut use_set, &mut def_set);
                    self.collect_variables(right, &mut use_set, &mut def_set);
                    def_set.insert(target.clone());
                }
                Instruction::UnaryOp { operand, target, .. } => {
                    self.collect_variables(operand, &mut use_set, &mut def_set);
                    def_set.insert(target.clone());
                }
                Instruction::Call { args, target, .. } => {
                    for arg in args {
                        self.collect_variables(arg, &mut use_set, &mut def_set);
                    }
                    if let Some(t) = target {
                        def_set.insert(t.clone());
                    }
                }
                Instruction::FieldLoad { object, target, .. } => {
                    self.collect_variables(object, &mut use_set, &mut def_set);
                    def_set.insert(target.clone());
                }
                Instruction::FieldStore { target, value, field: _ } => {
                    use_set.insert(target.clone());
                    self.collect_variables(value, &mut use_set, &mut def_set);
                }
                Instruction::IndexLoad { object, index, target, .. } => {
                    self.collect_variables(object, &mut use_set, &mut def_set);
                    self.collect_variables(index, &mut use_set, &mut def_set);
                    def_set.insert(target.clone());
                }
                Instruction::IndexStore { target, index, value, .. } => {
                    use_set.insert(target.clone());
                    self.collect_variables(index, &mut use_set, &mut def_set);
                    self.collect_variables(value, &mut use_set, &mut def_set);
                }
                _ => {}
            }
        }

        (use_set, def_set)
    }

    fn collect_variables(&self, val: &Value, use_set: &mut HashSet<SmolStr>, def_set: &mut HashSet<SmolStr>) {
        match val {
            Value::Var(name) if !def_set.contains(name) => {
                use_set.insert(name.clone());
            }
            _ => {}
        }
    }

    fn compute_kill_points(&mut self, node: NodeIndex, block: &BasicBlock, live_out: &HashSet<SmolStr>) {
        let mut kill_points = HashMap::new();
        let mut live = live_out.clone();

        for (idx, instr) in block.instructions.iter().enumerate().rev() {
            match instr {
                Instruction::Assign { target, value } => {
                    self.collect_variables(value, &mut live, &mut HashSet::new());
                    if !live.contains(target) {
                        kill_points.insert(target.clone(), idx);
                    }
                    def_insert(target, &mut live);
                }
                Instruction::BinaryOp { target, left, right, .. } => {
                    self.collect_variables(left, &mut live, &mut HashSet::new());
                    self.collect_variables(right, &mut live, &mut HashSet::new());
                    if !live.contains(target) {
                        kill_points.insert(target.clone(), idx);
                    }
                    def_insert(target, &mut live);
                }
                Instruction::UnaryOp { target, operand, .. } => {
                    self.collect_variables(operand, &mut live, &mut HashSet::new());
                    if !live.contains(target) {
                        kill_points.insert(target.clone(), idx);
                    }
                    def_insert(target, &mut live);
                }
                Instruction::Call { target, args, .. } => {
                    for arg in args {
                        self.collect_variables(arg, &mut live, &mut HashSet::new());
                    }
                    if let Some(t) = target {
                        if !live.contains(t) {
                            kill_points.insert(t.clone(), idx);
                        }
                        def_insert(t, &mut live);
                    }
                }
                Instruction::FieldLoad { target, object, .. } => {
                    self.collect_variables(object, &mut live, &mut HashSet::new());
                    if !live.contains(target) {
                        kill_points.insert(target.clone(), idx);
                    }
                    def_insert(target, &mut live);
                }
                Instruction::FieldStore { target, value, .. } => {
                    use_insert(target, &mut live);
                    self.collect_variables(value, &mut live, &mut HashSet::new());
                }
                Instruction::IndexLoad { target, object, index, .. } => {
                    self.collect_variables(object, &mut live, &mut HashSet::new());
                    self.collect_variables(index, &mut live, &mut HashSet::new());
                    if !live.contains(target) {
                        kill_points.insert(target.clone(), idx);
                    }
                    def_insert(target, &mut live);
                }
                Instruction::IndexStore { target, index, value, .. } => {
                    use_insert(target, &mut live);
                    self.collect_variables(index, &mut live, &mut HashSet::new());
                    self.collect_variables(value, &mut live, &mut HashSet::new());
                }
                _ => {}
            }
        }

        self.kill_points.insert(node, kill_points);
    }
}

fn use_insert(name: &SmolStr, set: &mut HashSet<SmolStr>) {
    set.insert(name.clone());
}

fn def_insert(name: &SmolStr, set: &mut HashSet<SmolStr>) {
    set.insert(name.clone());
}

impl Default for LivenessAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}
