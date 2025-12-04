use crate::{BasicBlock, ControlFlow};
use petgraph::graph::{Graph, NodeIndex};

pub fn get_predecessors(cfg: &Graph<BasicBlock, ControlFlow>, node: NodeIndex) -> Vec<NodeIndex> {
    cfg.neighbors_directed(node, petgraph::Direction::Incoming)
        .collect()
}

pub fn get_successors(cfg: &Graph<BasicBlock, ControlFlow>, node: NodeIndex) -> Vec<NodeIndex> {
    cfg.neighbors_directed(node, petgraph::Direction::Outgoing)
        .collect()
}

pub fn is_dominator(cfg: &Graph<BasicBlock, ControlFlow>, dom: NodeIndex, node: NodeIndex) -> bool {
    if dom == node {
        return true;
    }

    let mut visited = std::collections::HashSet::new();
    let mut queue = vec![dom];

    while let Some(current) = queue.pop() {
        if current == node {
            return true;
        }
        if visited.insert(current) {
            queue.extend(get_successors(cfg, current));
        }
    }

    false
}
