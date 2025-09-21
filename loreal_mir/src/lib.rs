use loreal_ast::{self as ast, Span, Type};
use petgraph::graph::{Graph, NodeIndex};
use smol_str::SmolStr;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Var(SmolStr),
    IntConst(i64),
    BoolConst(bool),
    StringConst(SmolStr),
    FloatConst(f64),
    CharConst(char),
    NilConst,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Assign {
        target: SmolStr,
        value: Value,
    },

    BinaryOp {
        target: SmolStr,
        op: ast::BinOp,
        left: Value,
        right: Value,
    },

    UnaryOp {
        target: SmolStr,
        op: ast::UnOp,
        operand: Value,
    },

    Inc {
        var: SmolStr,
    },
    Dec {
        var: SmolStr,
    },
    Drop {
        var: SmolStr,
    },
    Dup {
        var: SmolStr,
    },

    Reuse {
        old: SmolStr,
        new: SmolStr,
    },

    Call {
        target: Option<SmolStr>,
        func: SmolStr,
        args: Vec<Value>,
    },

    Alloc {
        target: SmolStr,
        ty: Type,
    },

    FieldStore {
        target: SmolStr,
        field: SmolStr,
        value: Value,
    },

    FieldLoad {
        target: SmolStr,
        object: Value,
        field: SmolStr,
    },

    AllocList {
        target: SmolStr,
        element_type: Type,
        length: Value,
    },

    IndexStore {
        target: SmolStr,
        index: Value,
        value: Value,
    },

    IndexLoad {
        target: SmolStr,
        object: Value,
        index: Value,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    Return(Value),
    Jump(NodeIndex),
    Branch {
        condition: Value,
        then_block: NodeIndex,
        else_block: NodeIndex,
    },
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: usize,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ControlFlow {
    Sequential,
    Then,
    Else,
}

#[derive(Debug, Clone)]
pub struct MirFunction {
    pub name: SmolStr,
    pub params: Vec<(SmolStr, Type)>,
    pub return_type: Type,
    pub cfg: Graph<BasicBlock, ControlFlow>,
    pub entry: NodeIndex,
    pub exit: NodeIndex,
    pub local_types: HashMap<SmolStr, Type>,
}
