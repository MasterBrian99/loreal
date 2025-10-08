use loreal_ast::{self as ast, Span, Type};
use petgraph::graph::{Graph, NodeIndex};
use smol_str::SmolStr;
use std::collections::HashMap;

pub mod anf;

pub use anf::ANFTransformer;

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

pub struct MirBuilder {
    cfg: Graph<BasicBlock, ControlFlow>,
    current_block: Option<NodeIndex>,
    next_block_id: usize,
    next_temp_id: usize,
    variables: HashMap<SmolStr, Value>,
    pub local_types: HashMap<SmolStr, Type>,
    struct_types: HashMap<SmolStr, ast::StructDef>,
}

impl MirBuilder {
    pub fn new() -> Self {
        Self {
            cfg: Graph::new(),
            current_block: None,
            next_block_id: 0,
            next_temp_id: 0,
            variables: HashMap::new(),
            local_types: HashMap::new(),
            struct_types: HashMap::new(),
        }
    }

    pub fn with_context(
        local_types: HashMap<SmolStr, Type>,
        struct_types: HashMap<SmolStr, ast::StructDef>,
    ) -> Self {
        Self {
            cfg: Graph::new(),
            current_block: None,
            next_block_id: 0,
            next_temp_id: 0,
            variables: HashMap::new(),
            local_types,
            struct_types,
        }
    }

    fn new_temp(&mut self, ty: Type) -> SmolStr {
        let name: SmolStr = format!("_t{}", self.next_temp_id).into();
        self.next_temp_id += 1;
        self.local_types.insert(name.clone(), ty);
        name
    }

    fn new_block(&mut self, span: Span) -> NodeIndex {
        let id = self.next_block_id;
        self.next_block_id += 1;

        let block = BasicBlock {
            id,
            instructions: Vec::new(),
            terminator: Terminator::Return(Value::NilConst),
            span,
        };

        self.cfg.add_node(block)
    }

    fn emit(&mut self, instr: Instruction) {
        if let Some(block_idx) = self.current_block {
            if let Some(block) = self.cfg.node_weight_mut(block_idx) {
                block.instructions.push(instr);
            }
        }
    }

    fn set_terminator(&mut self, term: Terminator) {
        if let Some(block_idx) = self.current_block {
            if let Some(block) = self.cfg.node_weight_mut(block_idx) {
                block.terminator = term;
            }
        }
    }

    pub fn lower_function(
        &mut self,
        name: SmolStr,
        params: Vec<(SmolStr, Type)>,
        return_type: Type,
        body: &ast::Expr,
    ) -> MirFunction {
        let entry = self.new_block(body.span());
        self.current_block = Some(entry);

        for (param_name, _) in &params {
            self.variables
                .insert(param_name.clone(), Value::Var(param_name.clone()));
        }

        let (result_value, _) = self.lower_expr(body);

        self.set_terminator(Terminator::Return(result_value));

        let exit = entry;

        MirFunction {
            name,
            params,
            return_type,
            cfg: self.cfg.clone(),
            entry,
            exit,
            local_types: self.local_types.clone(),
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> (Value, Type) {
        match expr {
            ast::Expr::IntLiteral { value, .. } => (
                Value::IntConst(*value),
                Type::Named {
                    name: "Int".into(),
                    span: Span::new(0, 0),
                },
            ),
            ast::Expr::FloatLiteral { value, .. } => (
                Value::FloatConst(*value),
                Type::Named {
                    name: "Float".into(),
                    span: Span::new(0, 0),
                },
            ),
            ast::Expr::BoolLiteral { value, .. } => (
                Value::BoolConst(*value),
                Type::Named {
                    name: "Bool".into(),
                    span: Span::new(0, 0),
                },
            ),
            ast::Expr::StringLiteral { value, .. } => (
                Value::StringConst(value.clone()),
                Type::Named {
                    name: "String".into(),
                    span: Span::new(0, 0),
                },
            ),
            ast::Expr::NilLiteral { .. } => (
                Value::NilConst,
                Type::Named {
                    name: "Nil".into(),
                    span: Span::new(0, 0),
                },
            ),

            ast::Expr::Identifier { name, span } => {
                let val = self
                    .variables
                    .get(name)
                    .cloned()
                    .unwrap_or(Value::Var(name.clone()));
                let ty = self.local_types.get(name).cloned().unwrap_or(Type::Named {
                    name: "Unknown".into(),
                    span: *span,
                });
                (val, ty)
            }

            ast::Expr::BinaryOp {
                op, left, right, ..
            } => {
                let (left_val, left_ty) = self.lower_expr(left);
                let (right_val, _) = self.lower_expr(right);
                let res_ty = match op {
                    ast::BinOp::Eq
                    | ast::BinOp::Ne
                    | ast::BinOp::Lt
                    | ast::BinOp::Le
                    | ast::BinOp::Gt
                    | ast::BinOp::Ge => Type::Named {
                        name: "Bool".into(),
                        span: Span::new(0, 0),
                    },
                    _ => left_ty,
                };

                let temp = self.new_temp(res_ty.clone());

                self.emit(Instruction::BinaryOp {
                    target: temp.clone(),
                    op: *op,
                    left: left_val,
                    right: right_val,
                });

                let result = Value::Var(temp.clone());
                self.variables.insert(temp, result.clone());
                (result, res_ty)
            }

            ast::Expr::UnaryOp { op, operand, .. } => {
                let (operand_val, operand_ty) = self.lower_expr(operand);
                let temp = self.new_temp(operand_ty.clone());

                self.emit(Instruction::UnaryOp {
                    target: temp.clone(),
                    op: *op,
                    operand: operand_val,
                });

                let result = Value::Var(temp.clone());
                self.variables.insert(temp, result.clone());
                (result, operand_ty)
            }

            ast::Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let (cond_val, _) = self.lower_expr(condition);

                let then_block = self.new_block(*span);
                let else_block = self.new_block(*span);
                let merge_block = self.new_block(*span);

                self.set_terminator(Terminator::Branch {
                    condition: cond_val,
                    then_block,
                    else_block,
                });

                if let Some(current) = self.current_block {
                    self.cfg.add_edge(current, then_block, ControlFlow::Then);
                    self.cfg.add_edge(current, else_block, ControlFlow::Else);
                }

                self.current_block = Some(then_block);
                let (then_val, then_ty) = self.lower_expr(then_branch);
                let result_temp = self.new_temp(then_ty.clone());

                self.emit(Instruction::Assign {
                    target: result_temp.clone(),
                    value: then_val,
                });
                self.set_terminator(Terminator::Jump(merge_block));
                self.cfg
                    .add_edge(then_block, merge_block, ControlFlow::Sequential);

                self.current_block = Some(else_block);
                let (else_val, _) = self.lower_expr(else_branch);
                self.emit(Instruction::Assign {
                    target: result_temp.clone(),
                    value: else_val,
                });
                self.set_terminator(Terminator::Jump(merge_block));
                self.cfg
                    .add_edge(else_block, merge_block, ControlFlow::Sequential);

                self.current_block = Some(merge_block);

                (Value::Var(result_temp), then_ty)
            }

            ast::Expr::Block {
                statements,
                result,
                ..
            } => {
                for stmt in statements {
                    self.lower_statement(stmt);
                }
                self.lower_expr(result)
            }

            _ => (
                Value::NilConst,
                Type::Named {
                    name: "Unknown".into(),
                    span: Span::new(0, 0),
                },
            ),
        }
    }

    fn lower_statement(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::Let {
                pattern,
                value,
                type_annotation: _,
                ..
            } => {
                if let ast::Pattern::Identifier { name, .. } = pattern {
                    let (val, ty) = self.lower_expr(value);
                    self.emit(Instruction::Assign {
                        target: name.clone(),
                        value: val,
                    });
                    self.variables.insert(name.clone(), Value::Var(name.clone()));
                    self.local_types.insert(name.clone(), ty);
                }
            }
            ast::Statement::Expr { expr, .. } => {
                self.lower_expr(expr);
            }
        }
    }
}

                self.lower_expr(result)
            }

            _ => (
                Value::NilConst,
                Type::Named {
                    name: "Unknown".into(),
                    span: Span::new(0, 0),
                },
            ),
        }
    }
}
