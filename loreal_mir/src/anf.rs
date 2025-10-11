use loreal_ast::{self as ast, Expr, Span, Statement};
use smol_str::SmolStr;

pub struct ANFTransformer {
    next_temp_id: usize,
    bindings: Vec<Statement>,
}

impl ANFTransformer {
    pub fn get_bindings(&self) -> Vec<Statement> {
        self.bindings.clone()
    }

    pub fn clear_bindings(&mut self) {
        self.bindings.clear();
    }

impl ANFTransformer {
    pub fn new() -> Self {
        Self {
            next_temp_id: 0,
            bindings: Vec::new(),
        }
    }

    fn new_temp(&mut self, _span: Span) -> SmolStr {
        let name: SmolStr = format!("_anf{}", self.next_temp_id).into();
        self.next_temp_id += 1;
        name
    }

    fn is_atomic(&self, expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::IntLiteral { .. }
                | Expr::FloatLiteral { .. }
                | Expr::BoolLiteral { .. }
                | Expr::CharLiteral { .. }
                | Expr::StringLiteral { .. }
                | Expr::NilLiteral { .. }
                | Expr::Identifier { .. }
        )
    }

    pub fn transform(&mut self, expr: Expr) -> Expr {
        if self.is_atomic(&expr) {
            return expr;
        }

        match expr {
            Expr::BinaryOp {
                op,
                left,
                right,
                span,
            } => {
                let left_atomic = self.transform(*left);
                let right_atomic = self.transform(*right);

                let temp = self.new_temp(span);
                let atomic_left = if self.is_atomic(&left_atomic) {
                    left_atomic
                } else {
                    let t = self.new_temp(span);
                    self.bindings.push(Statement::Let {
                        pattern: ast::Pattern::Identifier {
                            name: t.clone(),
                            span,
                        },
                        type_annotation: None,
                        value: Box::new(left_atomic),
                        span,
                    });
                    Expr::Identifier { name: t, span }
                };

                let atomic_right = if self.is_atomic(&right_atomic) {
                    right_atomic
                } else {
                    let t = self.new_temp(span);
                    self.bindings.push(Statement::Let {
                        pattern: ast::Pattern::Identifier {
                            name: t.clone(),
                            span,
                        },
                        type_annotation: None,
                        value: Box::new(right_atomic),
                        span,
                    });
                    Expr::Identifier { name: t, span }
                };

                let result_expr = Expr::BinaryOp {
                    op,
                    left: Box::new(atomic_left),
                    right: Box::new(atomic_right),
                    span,
                };

                self.bindings.push(Statement::Let {
                    pattern: ast::Pattern::Identifier {
                        name: temp.clone(),
                        span,
                    },
                    type_annotation: None,
                    value: Box::new(result_expr),
                    span,
                });

                Expr::Identifier { name: temp, span }
            }

            Expr::UnaryOp { op, operand, span } => {
                let atomic_operand = self.transform(*operand);

                let temp = self.new_temp(span);
                let atomic_op = if self.is_atomic(&atomic_operand) {
                    atomic_operand
                } else {
                    let t = self.new_temp(span);
                    self.bindings.push(Statement::Let {
                        pattern: ast::Pattern::Identifier {
                            name: t.clone(),
                            span,
                        },
                        type_annotation: None,
                        value: Box::new(atomic_operand),
                        span,
                    });
                    Expr::Identifier { name: t, span }
                };

                let result_expr = Expr::UnaryOp {
                    op,
                    operand: Box::new(atomic_op),
                    span,
                };

                self.bindings.push(Statement::Let {
                    pattern: ast::Pattern::Identifier {
                        name: temp.clone(),
                        span,
                    },
                    type_annotation: None,
                    value: Box::new(result_expr),
                    span,
                });

                Expr::Identifier { name: temp, span }
            }

            Expr::FunctionCall { func, args, span } => {
                let atomic_func = self.transform(*func);
                let mut atomic_args = Vec::new();
                for arg in args {
                    let atomic_arg = self.transform(arg);
                    if self.is_atomic(&atomic_arg) {
                        atomic_args.push(atomic_arg);
                    } else {
                        let t = self.new_temp(span);
                        self.bindings.push(Statement::Let {
                            pattern: ast::Pattern::Identifier {
                                name: t.clone(),
                                span,
                            },
                            type_annotation: None,
                            value: Box::new(atomic_arg),
                            span,
                        });
                        atomic_args.push(Expr::Identifier { name: t, span });
                    }
                }

                let temp = self.new_temp(span);
                let result_expr = Expr::FunctionCall {
                    func: Box::new(atomic_func),
                    args: atomic_args,
                    span,
                };

                self.bindings.push(Statement::Let {
                    pattern: ast::Pattern::Identifier {
                        name: temp.clone(),
                        span,
                    },
                    type_annotation: None,
                    value: Box::new(result_expr),
                    span,
                });

                Expr::Identifier { name: temp, span }
            }

            Expr::Block {
                statements,
                result,
                span,
            } => {
                let mut transformed_statements = Vec::new();
                for stmt in statements {
                    match stmt {
                        Statement::Let {
                            pattern,
                            type_annotation,
                            value,
                            span: stmt_span,
                        } => {
                            let transformed_value = self.transform(*value);
                            transformed_statements.push(Statement::Let {
                                pattern,
                                type_annotation,
                                value: Box::new(transformed_value),
                                span: stmt_span,
                            });
                        }
                        Statement::Expr { expr, span: stmt_span } => {
                            let transformed_expr = self.transform(*expr);
                            transformed_statements.push(Statement::Expr {
                                expr: Box::new(transformed_expr),
                                span: stmt_span,
                            });
                        }
                    }
                }

                let transformed_result = self.transform(*result);

                Expr::Block {
                    statements: transformed_statements,
                    result: Box::new(transformed_result),
                    span,
                }
            }

            _ => expr,
        }
    }
}
