use loreal_ast::{self as ast, Expr, Span, Statement};
use smol_str::SmolStr;

pub struct ANFTransformer {
    next_temp_id: usize,
    bindings: Vec<Statement>,
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

            _ => expr,
        }
    }
}
