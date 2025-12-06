use loreal_ast::*;

pub trait ExprVisitor {
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLiteral { .. } => self.visit_int_literal(expr),
            Expr::FloatLiteral { .. } => self.visit_float_literal(expr),
            Expr::BoolLiteral { .. } => self.visit_bool_literal(expr),
            Expr::CharLiteral { .. } => self.visit_char_literal(expr),
            Expr::StringLiteral { .. } => self.visit_string_literal(expr),
            Expr::NilLiteral { .. } => self.visit_nil_literal(expr),
            Expr::Identifier { .. } => self.visit_identifier(expr),
            Expr::FunctionCall { .. } => self.visit_function_call(expr),
            Expr::Lambda { .. } => self.visit_lambda(expr),
            Expr::BinaryOp { .. } => self.visit_binary_op(expr),
            Expr::UnaryOp { .. } => self.visit_unary_op(expr),
            Expr::If { .. } => self.visit_if(expr),
            Expr::Match { .. } => self.visit_match(expr),
            Expr::List { .. } => self.visit_list(expr),
            Expr::Tuple { .. } => self.visit_tuple(expr),
            Expr::Map { .. } => self.visit_map(expr),
            Expr::StructLiteral { .. } => self.visit_struct_literal(expr),
            Expr::FieldAccess { .. } => self.visit_field_access(expr),
            Expr::IndexAccess { .. } => self.visit_index_access(expr),
            Expr::Pipe { .. } => self.visit_pipe(expr),
            Expr::Block { .. } => self.visit_block(expr),
            Expr::Loop { .. } => self.visit_loop(expr),
            Expr::Next { .. } => self.visit_next(expr),
            Expr::Break { .. } => self.visit_break(expr),
        }
    }

    fn visit_int_literal(&mut self, _expr: &Expr) {}
    fn visit_float_literal(&mut self, _expr: &Expr) {}
    fn visit_bool_literal(&mut self, _expr: &Expr) {}
    fn visit_char_literal(&mut self, _expr: &Expr) {}
    fn visit_string_literal(&mut self, _expr: &Expr) {}
    fn visit_nil_literal(&mut self, _expr: &Expr) {}
    fn visit_identifier(&mut self, _expr: &Expr) {}
    fn visit_function_call(&mut self, _expr: &Expr) {}
    fn visit_lambda(&mut self, _expr: &Expr) {}
    fn visit_binary_op(&mut self, _expr: &Expr) {}
    fn visit_unary_op(&mut self, _expr: &Expr) {}
    fn visit_if(&mut self, _expr: &Expr) {}
    fn visit_match(&mut self, _expr: &Expr) {}
    fn visit_list(&mut self, _expr: &Expr) {}
    fn visit_tuple(&mut self, _expr: &Expr) {}
    fn visit_map(&mut self, _expr: &Expr) {}
    fn visit_struct_literal(&mut self, _expr: &Expr) {}
    fn visit_field_access(&mut self, _expr: &Expr) {}
    fn visit_index_access(&mut self, _expr: &Expr) {}
    fn visit_pipe(&mut self, _expr: &Expr) {}
    fn visit_block(&mut self, _expr: &Expr) {}
    fn visit_loop(&mut self, _expr: &Expr) {}
    fn visit_next(&mut self, _expr: &Expr) {}
    fn visit_break(&mut self, _expr: &Expr) {}
}
