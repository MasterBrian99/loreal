use crate::error::SemanticError;
use crate::symbol_table::{SymbolInfo, SymbolTable};
use crate::types::ConcreteType;
use loreal_ast::*;
use smol_str::SmolStr;
use std::collections::HashMap;

pub struct TypeChecker {
    symbol_table: SymbolTable,
    errors: Vec<SemanticError>,
    pub struct_types: HashMap<SmolStr, StructDef>,
    pub variable_types: HashMap<(SmolStr, SmolStr), ConcreteType>,
    pub current_func: Option<SmolStr>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
            struct_types: HashMap::new(),
            variable_types: HashMap::new(),
            current_func: None,
        }
    }

    pub fn check_module(&mut self, module: &Module) -> Result<(), Vec<SemanticError>> {
        for decl in &module.declarations {
            if let Declaration::Struct(struct_def) = decl {
                self.struct_types
                    .insert(struct_def.name.clone(), struct_def.clone());
            }
        }

        for decl in &module.declarations {
            if let Declaration::Function(func) = decl {
                let param_types: Vec<ConcreteType> = func
                    .params
                    .iter()
                    .map(|(_, ty)| ConcreteType::from_ast_type(ty))
                    .collect();

                let return_type = ConcreteType::from_ast_type(&func.return_type);

                let func_type = ConcreteType::Function {
                    params: param_types,
                    return_type: Box::new(return_type),
                };

                if let Err(e) = self.symbol_table.insert(
                    func.name.clone(),
                    SymbolInfo {
                        name: func.name.clone(),
                        ty: func_type,
                        span: func.span,
                    },
                ) {
                    self.errors.push(e);
                }
            }
        }

        for decl in &module.declarations {
            if let Declaration::Function(func) = decl {
                self.check_function(func);
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn check_function(&mut self, func: &FunctionDef) {
        self.symbol_table.push_scope();

        for (pattern, ty) in &func.params {
            if let Pattern::Identifier { name, span } = pattern {
                let param_type = ConcreteType::from_ast_type(ty);
                if let Err(e) = self.symbol_table.insert(
                    name.clone(),
                    SymbolInfo {
                        name: name.clone(),
                        ty: param_type.clone(),
                        span: *span,
                    },
                ) {
                    self.errors.push(e);
                }
                self.variable_types
                    .insert((func.name.clone(), name.clone()), param_type);
            }
        }

        self.current_func = Some(func.name.clone());
        let body_type = self.infer_expr(&func.body);
        let expected_return = ConcreteType::from_ast_type(&func.return_type);

        if !body_type.is_compatible(&expected_return) {
            self.errors.push(SemanticError::TypeMismatch {
                expected: expected_return.to_string(),
                found: body_type.to_string(),
                span: func.body.span(),
            });
        }
        self.current_func = None;

        self.symbol_table.pop_scope();
    }

    fn infer_expr(&mut self, expr: &Expr) -> ConcreteType {
        match expr {
            Expr::IntLiteral { .. } => ConcreteType::Int,
            Expr::FloatLiteral { .. } => ConcreteType::Float,
            Expr::BoolLiteral { .. } => ConcreteType::Bool,
            Expr::CharLiteral { .. } => ConcreteType::Char,
            Expr::StringLiteral { .. } => ConcreteType::String,
            Expr::NilLiteral { .. } => ConcreteType::Nil,

            Expr::Identifier { name, span } => {
                if let Some(info) = self.symbol_table.lookup(name) {
                    info.ty.clone()
                } else {
                    self.errors.push(SemanticError::UndefinedVariable {
                        name: name.clone(),
                        span: *span,
                    });
                    ConcreteType::Unknown
                }
            }

            Expr::BinaryOp {
                op,
                left,
                right,
                span,
            } => {
                let left_ty = self.infer_expr(left);
                let right_ty = self.infer_expr(right);
                self.check_binary_op(*op, &left_ty, &right_ty, *span)
            }

            Expr::UnaryOp { op, operand, .. } => {
                let operand_ty = self.infer_expr(operand);
                match op {
                    UnOp::Neg => {
                        if operand_ty.is_compatible(&ConcreteType::Int)
                            || operand_ty.is_compatible(&ConcreteType::Float)
                        {
                            operand_ty
                        } else {
                            ConcreteType::Int
                        }
                    }
                    UnOp::Not => ConcreteType::Bool,
                }
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let cond_ty = self.infer_expr(condition);
                if !cond_ty.is_compatible(&ConcreteType::Bool) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: cond_ty.to_string(),
                        span: condition.span(),
                    });
                }

                let then_ty = self.infer_expr(then_branch);
                let else_ty = self.infer_expr(else_branch);

                if !then_ty.is_compatible(&else_ty) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: then_ty.to_string(),
                        found: else_ty.to_string(),
                        span: *span,
                    });
                }

                then_ty
            }

            Expr::Block {
                statements, result, ..
            } => {
                self.symbol_table.push_scope();

                for stmt in statements {
                    self.check_statement(stmt);
                }

                let result_ty = self.infer_expr(result);
                self.symbol_table.pop_scope();
                result_ty
            }

            Expr::FunctionCall { func, args, span } => {
                if let Expr::Identifier { name, .. } = func.as_ref() {
                    let func_info = self.symbol_table.lookup(name).cloned();

                    if let Some(info) = func_info {
                        if let ConcreteType::Function {
                            params,
                            return_type,
                        } = &info.ty
                        {
                            if args.len() != params.len() {
                                self.errors.push(SemanticError::TypeMismatch {
                                    expected: format!("{} arguments", params.len()),
                                    found: format!("{} arguments", args.len()),
                                    span: *span,
                                });
                            }

                            for (arg, expected_ty) in args.iter().zip(params.iter()) {
                                let arg_ty = self.infer_expr(arg);
                                if !arg_ty.is_compatible(expected_ty) {
                                    self.errors.push(SemanticError::TypeMismatch {
                                        expected: expected_ty.to_string(),
                                        found: arg_ty.to_string(),
                                        span: arg.span(),
                                    });
                                }
                            }

                            (**return_type).clone()
                        } else {
                            self.errors.push(SemanticError::TypeMismatch {
                                expected: "function".to_string(),
                                found: info.ty.to_string(),
                                span: *span,
                            });
                            ConcreteType::Unknown
                        }
                    } else {
                        self.errors.push(SemanticError::UndefinedFunction {
                            name: name.clone(),
                            span: *span,
                        });
                        ConcreteType::Unknown
                    }
                } else {
                    ConcreteType::Unknown
                }
            }

            Expr::StructLiteral { name, fields, span } => {
                let struct_def = self.struct_types.get(name).cloned();
                if let Some(def) = struct_def {
                    let mut field_map: HashMap<SmolStr, &Type> =
                        def.fields.iter().map(|(n, t)| (n.clone(), t)).collect();

                    for (field_name, field_expr) in fields {
                        if let Some(expected_ty) = field_map.remove(field_name) {
                            let val_ty = self.infer_expr(field_expr);
                            let expected_concrete = ConcreteType::from_ast_type(expected_ty);
                            if !val_ty.is_compatible(&expected_concrete) {
                                self.errors.push(SemanticError::TypeMismatch {
                                    expected: expected_concrete.to_string(),
                                    found: val_ty.to_string(),
                                    span: field_expr.span(),
                                });
                            }
                        } else {
                            self.errors.push(SemanticError::UndefinedField {
                                field: field_name.clone(),
                                ty: name.to_string(),
                                span: field_expr.span(),
                            });
                        }
                    }

                    for missing_field in field_map.keys() {
                        self.errors.push(SemanticError::TypeMismatch {
                            expected: format!("field '{}'", missing_field),
                            found: "missing".to_string(),
                            span: *span,
                        });
                    }

                    ConcreteType::Named(name.clone())
                } else {
                    self.errors.push(SemanticError::UndefinedType {
                        name: name.clone(),
                        span: *span,
                    });
                    ConcreteType::Unknown
                }
            }

            Expr::Tuple { elements, span: _ } => {
                let mut types = Vec::new();
                for elem in elements {
                    types.push(self.infer_expr(elem));
                }
                ConcreteType::Tuple(types)
            }

            Expr::FieldAccess {
                object,
                field,
                span,
            } => {
                let obj_ty = self.infer_expr(object);
                match &obj_ty {
                    ConcreteType::Named(name) => {
                        if let Some(def) = self.struct_types.get(name) {
                            if let Some((_, field_ty)) = def.fields.iter().find(|(n, _)| n == field)
                            {
                                ConcreteType::from_ast_type(field_ty)
                            } else {
                                self.errors.push(SemanticError::UndefinedField {
                                    field: field.clone(),
                                    ty: name.to_string(),
                                    span: *span,
                                });
                                ConcreteType::Unknown
                            }
                        } else {
                            ConcreteType::Unknown
                        }
                    }
                    ConcreteType::Tuple(types) => {
                        if let Ok(index) = field.parse::<usize>() {
                            if index < types.len() {
                                types[index].clone()
                            } else {
                                self.errors.push(SemanticError::TypeMismatch {
                                    expected: format!("tuple index < {}", types.len()),
                                    found: format!("index {}", index),
                                    span: *span,
                                });
                                ConcreteType::Unknown
                            }
                        } else {
                            self.errors.push(SemanticError::TypeMismatch {
                                expected: "tuple index (integer)".to_string(),
                                found: format!("field '{}'", field),
                                span: *span,
                            });
                            ConcreteType::Unknown
                        }
                    }
                    _ => {
                        self.errors.push(SemanticError::TypeMismatch {
                            expected: "struct".to_string(),
                            found: obj_ty.to_string(),
                            span: *span,
                        });
                        ConcreteType::Unknown
                    }
                }
            }

            _ => ConcreteType::Unknown,
        }
    }

    fn check_binary_op(
        &mut self,
        op: BinOp,
        left_ty: &ConcreteType,
        right_ty: &ConcreteType,
        span: Span,
    ) -> ConcreteType {
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if left_ty.is_compatible(&ConcreteType::Int)
                    && right_ty.is_compatible(&ConcreteType::Int)
                {
                    ConcreteType::Int
                } else if left_ty.is_compatible(&ConcreteType::Float)
                    || right_ty.is_compatible(&ConcreteType::Float)
                {
                    ConcreteType::Float
                } else {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op,
                        left: left_ty.to_string(),
                        right: right_ty.to_string(),
                        span,
                    });
                    ConcreteType::Int
                }
            }

            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => ConcreteType::Bool,

            BinOp::Eq | BinOp::Ne => ConcreteType::Bool,

            BinOp::And | BinOp::Or => {
                if !left_ty.is_compatible(&ConcreteType::Bool) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: left_ty.to_string(),
                        span,
                    });
                }
                if !right_ty.is_compatible(&ConcreteType::Bool) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: right_ty.to_string(),
                        span,
                    });
                }
                ConcreteType::Bool
            }

            BinOp::Pipe => ConcreteType::Unknown,
        }
    }

    fn check_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Let {
                pattern,
                type_annotation,
                value,
                span,
            } => {
                let value_ty = self.infer_expr(value);

                if let Some(annotation) = type_annotation {
                    let expected_ty = ConcreteType::from_ast_type(annotation);
                    if !value_ty.is_compatible(&expected_ty) {
                        self.errors.push(SemanticError::TypeMismatch {
                            expected: expected_ty.to_string(),
                            found: value_ty.to_string(),
                            span: *span,
                        });
                    }
                }

                if let Pattern::Identifier { name, span } = pattern {
                    let ty = if let Some(annotation) = type_annotation {
                        ConcreteType::from_ast_type(annotation)
                    } else {
                        value_ty
                    };

                    if let Err(e) = self.symbol_table.insert(
                        name.clone(),
                        SymbolInfo {
                            name: name.clone(),
                            ty: ty.clone(),
                            span: *span,
                        },
                    ) {
                        self.errors.push(e);
                    }
                    if let Some(func_name) = &self.current_func {
                        self.variable_types
                            .insert((func_name.clone(), name.clone()), ty);
                    }
                }
            }

            Statement::Expr { expr, .. } => {
                self.infer_expr(expr);
            }
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
