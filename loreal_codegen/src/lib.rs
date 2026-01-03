//! Code generation backend for Loreal
//!
//! Phase 4: Multi-backend code generation (LLVM now, MLIR later)
//!
//! Architecture is designed to support multiple backends:
//! - LLVM backend (inkwell) - Current implementation  
//! - MLIR backend (melior) - Future optimization layer

use loreal_ast::Type;
use loreal_mir::{Instruction, MirFunction, Terminator, Value};
use std::collections::HashMap;

// ============================================================================
// Backend Abstraction - Allows easy MLIR addition later
// ============================================================================

/// Common interface for code generation backends
pub trait CodegenBackend {
    fn generate_function(&mut self, function: &MirFunction) -> Result<(), CodegenError>;
    fn emit_object(&self, output_path: &str) -> Result<(), CodegenError>;
    fn emit_ir(&self, output_path: &str) -> Result<(), CodegenError>;
}

// ============================================================================
// LLVM Backend (inkwell) - Current Implementation
// ============================================================================

#[cfg(feature = "llvm-backend")]
mod llvm_backend {
    use super::*;
    use inkwell::builder::Builder;
    use inkwell::context::Context;
    use inkwell::module::Module;
    use inkwell::targets::{CodeModel, FileType, RelocMode, Target, TargetMachine};
    use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
    use inkwell::values::{
        BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue, ValueKind,
    };
    use inkwell::IntPredicate;

    pub struct LLVMBackend<'ctx> {
        context: &'ctx Context,
        module: Module<'ctx>,
        builder: Builder<'ctx>,
        /// Map variable names to (alloca pointer, value type)
        variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
        /// Track current function being compiled
        current_function: Option<FunctionValue<'ctx>>,
        /// Map struct names to (LLVM StructType, Field names)
        struct_metadata: HashMap<String, (inkwell::types::StructType<'ctx>, Vec<String>)>,
    }

    impl<'ctx> LLVMBackend<'ctx> {
        pub fn new(context: &'ctx Context, module_name: &str) -> Self {
            let module = context.create_module(module_name);
            let builder = context.create_builder();

            Self {
                context,
                module,
                builder,
                variables: HashMap::new(),
                current_function: None,
                struct_metadata: HashMap::new(),
            }
        }

        pub fn define_structs(
            &mut self,
            structs: &[loreal_ast::StructDef],
        ) -> Result<(), CodegenError> {
            let header_type = self.context.struct_type(
                &[
                    self.context.i64_type().into(), // ref_count
                    self.context.i64_type().into(), // size
                ],
                false,
            );

            // First pass: create opaque types
            for def in structs {
                let struct_type = self.context.opaque_struct_type(def.name.as_str());
                let field_names = def.fields.iter().map(|(n, _)| n.to_string()).collect();
                self.struct_metadata
                    .insert(def.name.to_string(), (struct_type, field_names));
            }

            // Second pass: set bodies
            for def in structs {
                let (struct_type, _) = &self.struct_metadata[def.name.as_str()];
                let mut field_types = Vec::new();
                field_types.push(header_type.into()); // Header at index 0

                for (_, ty) in &def.fields {
                    field_types.push(self.type_to_llvm(ty)?);
                }
                struct_type.set_body(&field_types, false);
            }
            Ok(())
        }

        /// Convert Loreal Type to LLVM Type
        fn type_to_llvm(&self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, CodegenError> {
            match ty {
                Type::Named { name, .. } => match name.as_str() {
                    "Int" => Ok(self.context.i64_type().into()),
                    "Float" => Ok(self.context.f64_type().into()),
                    "Bool" => Ok(self.context.bool_type().into()),
                    "Char" => Ok(self.context.i32_type().into()),
                    _ => {
                        // Check if it's a defined struct
                        if self.struct_metadata.contains_key(name.as_str()) {
                            // Structs are always passed as pointers (objects) in Loreal
                            Ok(self
                                .context
                                .ptr_type(inkwell::AddressSpace::default())
                                .into())
                        } else {
                            // Fallback to generic pointer for now (e.g. usage before definition in recursive types?)
                            // Or error. For now, opaque ptr.
                            Ok(self
                                .context
                                .ptr_type(inkwell::AddressSpace::default())
                                .into())
                        }
                    }
                },
                Type::List { .. } => {
                    // Lists are pointer types
                    Ok(self
                        .context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into())
                }
                Type::Tuple { .. } => {
                    // Tuples are pointer types
                    Ok(self
                        .context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into())
                }
                Type::Function { .. } => {
                    // Function pointers
                    Ok(self
                        .context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into())
                }
                _ => Ok(self
                    .context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into()),
            }
        }

        /// Get or declare the loreal_alloc function from runtime
        fn get_loreal_alloc(&self) -> FunctionValue<'ctx> {
            if let Some(func) = self.module.get_function("loreal_alloc") {
                func
            } else {
                let i64_type = self.context.i64_type();
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                let fn_type = ptr_type.fn_type(&[i64_type.into()], false);
                self.module.add_function("loreal_alloc", fn_type, None)
            }
        }

        /// Convert MIR Value to LLVM Value
        fn value_to_llvm(&self, value: &Value) -> Result<BasicValueEnum<'ctx>, CodegenError> {
            match value {
                Value::IntConst(n) => Ok(self.context.i64_type().const_int(*n as u64, true).into()),
                Value::BoolConst(b) => Ok(self
                    .context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false)
                    .into()),
                Value::FloatConst(f) => Ok(self.context.f64_type().const_float(*f).into()),
                Value::CharConst(c) => {
                    Ok(self.context.i32_type().const_int(*c as u64, false).into())
                }
                Value::StringConst(s) => {
                    // Create global string constant
                    let string_value =
                        self.builder
                            .build_global_string_ptr(s, "str")
                            .map_err(|e| {
                                CodegenError::LLVMError(format!(
                                    "Failed to create string constant: {}",
                                    e
                                ))
                            })?;
                    Ok(string_value.as_pointer_value().into())
                }
                Value::NilConst => {
                    // Nil as null pointer
                    Ok(self
                        .context
                        .ptr_type(inkwell::AddressSpace::default())
                        .const_null()
                        .into())
                }
                Value::Var(name) => {
                    let (ptr, ty) = self
                        .variables
                        .get(name.as_str())
                        .copied()
                        .ok_or_else(|| CodegenError::UndefinedVariable(name.to_string()))?;

                    self.builder
                        .build_load(ty, ptr, name.as_str())
                        .map_err(|e| CodegenError::LLVMError(format!("Load failed: {}", e)))
                }
            }
        }

        /// Generate LLVM function from MIR
        fn generate_llvm_function(
            &mut self,
            mir_func: &MirFunction,
        ) -> Result<FunctionValue<'ctx>, CodegenError> {
            // Build parameter types
            let param_types: Vec<BasicMetadataTypeEnum> = mir_func
                .params
                .iter()
                .map(|(_, ty)| self.type_to_llvm(ty).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;

            // Build return type
            let return_type = self.type_to_llvm(&mir_func.return_type)?;

            // Create function type
            let fn_type = return_type.fn_type(&param_types, false);

            println!(
                "Generating function: '{:?}' (len: {})",
                mir_func.name,
                mir_func.name.len()
            );

            // Rename main to loreal_main to allow wrapper generation
            let func_name = if mir_func.name.trim() == "main" {
                "loreal_main"
            } else {
                mir_func.name.as_str()
            };

            // Add function to module
            let function = self.module.add_function(func_name, fn_type, None);

            // Set parameter names - but don't store in variables map yet
            // We'll alloca and store them in generate_function to be consistent
            for (i, (param_name, _)) in mir_func.params.iter().enumerate() {
                let param = function
                    .get_nth_param(i as u32)
                    .ok_or_else(|| CodegenError::LLVMError("Missing parameter".into()))?;
                param.set_name(param_name.as_str());
            }

            Ok(function)
        }

        /// Get or declare a runtime function
        fn get_runtime_fn(&self, name: &str) -> FunctionValue<'ctx> {
            if let Some(f) = self.module.get_function(name) {
                return f;
            }

            // Both inc and dec take a single pointer and return void
            let void_type = self.context.void_type();
            let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
            let fn_type = void_type.fn_type(&[ptr_type.into()], false);

            self.module.add_function(name, fn_type, None)
        }

        /// Translate MIR instruction to LLVM
        fn translate_instruction(
            &mut self,
            instr: &Instruction,
            mir_func: &MirFunction,
        ) -> Result<(), CodegenError> {
            match instr {
                Instruction::Assign { target, value } => {
                    let llvm_value = self.value_to_llvm(value)?;

                    // Store the value
                    let (var_ptr, _) = self
                        .variables
                        .get(target.as_str())
                        .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;

                    self.builder
                        .build_store(*var_ptr, llvm_value)
                        .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;

                    Ok(())
                }

                Instruction::BinaryOp {
                    target,
                    op,
                    left,
                    right,
                } => {
                    let left_val = self.value_to_llvm(left)?;
                    let right_val = self.value_to_llvm(right)?;

                    let result: BasicValueEnum = match op {
                        loreal_ast::BinOp::Add => {
                            if left_val.is_int_value() {
                                self.builder
                                    .build_int_add(
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "add",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!(
                                            "Add operation failed: {}",
                                            e
                                        ))
                                    })?
                                    .into()
                            } else {
                                self.builder
                                    .build_float_add(
                                        left_val.into_float_value(),
                                        right_val.into_float_value(),
                                        "fadd",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!(
                                            "FAdd operation failed: {}",
                                            e
                                        ))
                                    })?
                                    .into()
                            }
                        }
                        loreal_ast::BinOp::Sub => {
                            if left_val.is_int_value() {
                                self.builder
                                    .build_int_sub(
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "sub",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!(
                                            "Sub operation failed: {}",
                                            e
                                        ))
                                    })?
                                    .into()
                            } else {
                                self.builder
                                    .build_float_sub(
                                        left_val.into_float_value(),
                                        right_val.into_float_value(),
                                        "fsub",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!(
                                            "FSub operation failed: {}",
                                            e
                                        ))
                                    })?
                                    .into()
                            }
                        }
                        loreal_ast::BinOp::Mul => {
                            if left_val.is_int_value() {
                                self.builder
                                    .build_int_mul(
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "mul",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!(
                                            "Mul operation failed: {}",
                                            e
                                        ))
                                    })?
                                    .into()
                            } else {
                                self.builder
                                    .build_float_mul(
                                        left_val.into_float_value(),
                                        right_val.into_float_value(),
                                        "fmul",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("FMul failed: {}", e))
                                    })?
                                    .into()
                            }
                        }
                        loreal_ast::BinOp::Eq => {
                            if left_val.is_int_value() {
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::EQ,
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "eq",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Eq failed: {}", e))
                                    })?
                                    .into()
                            } else {
                                // For pointers (strings), compare addresses
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::EQ,
                                        left_val.into_pointer_value(),
                                        right_val.into_pointer_value(),
                                        "ptreq",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Ptr Eq failed: {}", e))
                                    })?
                                    .into()
                            }
                        }
                        loreal_ast::BinOp::Ne => {
                            if left_val.is_int_value() {
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::NE,
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "ne",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Ne failed: {}", e))
                                    })?
                                    .into()
                            } else {
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::NE,
                                        left_val.into_pointer_value(),
                                        right_val.into_pointer_value(),
                                        "ptrne",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Ptr Ne failed: {}", e))
                                    })?
                                    .into()
                            }
                        }
                        loreal_ast::BinOp::Lt => {
                            if left_val.is_int_value() {
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::SLT,
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "lt",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Lt failed: {}", e))
                                    })?
                                    .into()
                            } else {
                                self.builder
                                    .build_float_compare(
                                        inkwell::FloatPredicate::OLT,
                                        left_val.into_float_value(),
                                        right_val.into_float_value(),
                                        "flt",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Float Lt failed: {}", e))
                                    })?
                                    .into()
                            }
                        }
                        loreal_ast::BinOp::Le => {
                            if left_val.is_int_value() {
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::SLE,
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "le",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Le failed: {}", e))
                                    })?
                                    .into()
                            } else {
                                self.builder
                                    .build_float_compare(
                                        inkwell::FloatPredicate::OLE,
                                        left_val.into_float_value(),
                                        right_val.into_float_value(),
                                        "fle",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Float Le failed: {}", e))
                                    })?
                                    .into()
                            }
                        }
                        loreal_ast::BinOp::Gt => {
                            if left_val.is_int_value() {
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::SGT,
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "gt",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Gt failed: {}", e))
                                    })?
                                    .into()
                            } else {
                                self.builder
                                    .build_float_compare(
                                        inkwell::FloatPredicate::OGT,
                                        left_val.into_float_value(),
                                        right_val.into_float_value(),
                                        "fgt",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Float Gt failed: {}", e))
                                    })?
                                    .into()
                            }
                        }
                        loreal_ast::BinOp::Ge => {
                            if left_val.is_int_value() {
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::SGE,
                                        left_val.into_int_value(),
                                        right_val.into_int_value(),
                                        "ge",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Ge failed: {}", e))
                                    })?
                                    .into()
                            } else {
                                self.builder
                                    .build_float_compare(
                                        inkwell::FloatPredicate::OGE,
                                        left_val.into_float_value(),
                                        right_val.into_float_value(),
                                        "fge",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Float Ge failed: {}", e))
                                    })?
                                    .into()
                            }
                        }
                        _ => {
                            return Err(CodegenError::UnsupportedFeature(format!(
                                "Binary op {:?} not yet implemented",
                                op
                            )))
                        }
                    };

                    let (var_ptr, var_ty) = self
                        .variables
                        .get(target.as_str())
                        .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;

                    let val_to_store = if result.is_int_value()
                        && result.into_int_value().get_type().get_bit_width() == 1
                        && var_ty.is_int_type()
                        && var_ty.into_int_type().get_bit_width() == 64
                    {
                        self.builder
                            .build_int_z_extend(
                                result.into_int_value(),
                                var_ty.into_int_type(),
                                "bool_extend",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("Zext failed: {}", e)))?
                            .into()
                    } else {
                        result
                    };

                    self.builder
                        .build_store(*var_ptr, val_to_store)
                        .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;
                    Ok(())
                }

                Instruction::UnaryOp {
                    target,
                    op,
                    operand,
                } => {
                    let val = self.value_to_llvm(operand)?;

                    let result: BasicValueEnum = match op {
                        loreal_ast::UnOp::Neg => {
                            if val.is_int_value() {
                                self.builder
                                    .build_int_neg(val.into_int_value(), "neg")
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Neg failed: {}", e))
                                    })?
                                    .into()
                            } else {
                                self.builder
                                    .build_float_neg(val.into_float_value(), "fneg")
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("FNeg failed: {}", e))
                                    })?
                                    .into()
                            }
                        }
                        loreal_ast::UnOp::Not => {
                            // Boolean not
                            self.builder
                                .build_not(val.into_int_value(), "not")
                                .map_err(|e| CodegenError::LLVMError(format!("Not failed: {}", e)))?
                                .into()
                        }
                    };

                    let (var_ptr, var_ty) = self
                        .variables
                        .get(target.as_str())
                        .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;

                    let val_to_store = if result.is_int_value()
                        && result.into_int_value().get_type().get_bit_width() == 1
                        && var_ty.is_int_type()
                        && var_ty.into_int_type().get_bit_width() == 64
                    {
                        self.builder
                            .build_int_z_extend(
                                result.into_int_value(),
                                var_ty.into_int_type(),
                                "bool_extend",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("Zext failed: {}", e)))?
                            .into()
                    } else {
                        result
                    };

                    self.builder
                        .build_store(*var_ptr, val_to_store)
                        .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;
                    Ok(())
                }

                Instruction::Call { target, func, args } => {
                    // Get the function from module
                    let callee = self
                        .module
                        .get_function(func.as_str())
                        .ok_or_else(|| CodegenError::UndefinedFunction(func.to_string()))?;

                    // Convert arguments
                    let arg_values: Vec<BasicMetadataValueEnum> = args
                        .iter()
                        .map(|arg| self.value_to_llvm(arg).map(|v| v.into()))
                        .collect::<Result<Vec<_>, _>>()?;

                    // Build call
                    let call_site = self
                        .builder
                        .build_call(callee, &arg_values, "call")
                        .map_err(|e| CodegenError::LLVMError(format!("Call failed: {}", e)))?;

                    // Store result if there's a target variable
                    if let Some(target_name) = target {
                        if let ValueKind::Basic(val) = call_site.try_as_basic_value() {
                            let (vp, _) =
                                self.variables.get(target_name.as_str()).ok_or_else(|| {
                                    CodegenError::UndefinedVariable(target_name.to_string())
                                })?;

                            self.builder.build_store(*vp, val).map_err(|e| {
                                CodegenError::LLVMError(format!("Store failed: {}", e))
                            })?;
                        }
                    }

                    Ok(())
                }

                // RC operations - declare external runtime functions
                Instruction::Inc { var } => {
                    let val = self.value_to_llvm(&Value::Var(var.clone()))?;
                    if val.is_pointer_value() {
                        let inc_fn = self.get_runtime_fn("loreal_inc");
                        self.builder
                            .build_call(inc_fn, &[val.into()], "inc_call")
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("Inc call failed: {}", e))
                            })?;
                    }
                    Ok(())
                }

                Instruction::Dec { var } => {
                    let val = self.value_to_llvm(&Value::Var(var.clone()))?;
                    if val.is_pointer_value() {
                        let dec_fn = self.get_runtime_fn("loreal_dec");
                        self.builder
                            .build_call(dec_fn, &[val.into()], "dec_call")
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("Dec call failed: {}", e))
                            })?;
                    }
                    Ok(())
                }

                Instruction::Drop { var } => {
                    // Drop is same as Dec - decrement RC and free if zero
                    let val = self.value_to_llvm(&Value::Var(var.clone()))?;
                    if val.is_pointer_value() {
                        let dec_fn = self.get_runtime_fn("loreal_dec");
                        self.builder
                            .build_call(dec_fn, &[val.into()], "drop_call")
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("Drop call failed: {}", e))
                            })?;
                    }
                    Ok(())
                }

                Instruction::Alloc { target, ty } => {
                    // For structs, we use struct metadata. For tuples, we just check types.
                    if let Type::Named { name, .. } = ty {
                        let (struct_ty, _) =
                            self.struct_metadata.get(name.as_str()).ok_or_else(|| {
                                CodegenError::UnsupportedFeature(format!(
                                    "Unknown struct: {}",
                                    name
                                ))
                            })?;

                        // Size calculation: loreal_alloc expects PAYLOAD SIZE (it adds +24 bytes internally? Or we pass Total Size?)
                        // "loreal_alloc(size) returns Header + size" based on comments.
                        // Our LLVM struct includes Header (16 bytes). So we need total_size - 16.

                        let header_size = self.context.i64_type().const_int(24, false);
                        let total_size = struct_ty
                            .size_of()
                            .ok_or(CodegenError::LLVMError("Failed to get struct size".into()))?;

                        let fields_size = self
                            .builder
                            .build_int_sub(total_size, header_size, "fields_size")
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("Size sub failed: {}", e))
                            })?;

                        let loreal_alloc = self.get_loreal_alloc();
                        let call = self
                            .builder
                            .build_call(loreal_alloc, &[fields_size.into()], "alloc")
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("Alloc call failed: {}", e))
                            })?;

                        let ptr = match call.try_as_basic_value() {
                            ValueKind::Basic(v) => v,
                            _ => {
                                return Err(CodegenError::LLVMError(
                                    "Alloc returned nothing".into(),
                                ))
                            }
                        };

                        // Cast i8* to struct pointer
                        let struct_ptr = self
                            .builder
                            .build_pointer_cast(
                                ptr.into_pointer_value(),
                                self.context.ptr_type(inkwell::AddressSpace::default()),
                                "struct_ptr",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("Cast failed: {}", e)))?;

                        let (target_ptr, _) = self
                            .variables
                            .get(target.as_str())
                            .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;

                        self.builder
                            .build_store(*target_ptr, struct_ptr)
                            .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;
                    } else if let Type::Tuple { types, .. } = ty {
                        // Tuple allocation
                        // Size = element_count * 8
                        // We assume all elements are 64-bit/pointers for now (uniform representation).
                        // Note: If we had unboxed ints, size would be different. But we agreed on uniform rep.
                        // Or we use `types` to calculate size. `type_to_llvm` returns pointer/int64 which are 8 bytes.
                        let payload_size = types.len() * 8;
                        let size_val = self
                            .context
                            .i64_type()
                            .const_int(payload_size as u64, false);

                        let loreal_alloc = self.get_loreal_alloc();
                        let call = self
                            .builder
                            .build_call(loreal_alloc, &[size_val.into()], "alloc_tuple")
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("Alloc tuple call failed: {}", e))
                            })?;

                        let ptr = match call.try_as_basic_value() {
                            ValueKind::Basic(v) => v,
                            _ => {
                                return Err(CodegenError::LLVMError(
                                    "Alloc returned nothing".into(),
                                ))
                            }
                        };

                        // Store to target
                        let (target_ptr, _) = self
                            .variables
                            .get(target.as_str())
                            .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;

                        // We store the i8* (opaque pointer) directly or cast if target expects generic pointer?
                        // Tuple type is treating as opaque pointer in type_to_llvm (i8*).
                        // So `ptr` (which is i8*) is correct.
                        let ptr_val = ptr.into_pointer_value();
                        self.builder
                            .build_store(*target_ptr, ptr_val)
                            .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;

                        // Set type tags in RcObject header
                        // Header layout: [refcount:8][size:8][object_type:1][type_data:1][padding:6]
                        // object_type is at offset 16, type_data at offset 17
                        let type_tag_offset = self.context.i32_type().const_int(16, false);
                        let type_tag_ptr = unsafe {
                            self.builder
                                .build_gep(
                                    self.context.i8_type(),
                                    ptr_val,
                                    &[type_tag_offset.into()],
                                    "type_tag_ptr",
                                )
                                .map_err(|e| {
                                    CodegenError::LLVMError(format!("GEP failed: {}", e))
                                })?
                        };

                        // TYPE_TUPLE = 1
                        let type_tuple = self.context.i8_type().const_int(1, false);
                        self.builder
                            .build_store(type_tag_ptr, type_tuple)
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("Store type failed: {}", e))
                            })?;

                        // Store element count in type_data (offset 17)
                        let type_data_offset = self.context.i32_type().const_int(17, false);
                        let type_data_ptr = unsafe {
                            self.builder
                                .build_gep(
                                    self.context.i8_type(),
                                    ptr_val,
                                    &[type_data_offset.into()],
                                    "type_data_ptr",
                                )
                                .map_err(|e| {
                                    CodegenError::LLVMError(format!("GEP failed: {}", e))
                                })?
                        };

                        let elem_count =
                            self.context.i8_type().const_int(types.len() as u64, false);
                        self.builder
                            .build_store(type_data_ptr, elem_count)
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("Store type_data failed: {}", e))
                            })?;
                    } else {
                        return Err(CodegenError::UnsupportedFeature(format!(
                            "Alloc not supported for type {:?}",
                            ty
                        )));
                    }

                    Ok(())
                }

                Instruction::FieldStore {
                    target,
                    field,
                    value,
                } => {
                    let mir_ty = mir_func
                        .local_types
                        .get(target)
                        .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;

                    if let Type::Named { name, .. } = mir_ty {
                        let struct_name = name.as_str();
                        let (struct_ty, fields) =
                            self.struct_metadata.get(struct_name).ok_or_else(|| {
                                CodegenError::UnsupportedFeature(format!(
                                    "Undefined struct: {}",
                                    struct_name
                                ))
                            })?;

                        let field_idx =
                            fields.iter().position(|f| f == field).ok_or_else(|| {
                                CodegenError::UnsupportedFeature(format!(
                                    "Field {} not found in struct {}",
                                    field, struct_name
                                ))
                            })?;

                        let (var_ptr, var_ty) = self
                            .variables
                            .get(target.as_str())
                            .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;

                        let struct_ptr = self
                            .builder
                            .build_load(*var_ty, *var_ptr, "obj_ptr")
                            .map_err(|e| CodegenError::LLVMError(format!("Load failed: {}", e)))?
                            .into_pointer_value();

                        // Note: offset+1 because first field is header. Wait, in Structs,
                        // build_struct_gep uses index relative to the defined LLVM struct.
                        // My define_structs logic (lines 114) creates struct { RcHeader, f1, f2... }?
                        // If so, Header is index 0. So fields start at 1.
                        // Let's assume (field_idx + 1) is correct for now (as in original code).

                        let field_ptr = self
                            .builder
                            .build_struct_gep(
                                *struct_ty,
                                struct_ptr,
                                (field_idx + 1) as u32,
                                &format!("field_{}", field),
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("GEP failed: {}", e)))?;

                        let val = self.value_to_llvm(value)?;
                        self.builder.build_store(field_ptr, val).map_err(|e| {
                            CodegenError::LLVMError(format!("Field store failed: {}", e))
                        })?;
                    } else if let Type::Tuple { types: _, .. } = mir_ty {
                        let field_idx = field.parse::<usize>().map_err(|_| {
                            CodegenError::LLVMError(format!("Invalid tuple field: {}", field))
                        })?;

                        // Tuples are stored as [header (16 bytes) | data...]
                        // Elements are pointers (8 bytes).
                        // Offset = 16 + idx * 8
                        let offset = 24 + field_idx * 8;

                        let (var_ptr, var_ty) = self
                            .variables
                            .get(target.as_str())
                            .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;

                        let tuple_ptr = self
                            .builder
                            .build_load(*var_ty, *var_ptr, "tuple_ptr")
                            .map_err(|e| CodegenError::LLVMError(format!("Load failed: {}", e)))?
                            .into_pointer_value();

                        let field_ptr_raw = unsafe {
                            self.builder
                                .build_gep(
                                    self.context.i8_type(),
                                    tuple_ptr,
                                    &[self.context.i64_type().const_int(offset as u64, false)],
                                    "field_ptr_raw",
                                )
                                .map_err(|e| {
                                    CodegenError::LLVMError(format!("GEP failed: {}", e))
                                })?
                        };

                        // Cast to element pointer type (using generic ptr for opaque types)
                        let field_ptr = self
                            .builder
                            .build_pointer_cast(
                                field_ptr_raw,
                                self.context.ptr_type(inkwell::AddressSpace::default()),
                                "field_ptr",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("Cast failed: {}", e)))?;

                        let val = self.value_to_llvm(value)?;
                        self.builder
                            .build_store(field_ptr, val)
                            .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;
                    } else {
                        return Err(CodegenError::LLVMError(format!(
                            "FieldStore on non-struct/tuple type: {:?}",
                            mir_ty
                        )));
                    }

                    Ok(())
                }

                Instruction::FieldLoad {
                    target,
                    object,
                    field,
                } => {
                    let obj_src = match object {
                        Value::Var(name) => name,
                        _ => {
                            return Err(CodegenError::LLVMError(
                                "FieldLoad object must be a variable".into(),
                            ))
                        }
                    };

                    let mir_ty = mir_func
                        .local_types
                        .get(obj_src)
                        .ok_or_else(|| CodegenError::UndefinedVariable(obj_src.to_string()))?;

                    if let Type::Named { name, .. } = mir_ty {
                        let struct_name = name.as_str();
                        let (struct_ty, fields) =
                            self.struct_metadata.get(struct_name).ok_or_else(|| {
                                CodegenError::UnsupportedFeature(format!(
                                    "Undefined struct: {}",
                                    struct_name
                                ))
                            })?;

                        let field_idx =
                            fields.iter().position(|f| f == field).ok_or_else(|| {
                                CodegenError::UnsupportedFeature(format!(
                                    "Field {} not found in struct {}",
                                    field, struct_name
                                ))
                            })?;

                        let (var_ptr, var_ty) = self
                            .variables
                            .get(obj_src.as_str())
                            .ok_or_else(|| CodegenError::UndefinedVariable(obj_src.to_string()))?;

                        let struct_ptr = self
                            .builder
                            .build_load(*var_ty, *var_ptr, "obj_ptr")
                            .map_err(|e| CodegenError::LLVMError(format!("Load failed: {}", e)))?
                            .into_pointer_value();

                        let field_ptr = self
                            .builder
                            .build_struct_gep(
                                *struct_ty,
                                struct_ptr,
                                (field_idx + 1) as u32,
                                &format!("field_{}", field),
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("GEP failed: {}", e)))?;

                        let target_mir_ty = mir_func
                            .local_types
                            .get(target)
                            .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;
                        let target_llvm_ty = self.type_to_llvm(target_mir_ty)?;

                        let val = self
                            .builder
                            .build_load(target_llvm_ty, field_ptr, target.as_str())
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("Field load failed: {}", e))
                            })?;

                        // Store in target variable
                        let (target_ptr, _) = self
                            .variables
                            .get(target.as_str())
                            .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;
                        self.builder
                            .build_store(*target_ptr, val)
                            .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;
                    } else if let Type::Tuple { types: _, .. } = mir_ty {
                        let field_idx = field.parse::<usize>().map_err(|_| {
                            CodegenError::LLVMError(format!("Invalid tuple field: {}", field))
                        })?;

                        let offset = 24 + field_idx * 8;

                        let (var_ptr, var_ty) = self
                            .variables
                            .get(obj_src.as_str())
                            .ok_or_else(|| CodegenError::UndefinedVariable(obj_src.to_string()))?;

                        let tuple_ptr = self
                            .builder
                            .build_load(*var_ty, *var_ptr, "tuple_ptr")
                            .map_err(|e| CodegenError::LLVMError(format!("Load failed: {}", e)))?
                            .into_pointer_value();

                        let field_ptr_raw = unsafe {
                            self.builder
                                .build_gep(
                                    self.context.i8_type(),
                                    tuple_ptr,
                                    &[self.context.i64_type().const_int(offset as u64, false)],
                                    "field_ptr_raw",
                                )
                                .map_err(|e| {
                                    CodegenError::LLVMError(format!("GEP failed: {}", e))
                                })?
                        };

                        let target_mir_ty = mir_func
                            .local_types
                            .get(target)
                            .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;
                        let target_llvm_ty = self.type_to_llvm(target_mir_ty)?;

                        let field_ptr = self
                            .builder
                            .build_pointer_cast(
                                field_ptr_raw,
                                self.context.ptr_type(inkwell::AddressSpace::default()),
                                "field_ptr",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("Cast failed: {}", e)))?;

                        let val = self
                            .builder
                            .build_load(target_llvm_ty, field_ptr, target.as_str())
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("Field load failed: {}", e))
                            })?;

                        let (target_ptr, _) = self
                            .variables
                            .get(target.as_str())
                            .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;
                        self.builder
                            .build_store(*target_ptr, val)
                            .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;
                    } else {
                        return Err(CodegenError::LLVMError(format!(
                            "FieldLoad on non-struct/tuple type: {:?}",
                            mir_ty
                        )));
                    }

                    Ok(())
                }

                Instruction::AllocList {
                    target,
                    element_type,
                    length,
                } => {
                    let len_val = self.value_to_llvm(length)?.into_int_value();
                    let elem_llvm_ty = self.type_to_llvm(element_type)?;
                    let elem_size = elem_llvm_ty
                        .size_of()
                        .ok_or(CodegenError::LLVMError("Failed to get element size".into()))?;

                    // Total extra size = 8 (for length) + len * elem_size
                    // loreal_alloc(size) returns Header + size
                    let data_size = self
                        .builder
                        .build_int_mul(len_val, elem_size, "data_size")
                        .map_err(|e| CodegenError::LLVMError(format!("Mul failed: {}", e)))?;
                    let total_extra_size = self
                        .builder
                        .build_int_add(
                            data_size,
                            self.context.i64_type().const_int(8, false),
                            "total_extra_size",
                        )
                        .map_err(|e| CodegenError::LLVMError(format!("Add failed: {}", e)))?;

                    let loreal_alloc = self.get_loreal_alloc();
                    let call = self
                        .builder
                        .build_call(loreal_alloc, &[total_extra_size.into()], "alloc_list")
                        .map_err(|e| {
                            CodegenError::LLVMError(format!("AllocList call failed: {}", e))
                        })?;

                    let ptr = match call.try_as_basic_value() {
                        ValueKind::Basic(v) => v.into_pointer_value(),
                        _ => return Err(CodegenError::LLVMError("Alloc returned nothing".into())),
                    };

                    // Store length at offset 24 (after 24-byte RcHeader)
                    let len_ptr_raw = unsafe {
                        self.builder
                            .build_gep(
                                self.context.i8_type(),
                                ptr,
                                &[self.context.i64_type().const_int(24, false)],
                                "len_ptr_raw",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("GEP failed: {}", e)))?
                    };
                    let len_ptr = self
                        .builder
                        .build_pointer_cast(
                            len_ptr_raw,
                            self.context.ptr_type(inkwell::AddressSpace::default()),
                            "len_ptr",
                        )
                        .map_err(|e| CodegenError::LLVMError(format!("Cast failed: {}", e)))?;

                    self.builder.build_store(len_ptr, len_val).map_err(|e| {
                        CodegenError::LLVMError(format!("Store length failed: {}", e))
                    })?;

                    // Store the result pointer in the target variable
                    let (target_ptr, _) = self
                        .variables
                        .get(target.as_str())
                        .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;

                    self.builder
                        .build_store(*target_ptr, ptr)
                        .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;

                    // Set type tag to TYPE_LIST (2) at offset 16
                    let type_tag_offset = self.context.i32_type().const_int(16, false);
                    let type_tag_ptr = unsafe {
                        self.builder
                            .build_gep(
                                self.context.i8_type(),
                                ptr,
                                &[type_tag_offset.into()],
                                "type_tag_ptr",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("GEP failed: {}", e)))?
                    };

                    let type_list = self.context.i8_type().const_int(2, false);
                    self.builder
                        .build_store(type_tag_ptr, type_list)
                        .map_err(|e| {
                            CodegenError::LLVMError(format!("Store type failed: {}", e))
                        })?;

                    Ok(())
                }

                Instruction::IndexStore {
                    target,
                    index,
                    value,
                } => {
                    let obj_ptr = self
                        .value_to_llvm(&Value::Var(target.clone()))?
                        .into_pointer_value();
                    let idx_val = self.value_to_llvm(index)?.into_int_value();
                    let val_to_store = self.value_to_llvm(value)?;

                    // Get element type from mir_func.local_types
                    let mir_ty = mir_func
                        .local_types
                        .get(target)
                        .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;
                    let elem_mir_ty = if let Type::List { element_type, .. } = mir_ty {
                        element_type.as_ref()
                    } else {
                        return Err(CodegenError::UnsupportedFeature(
                            "IndexStore on non-list".into(),
                        ));
                    };
                    let elem_llvm_ty = self.type_to_llvm(elem_mir_ty)?;

                    // Calculate index pointer: ptr + 24 (Header + length) + index * sizeof(elem)
                    let base_offset = self.context.i64_type().const_int(24, false);
                    let element_ptr = unsafe {
                        let offset_in_bytes = self
                            .builder
                            .build_int_mul(idx_val, elem_llvm_ty.size_of().unwrap(), "offset_bytes")
                            .map_err(|e| CodegenError::LLVMError(format!("Mul failed: {}", e)))?;

                        let total_offset = self
                            .builder
                            .build_int_add(base_offset, offset_in_bytes, "total_offset")
                            .map_err(|e| CodegenError::LLVMError(format!("Add failed: {}", e)))?;

                        let byte_ptr = self
                            .builder
                            .build_gep(self.context.i8_type(), obj_ptr, &[total_offset], "byte_ptr")
                            .map_err(|e| CodegenError::LLVMError(format!("GEP failed: {}", e)))?;

                        self.builder
                            .build_pointer_cast(
                                byte_ptr,
                                self.context.ptr_type(inkwell::AddressSpace::default()),
                                "elem_ptr",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("Cast failed: {}", e)))?
                    };

                    self.builder
                        .build_store(element_ptr, val_to_store)
                        .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;
                    Ok(())
                }

                Instruction::IndexLoad {
                    target,
                    object,
                    index,
                } => {
                    let obj_ptr = self.value_to_llvm(object)?.into_pointer_value();
                    let idx_val = self.value_to_llvm(index)?.into_int_value();

                    // Get element type
                    let target_mir_ty = mir_func
                        .local_types
                        .get(target)
                        .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;
                    let target_llvm_ty = self.type_to_llvm(target_mir_ty)?;

                    // Calculate index pointer
                    let base_offset = self.context.i64_type().const_int(24, false);
                    let element_ptr = unsafe {
                        let offset_in_bytes = self
                            .builder
                            .build_int_mul(
                                idx_val,
                                target_llvm_ty.size_of().unwrap(),
                                "offset_bytes",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("Mul failed: {}", e)))?;

                        let total_offset = self
                            .builder
                            .build_int_add(base_offset, offset_in_bytes, "total_offset")
                            .map_err(|e| CodegenError::LLVMError(format!("Add failed: {}", e)))?;

                        let byte_ptr = self
                            .builder
                            .build_gep(self.context.i8_type(), obj_ptr, &[total_offset], "byte_ptr")
                            .map_err(|e| CodegenError::LLVMError(format!("GEP failed: {}", e)))?;

                        self.builder
                            .build_pointer_cast(
                                byte_ptr,
                                self.context.ptr_type(inkwell::AddressSpace::default()),
                                "elem_ptr",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("Cast failed: {}", e)))?
                    };

                    let val = self
                        .builder
                        .build_load(target_llvm_ty, element_ptr, target.as_str())
                        .map_err(|e| CodegenError::LLVMError(format!("Load failed: {}", e)))?;

                    let (target_ptr, _) = self
                        .variables
                        .get(target.as_str())
                        .ok_or_else(|| CodegenError::UndefinedVariable(target.to_string()))?;

                    self.builder
                        .build_store(*target_ptr, val)
                        .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;

                    Ok(())
                }

                // FBIP: Reuse instruction - conditional in-place mutation
                Instruction::Reuse { old, new } => {
                    // Get the old pointer
                    let old_val = self.value_to_llvm(&Value::Var(old.clone()))?;
                    if !old_val.is_pointer_value() {
                        return Ok(()); // Can only reuse pointers
                    }
                    let old_ptr = old_val.into_pointer_value();

                    // Get refcount from RC object header (at offset -8 bytes)
                    // Layout: [refcount:8][size:8][type_tag:8] then data
                    let rc_offset = self.context.i32_type().const_int((-8_i32) as u64, true);
                    let rc_ptr_i8 = unsafe {
                        self.builder
                            .build_gep(
                                self.context.i8_type(),
                                old_ptr,
                                &[rc_offset.into()],
                                "rc_ptr_i8",
                            )
                            .map_err(|e| CodegenError::LLVMError(format!("GEP RC failed: {}", e)))?
                    };

                    // Cast to i64* for loading refcount
                    let rc_ptr = self
                        .builder
                        .build_pointer_cast(
                            rc_ptr_i8,
                            self.context.ptr_type(inkwell::AddressSpace::default()),
                            "rc_ptr",
                        )
                        .map_err(|e| {
                            CodegenError::LLVMError(format!("Cast RC ptr failed: {}", e))
                        })?;

                    // Load refcount
                    let refcount = self
                        .builder
                        .build_load(self.context.i64_type(), rc_ptr, "refcount")
                        .map_err(|e| CodegenError::LLVMError(format!("Load RC failed: {}", e)))?
                        .into_int_value();

                    // Check if RC == 1 (unique ownership)
                    let is_unique = self
                        .builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            refcount,
                            self.context.i64_type().const_int(1, false),
                            "is_unique",
                        )
                        .map_err(|e| {
                            CodegenError::LLVMError(format!("RC compare failed: {}", e))
                        })?;

                    // Create basic blocks for conditional
                    let current_func = self
                        .current_function
                        .ok_or(CodegenError::LLVMError("No current function".into()))?;
                    let reuse_block = self.context.append_basic_block(current_func, "reuse_block");
                    let alloc_block = self.context.append_basic_block(current_func, "alloc_block");
                    let continue_block = self
                        .context
                        .append_basic_block(current_func, "continue_block");

                    // Branch based on RC
                    self.builder
                        .build_conditional_branch(is_unique, reuse_block, alloc_block)
                        .map_err(|e| {
                            CodegenError::LLVMError(format!("Cond branch failed: {}", e))
                        })?;

                    // Reuse block: RC == 1, reuse memory in-place
                    self.builder.position_at_end(reuse_block);
                    // Just pass through the old pointer (fields will be overwritten by FieldStore)
                    self.builder
                        .build_unconditional_branch(continue_block)
                        .map_err(|e| {
                            CodegenError::LLVMError(format!("Uncond branch failed: {}", e))
                        })?;

                    // Alloc block: RC > 1, allocate new memory
                    self.builder.position_at_end(alloc_block);

                    // Get size from old object's header (at offset -16 bytes)
                    let size_offset = self.context.i32_type().const_int((-16_i32) as u64, true);
                    let size_ptr_i8 = unsafe {
                        self.builder
                            .build_gep(
                                self.context.i8_type(),
                                old_ptr,
                                &[size_offset.into()],
                                "size_ptr_i8",
                            )
                            .map_err(|e| {
                                CodegenError::LLVMError(format!("GEP size failed: {}", e))
                            })?
                    };

                    let size_ptr = self
                        .builder
                        .build_pointer_cast(
                            size_ptr_i8,
                            self.context.ptr_type(inkwell::AddressSpace::default()),
                            "size_ptr",
                        )
                        .map_err(|e| {
                            CodegenError::LLVMError(format!("Cast size ptr failed: {}", e))
                        })?;

                    let size = self
                        .builder
                        .build_load(self.context.i64_type(), size_ptr, "size")
                        .map_err(|e| CodegenError::LLVMError(format!("Load size failed: {}", e)))?
                        .into_int_value();

                    // Call loreal_alloc with same size
                    let loreal_alloc = self.get_loreal_alloc();
                    let call = self
                        .builder
                        .build_call(loreal_alloc, &[size.into()], "reuse_alloc")
                        .map_err(|e| {
                            CodegenError::LLVMError(format!("Reuse alloc failed: {}", e))
                        })?;

                    let new_ptr = match call.try_as_basic_value() {
                        ValueKind::Basic(v) => v.into_pointer_value(),
                        _ => return Err(CodegenError::LLVMError("Alloc returned nothing".into())),
                    };

                    // Decrement old RC (we're done with it since we allocated new)
                    let dec_fn = self.get_runtime_fn("loreal_dec");
                    self.builder
                        .build_call(dec_fn, &[old_ptr.into()], "dec_old")
                        .map_err(|e| CodegenError::LLVMError(format!("Dec call failed: {}", e)))?;

                    self.builder
                        .build_unconditional_branch(continue_block)
                        .map_err(|e| {
                            CodegenError::LLVMError(format!("Uncond branch failed: {}", e))
                        })?;

                    // Continue block: phi to select pointer
                    self.builder.position_at_end(continue_block);
                    let phi = self
                        .builder
                        .build_phi(
                            self.context.ptr_type(inkwell::AddressSpace::default()),
                            "result_ptr",
                        )
                        .map_err(|e| CodegenError::LLVMError(format!("Phi failed: {}", e)))?;

                    phi.add_incoming(&[(&old_ptr, reuse_block), (&new_ptr, alloc_block)]);

                    // Store result in new variable
                    let (target_ptr, _) = self
                        .variables
                        .get(new.as_str())
                        .ok_or_else(|| CodegenError::UndefinedVariable(new.to_string()))?;

                    self.builder
                        .build_store(*target_ptr, phi.as_basic_value())
                        .map_err(|e| CodegenError::LLVMError(format!("Store failed: {}", e)))?;

                    Ok(())
                }

                _ => Err(CodegenError::UnsupportedFeature(format!(
                    "Instruction {:?} not yet implemented",
                    instr
                ))),
            }
        }
        /// Generate wrapper for main to initialize runtime
        pub fn generate_main_wrapper(&self) -> Result<(), CodegenError> {
            // Check if loreal_main exists
            if let Some(loreal_main) = self.module.get_function("loreal_main") {
                // Create C-compatible main: i32 main()
                let i32_type = self.context.i32_type();
                let fn_type = i32_type.fn_type(&[], false);
                let main_func = self.module.add_function("main", fn_type, None);

                let basic_block = self.context.append_basic_block(main_func, "entry");
                self.builder.position_at_end(basic_block);

                // Declare/Get loreal_init
                let void_type = self.context.void_type();
                let init_fn_type = void_type.fn_type(&[], false);
                let init_fn = self
                    .module
                    .get_function("loreal_init")
                    .unwrap_or_else(|| self.module.add_function("loreal_init", init_fn_type, None));

                // Call loreal_init
                self.builder
                    .build_call(init_fn, &[], "")
                    .map_err(|e| CodegenError::LLVMError(format!("Call init failed: {}", e)))?;

                // Call loreal_main
                let call_main = self
                    .builder
                    .build_call(loreal_main, &[], "res")
                    .map_err(|e| CodegenError::LLVMError(format!("Call main failed: {}", e)))?;

                // Process return value
                // loreal_main returns i64 (Int) or void.
                // We want to return (i32) res.
                let ret_val = call_main.try_as_basic_value();
                let final_ret: inkwell::values::BasicValueEnum = match ret_val {
                    inkwell::values::ValueKind::Basic(val) => {
                        if val.is_int_value() {
                            let int_val = val.into_int_value();
                            self.builder
                                .build_int_cast(int_val, i32_type, "ret_cast")
                                .map_err(|e| {
                                    CodegenError::LLVMError(format!("Cast failed: {}", e))
                                })?
                                .into()
                        } else {
                            i32_type.const_int(0, false).into()
                        }
                    }
                    _ => i32_type.const_int(0, false).into(),
                };

                // Declare/Get loreal_shutdown
                let shutdown_fn =
                    self.module
                        .get_function("loreal_shutdown")
                        .unwrap_or_else(|| {
                            self.module
                                .add_function("loreal_shutdown", init_fn_type, None)
                        });

                // Call loreal_shutdown
                self.builder
                    .build_call(shutdown_fn, &[], "")
                    .map_err(|e| CodegenError::LLVMError(format!("Call shutdown failed: {}", e)))?;

                // Return
                self.builder
                    .build_return(Some(&final_ret))
                    .map_err(|e| CodegenError::LLVMError(format!("Return failed: {}", e)))?;
            }
            Ok(())
        }
    }

    impl<'ctx> CodegenBackend for LLVMBackend<'ctx> {
        fn generate_function(&mut self, mir_func: &MirFunction) -> Result<(), CodegenError> {
            // Clear variables for each function to avoid pollution
            self.variables.clear();

            // Create LLVM function
            let function = self.generate_llvm_function(mir_func)?;
            self.current_function = Some(function);

            // Create LLVM basic blocks for all MIR blocks
            let mut block_map = std::collections::HashMap::new();

            for node_idx in mir_func.cfg.node_indices() {
                if let Some(mir_block) = mir_func.cfg.node_weight(node_idx) {
                    let llvm_block = self
                        .context
                        .append_basic_block(function, &format!("block_{}", mir_block.id));
                    block_map.insert(node_idx, llvm_block);
                }
            }

            // Pre-allocate all variables at function entry to avoid undefined variables
            // This is needed because variables can be used in blocks before they're assigned
            let entry_block = block_map[&mir_func.entry];
            self.builder.position_at_end(entry_block);

            // Collect all variable names from all instructions in all blocks
            let mut all_vars = std::collections::HashSet::new();
            for node_idx in mir_func.cfg.node_indices() {
                if let Some(mir_block) = mir_func.cfg.node_weight(node_idx) {
                    for instr in &mir_block.instructions {
                        match instr {
                            Instruction::Assign { target, .. }
                            | Instruction::BinaryOp { target, .. }
                            | Instruction::UnaryOp { target, .. }
                            | Instruction::Alloc { target, .. }
                            | Instruction::FieldLoad { target, .. }
                            | Instruction::AllocList { target, .. }
                            | Instruction::IndexLoad { target, .. } => {
                                all_vars.insert(target.clone());
                            }
                            Instruction::Call {
                                target: Some(target),
                                ..
                            } => {
                                all_vars.insert(target.clone());
                            }
                            _ => {}
                        }
                    }
                }
            }

            // Allocate stack space for all variables and parameters
            // For parameters, also store their initial values
            for (i, (param_name, param_ty)) in mir_func.params.iter().enumerate() {
                let llvm_ty = self.type_to_llvm(param_ty)?;
                let alloca = self
                    .builder
                    .build_alloca(llvm_ty, param_name.as_str())
                    .map_err(|e| CodegenError::LLVMError(format!("Param alloca failed: {}", e)))?;

                let param_val = function.get_nth_param(i as u32).unwrap();
                self.builder
                    .build_store(alloca, param_val)
                    .map_err(|e| CodegenError::LLVMError(format!("Param store failed: {}", e)))?;

                self.variables
                    .insert(param_name.to_string(), (alloca, llvm_ty));
            }

            for var_name in all_vars {
                // Skip if already in variables (e.g. parameter)
                if !self.variables.contains_key(var_name.as_str()) {
                    let mir_ty = mir_func
                        .local_types
                        .get(&var_name)
                        .ok_or_else(|| CodegenError::UndefinedVariable(var_name.to_string()))?;
                    let llvm_ty = self.type_to_llvm(mir_ty)?;
                    let alloca = self
                        .builder
                        .build_alloca(llvm_ty, var_name.as_str())
                        .map_err(|e| CodegenError::LLVMError(format!("Alloca failed: {}", e)))?;
                    self.variables
                        .insert(var_name.to_string(), (alloca, llvm_ty));
                }
            }

            // Process each MIR block
            for node_idx in mir_func.cfg.node_indices() {
                if let Some(mir_block) = mir_func.cfg.node_weight(node_idx) {
                    self.builder.position_at_end(block_map[&node_idx]);

                    for instr in &mir_block.instructions {
                        self.translate_instruction(instr, mir_func)?;
                    }

                    // Translate terminator
                    match &mir_block.terminator {
                        Terminator::Return(value) => {
                            let return_val = self.value_to_llvm(value)?;
                            self.builder.build_return(Some(&return_val)).map_err(|e| {
                                CodegenError::LLVMError(format!("Return failed: {}", e))
                            })?;
                        }
                        Terminator::Jump(target) => {
                            let target_block = block_map.get(target).ok_or_else(|| {
                                CodegenError::LLVMError("Invalid jump target".into())
                            })?;
                            self.builder
                                .build_unconditional_branch(*target_block)
                                .map_err(|e| {
                                    CodegenError::LLVMError(format!("Jump failed: {}", e))
                                })?;
                        }
                        Terminator::Branch {
                            condition,
                            then_block,
                            else_block,
                        } => {
                            let cond_val = self.value_to_llvm(condition)?;

                            // If condition is i64 (boolean stored in i64), convert back to i1
                            let bool_cond = if cond_val.is_int_value()
                                && cond_val.into_int_value().get_type().get_bit_width() == 64
                            {
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::NE,
                                        cond_val.into_int_value(),
                                        self.context.i64_type().const_int(0, false),
                                        "bool_check",
                                    )
                                    .map_err(|e| {
                                        CodegenError::LLVMError(format!("Bool check failed: {}", e))
                                    })?
                            } else {
                                cond_val.into_int_value()
                            };

                            let then_bb = block_map.get(then_block).ok_or_else(|| {
                                CodegenError::LLVMError("Invalid then block".into())
                            })?;
                            let else_bb = block_map.get(else_block).ok_or_else(|| {
                                CodegenError::LLVMError("Invalid else block".into())
                            })?;

                            self.builder
                                .build_conditional_branch(bool_cond, *then_bb, *else_bb)
                                .map_err(|e| {
                                    CodegenError::LLVMError(format!("Branch failed: {}", e))
                                })?;
                        }
                    }
                }
            }

            Ok(())
        }

        fn emit_object(&self, output_path: &str) -> Result<(), CodegenError> {
            Target::initialize_native(&inkwell::targets::InitializationConfig::default()).map_err(
                |e| CodegenError::LLVMError(format!("Failed to initialize native target: {}", e)),
            )?;

            let triple = TargetMachine::get_default_triple();
            let target = Target::from_triple(&triple).map_err(|e| {
                CodegenError::LLVMError(format!(
                    "Failed to get target from triple {}: {}",
                    triple, e
                ))
            })?;

            let cpu = "generic";
            let features = "";
            let target_machine = target
                .create_target_machine(
                    &triple,
                    cpu,
                    features,
                    inkwell::OptimizationLevel::Default,
                    RelocMode::Default,
                    CodeModel::Default,
                )
                .ok_or_else(|| CodegenError::LLVMError("Failed to create target machine".into()))?;

            target_machine
                .write_to_file(&self.module, FileType::Object, output_path.as_ref())
                .map_err(|e| {
                    CodegenError::LLVMError(format!("Failed to write object file: {}", e))
                })?;

            Ok(())
        }

        fn emit_ir(&self, output_path: &str) -> Result<(), CodegenError> {
            self.module
                .print_to_file(output_path)
                .map_err(|e| CodegenError::IOError(e.to_string()))
        }
    }
}

#[cfg(feature = "llvm-backend")]
pub use llvm_backend::LLVMBackend;

// ============================================================================
// Code Generator - High-level API
// ============================================================================

pub struct CodeGenerator {
    module_name: String,
    structs: Vec<loreal_ast::StructDef>,
}

impl CodeGenerator {
    pub fn new(module_name: &str, structs: Vec<loreal_ast::StructDef>) -> Self {
        Self {
            module_name: module_name.to_string(),
            structs,
        }
    }

    /// Generate code for MIR functions
    #[cfg(feature = "llvm-backend")]
    pub fn generate(&self, functions: &[MirFunction]) -> Result<(), CodegenError> {
        use inkwell::context::Context;

        let context = Context::create();
        let mut backend = LLVMBackend::new(&context, &self.module_name);

        backend.define_structs(&self.structs)?;

        for func in functions {
            backend.generate_function(func)?;
        }

        backend.generate_main_wrapper()?;

        Ok(())
    }

    /// Emit LLVM IR to file
    #[cfg(feature = "llvm-backend")]
    pub fn emit_llvm_ir(
        &self,
        functions: &[MirFunction],
        output_path: &str,
    ) -> Result<(), CodegenError> {
        use inkwell::context::Context;

        let context = Context::create();
        let mut backend = LLVMBackend::new(&context, &self.module_name);

        backend.define_structs(&self.structs)?;

        for func in functions {
            backend.generate_function(func)?;
        }

        backend.generate_main_wrapper()?;

        backend.emit_ir(output_path)
    }

    /// Emit native object file
    #[cfg(feature = "llvm-backend")]
    pub fn emit_obj(
        &self,
        functions: &[MirFunction],
        output_path: &str,
    ) -> Result<(), CodegenError> {
        use inkwell::context::Context;

        let context = Context::create();
        let mut backend = LLVMBackend::new(&context, &self.module_name);

        backend.define_structs(&self.structs)?;

        for func in functions {
            backend.generate_function(func)?;
        }

        backend.generate_main_wrapper()?;

        backend.emit_object(output_path)
    }
}

// ============================================================================
// Codegen Errors
// ============================================================================

#[derive(Debug)]
pub enum CodegenError {
    LLVMError(String),
    MLIRError(String),
    UnsupportedFeature(String),
    IOError(String),
    UndefinedVariable(String),
    UndefinedFunction(String),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::LLVMError(msg) => write!(f, "LLVM error: {}", msg),
            CodegenError::MLIRError(msg) => write!(f, "MLIR error: {}", msg),
            CodegenError::UnsupportedFeature(msg) => write!(f, "Unsupported feature: {}", msg),
            CodegenError::IOError(msg) => write!(f, "IO error: {}", msg),
            CodegenError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            CodegenError::UndefinedFunction(name) => write!(f, "Undefined function: {}", name),
        }
    }
}

impl std::error::Error for CodegenError {}
