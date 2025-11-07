use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, Result};

#[derive(Parser)]
#[command(name = "loreal")]
#[command(about = "A purely functional language with compile-time reference counting", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile a Loreal source file
    Build {
        input: PathBuf,

        #[arg(short, long)]
        output: Option<PathBuf>,

        #[arg(long)]
        emit_ast: bool,

        #[arg(long)]
        emit_mir: bool,

        #[arg(short = 'O', long, default_value = "0")]
        opt_level: u8,
    },

    Run {
        /// Input source file
        input: PathBuf,

        /// Arguments to pass to the program
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
}

fn main() -> Result<()> {
    // Parse command-line arguments
    let cli = Cli::parse();
    match cli.command {
        Commands::Build { input, output } => {
            println!("📦 Building: {}", input.display());

            // Phase 1: Read source file
            let source = std::fs::read_to_string(&input)
                .into_diagnostic()
                .map_err(|e| {
                    eprintln!("Failed to read source file: {}", e);
                    e
                })?;
            println!("✓ Loaded {} bytes", source.len());

            println!("🔤 Tokenizing...");
            let lexer = loreal_lexer::Lexer::new(&source);

            println!("🌳 Parsing...");
            let mut parser = loreal_parser::Parser::new(lexer);

            let module = match parser.parse() {
                Ok(module) => {
                    println!("✓ Parsed successfully");
                    module
                }
                Err(errors) => {
                    eprintln!("\n❌ Parse errors:");
                    for error in &errors {
                        eprintln!("  {}", error);
                    }
                    return Err(miette::miette!("{} parse error(s)", errors.len()));
                }
            };

            if emit_ast {
                println!("\n📄 Abstract Syntax Tree:");
                println!("{:#?}", module);
            }

            println!("🔍 Type checking...");
            let mut type_checker = loreal_semantic::TypeChecker::new();

            match type_checker.check_module(&module) {
                Ok(()) => {
                    println!("✓ Type checking passed");
                }
                Err(errors) => {
                    eprintln!("\n❌ Type errors:");
                    for error in &errors {
                        eprintln!("  {}", error);
                    }
                    return Err(miette::miette!("{} type error(s)", errors.len()));
                }
            }

            println!("🏗️  Lowering to MIR...");
            let mut mir_functions = Vec::new();

            for decl in &module.declarations {
                if let loreal_ast::Declaration::Function(func) = decl {
                    let mut local_types = std::collections::HashMap::new();
                    for ((f_name, v_name), ty) in &type_checker.variable_types {
                        if f_name == &func.name {
                            local_types.insert(v_name.clone(), ty.to_ast_type());
                        }
                    }

                    let mut mir_builder = loreal_mir::MirBuilder::with_context(
                        local_types,
                        type_checker.struct_types.clone(),
                    );

                    let params: Vec<(smol_str::SmolStr, loreal_ast::Type)> = func
                        .params
                        .iter()
                        .map(|(pattern, ty)| {
                            let name = if let loreal_ast::Pattern::Identifier { name, .. } = pattern
                            {
                                name.clone()
                            } else {
                                "param".into()
                            };
                            (name, ty.clone())
                        })
                        .collect();

                    let mir_func = mir_builder.lower_function(
                        func.name.clone(),
                        params,
                        func.return_type.clone(),
                        &func.body,
                    );

                    mir_functions.push(mir_func);
                }
            }

            println!("✓ Lowered {} function(s) to MIR", mir_functions.len());

            let executable = output.unwrap_or_else(|| "a.out".into());
            println!("\n✓ Compilation successful!");
            println!("  Output: {}", executable.display());

            Ok(())
        }
        Commands::Run { input, args } => todo!(),
    }
}
