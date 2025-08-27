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
            // Phase 1: Lexing
            println!("🔤 Tokenizing...");
            let lexer = loreal_lexer::Lexer::new(&source);
            Ok(())
        }
        Commands::Run { input, args } => todo!(),
    }
}
