use std::path::PathBuf;

use clap::{Parser, Subcommand};


#[derive(Parser)]
#[command(name = "loreal")]
#[command(about = "A purely functional language with compile-time reference counting", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build {
        input: PathBuf,

    },

    /// Compile and run a Loreal program
    Run {
        /// Input source file
        input: PathBuf,

        /// Arguments to pass to the program
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
}


fn main() {
    // Parse command-line arguments
    let cli = Cli::parse();
    match &cli.command {
        Commands::Build { input } => {
            println!("Building Loreal program from file: {:?}", input);
            // Here you would add the logic to build the Loreal program
        }

        Commands::Run { input, args } => {
            println!("Running Loreal program from file: {:?} with args: {:?}", input, args);
            // Here you would add the logic to run the Loreal program
        }
    }
}