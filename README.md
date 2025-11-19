# Loreal Compiler

A purely functional language with compile-time reference counting.

## Features

- Purely functional programming model
- Compile-time reference counting via Perceus algorithm
- Functional But In-Place (FBIP) optimization
- LLVM-based code generation

## Crates

- `loreal_ast` - Abstract Syntax Tree definitions
- `loreal_lexer` - Lexical analysis
- `loreal_parser` - Recursive descent parser
- `loreal_semantic` - Type checking and semantic analysis
- `loreal_mir` - Mid-Level IR with CFG and optimizations
- `loreal_codegen` - LLVM IR generation
- `loreal_runtime` - Runtime support library
- `loreal_cli` - Command-line interface

## Building

```bash
cargo build --release
```

## Usage

```bash
# Compile a Loreal file
loreal build input.lr -o output

# Run a Loreal program
loreal run input.lr
```

## License

MIT OR Apache-2.0
